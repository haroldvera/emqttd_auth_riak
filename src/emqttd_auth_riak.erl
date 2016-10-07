%%--------------------------------------------------------------------
%% Copyright (c) 2015-2016 Harold Vera <harold@dox.cl>.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

%% @doc Authentication with MongoDB.
-module(emqttd_auth_riak).

-behaviour(emqttd_auth_mod).

-include("emqttd_auth_riak.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-export([init/1, check/3, description/0, hash/2]).


%-behaviour(ecpool_worker).

-export([config/1,
  replvar/2, query/3, check_superuser/3]).

-record(state, {superquery, authquery}).

-define(EMPTY(F), (F =:= undefined orelse F =:= <<>>)).

-compile([{parse_transform, lager_transform}]).

%%--------------------------------------------------------------------
%% Config
%%--------------------------------------------------------------------

config(superqueryriak) ->
  with_env(superqueryriak, fun(Config) ->
    #superquery{
      bucket = list_to_binary(get_value(bucket, Config, "user")),
      super_field      = list_to_binary(get_value(super_field, Config, "groups")),
      super_value      = list_to_binary(get_value(super_value, Config, "SPUTNIK_ADMINISTRATOR"))
    }
                       end);

config(authqueryriak) ->
  with_env(authqueryriak, fun(Config) ->
    #authquery{
      bucket = list_to_binary(get_value(bucket, Config, "user")),
      field      = list_to_binary(get_value(field, Config, "idx_username")),
      password_field      = list_to_binary(get_value(password_field, Config, "password")),
      clientid_field      = list_to_binary(get_value(clientid_field, Config, "clientid")),
      hash       = get_value(password_hash, Config, sha256)
    }
                      end);

config(aclquery) ->
  with_env(aclquery, fun(Config) ->
    #aclquery{
      bucket = list_to_binary(get_value(bucket, Config, "mqtt_acl"))
    }
                     end);

config(Key) ->
  with_env(Key, fun(Env) -> Env end).

with_env(Key, Fun) ->
  case gen_conf:value(?APP, Key) of
    {ok, Env}   -> Fun(Env);
    undefined -> undefined
  end.


%%--------------------------------------------------------------------
%% Auth Mod Callback
%%--------------------------------------------------------------------

init([SuperQuery, AuthQuery]) ->
  {ok, #state{superquery = SuperQuery, authquery = AuthQuery}}.

check(#mqtt_client{username = Username}, _Password, _State) when ?EMPTY(Username) ->
  {error, username_undefined};




check(#mqtt_client{username = Username}, Password, #state{superquery = SuperQuery,authquery = AuthQuery})
  when ?EMPTY(Password) ->
  #authquery{bucket = Bucket, field = Field} = AuthQuery,
  #superquery{super_field = SuperuserField, super_value = SuperuserID} = SuperQuery,
  User = query(Bucket, Field, Username),
  lager:notice("user no pass ~p", [User]),
  case check_superuser(User, SuperuserField, SuperuserID) of
    false -> {error, password_undefined};
    _ -> ok
  end ;



check(#mqtt_client{username = Username, client_id = ClientId}, Password, #state{superquery = SuperQuery,authquery = AuthQuery})
   ->
  #authquery{bucket = Bucket, field = Field, clientid_field = ClientIdField,
    password_field = PassworldField} = AuthQuery,
  #superquery{super_field = SuperuserField, super_value = SuperuserID} = SuperQuery,
  User = query(Bucket, Field, Username),
  lager:notice("user ~p", [User]),
  case check_superuser(User, SuperuserField, SuperuserID) of
    false ->  case check_pass(User, PassworldField, Password) of
                ok -> ok;
                _ -> check_clientid(User, ClientIdField, ClientId)
              end;
    true  -> ok
  end.



hash(Type, Password) ->
  emqttd_auth_mod:passwd_hash(Type, Password).

description() -> "Authentication with RiakDB".



replvar({Field, <<"%u">>}, #mqtt_client{username = Username}) ->
  {Field, Username};
replvar({Field, <<"%c">>}, #mqtt_client{client_id = ClientId}) ->
  {Field, ClientId};
replvar(Selector, _Client) ->
  Selector.

%%--------------------------------------------------------------------
%% Riak Connect/Query
%%--------------------------------------------------------------------




check_pass(undefined, _, _) ->
  {error, notfound};
check_pass(Obj, PasswordField, Password) ->
  case maps:get(PasswordField, Obj, undefined) of
    undefined -> {error, notfound};
    PassDb  -> case PassDb =:= Password of
                 true -> ok;
                 false -> {error, password_error}
               end
  end.

check_clientid(undefined, _, _) ->
  {error, notfound};
check_clientid(Obj, ClientIdField, ClientId) ->
  lager:info("obj ~p clientidfield ~p", [Obj, ClientIdField]),
  case maps:get(ClientIdField, Obj, undefined) of
    undefined -> {error, notfound};
    PassDb  -> case PassDb =:= ClientId of
                 true -> ok;
                 false -> {error, clientid_undefined}
               end
  end.

check_superuser(undefined, _, _) ->
  {error, password_undefined};
check_superuser(Obj, SuperuserField, SuperuserId) ->
  case maps:get(SuperuserField, Obj, undefined) of
    undefined -> false;
    Groups  -> case [Group || Group <- Groups, Group =:= SuperuserId] of
                 [] -> false;
                 _ -> true
               end
  end.

query(Bucket, Field, Username) ->
  lager:info("field ~p ~p ~p",[Bucket, Field, Username]),
  ecpool:with_client(
    ?APP,
    fun(Conn) ->
      case  riakc_pb_socket:get_index_eq(Conn, Bucket, {binary_index, binary_to_list(Field)}, Username) of
        {ok, {_, [],
          _, _}} ->
          undefined;
        {ok, {_, Ids,
          _, _}} ->
          get_db_value(Conn, Bucket, lists:nth(1, Ids))
      end

    end
  ).

get_db_value(Pid, Bucket, Id) ->
  lager:info("bucket ~p Id ~p",[Bucket, Id]),
  {ok, Obj} = riakc_pb_socket:get(Pid, Bucket, <<Id/binary>>),
  maps:from_list(jsx:decode(riakc_obj:get_value(Obj))).

get_value(Key, Config, Default) ->
  maps:get(Key, maps:from_list(Config), Default).
