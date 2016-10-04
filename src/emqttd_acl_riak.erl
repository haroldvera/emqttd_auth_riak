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

%% @doc ACL with Riak.
-module(emqttd_acl_riak).

-behaviour(emqttd_acl_mod).

-include("emqttd_auth_riak.hrl").

-include_lib("emqttd/include/emqttd.hrl").

%% ACL callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

-record(state, {authquery, superquery}).
-import(emqttd_auth_riak, [check_superuser/3, query/3]).
-compile([{parse_transform, lager_transform}]).

init({AuthQuery, SuperQuery}) ->
  {ok, #state{authquery = AuthQuery, superquery = SuperQuery}}.

check_acl({#mqtt_client{username = <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
  ignore;

check_acl({#mqtt_client{username = Username} = Client, PubSub, Topic}, #state{
  authquery = AuthQuery,
  superquery = SuperQuery}
) ->
  #authquery{bucket = Bucket, field = Field} = AuthQuery,
  #superquery{super_field = SuperuserField, super_value = SuperuserID} = SuperQuery,
  User = query(Bucket, Field, Username),
  lager:notice("user ~p", [User]),
  case check_superuser(User, SuperuserField, SuperuserID) of
    false ->
      lager:info("client ~p, topic ~p, Database ~p",[Client, Topic, topics(PubSub, User)]),
      case match(Client, Topic, topics(PubSub, User)) of
        matched -> allow;
        nomatch -> deny
      end;
    true  -> allow
  end.

match(_Client, _Topic, []) ->
  nomatch;
match(Client, Topic, [TopicFilter|More]) ->
  case emqttd_topic:match(Topic, TopicFilter) of
    true  -> matched;
    false -> match(Client, Topic, More)
  end.

topics(publish, Row) ->
  lists:umerge(maps:get(<<"publish">>, Row, []), maps:get(<<"pubsub">>, Row, []));

topics(subscribe, Row) ->
  lists:umerge(maps:get(<<"subscribe">>, Row, []), maps:get(<<"pubsub">>, Row, [])).



reload_acl(_State) ->
  ok.

description() ->
  "ACL with RiakDB".

