%%--------------------------------------------------------------------
%% Copyright (c) 2016 Feng Lee <feng@emqtt.io>.
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

-module(emqttd_auth_riak_app).

-behaviour(application).

-include("emqttd_auth_riak.hrl").

-import(emqttd_riak_client, [parse_query_riak/1]).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%%--------------------------------------------------------------------
%% Application Callbacks
%%--------------------------------------------------------------------

-compile([{parse_transform, lager_transform}]).

start(_StartType, _StartArgs) ->
    gen_conf:init(?APP),
    %gen_conf:init(?APP1),

    {ok, Sup} = emqttd_auth_riak_sup:start_link(),
    ok = register_auth_mod_riak(),
    ok = register_acl_mode_riak(),
    {ok, Sup}.

register_acl_mode_riak() ->
    SuperQueryRiak = emqttd_auth_riak:config(superqueryriak),
    AuthQueryRiak = emqttd_auth_riak:config(authqueryriak),
    AclEnv = {AuthQueryRiak, SuperQueryRiak},
    emqttd_access_control:register_mod(acl, emqttd_acl_riak, AclEnv).

register_auth_mod_riak() ->
    SuperQueryRiak = emqttd_auth_riak:config(superqueryriak),
    AuthQueryRiak = emqttd_auth_riak:config(authqueryriak),
    emqttd_access_control:register_mod(auth, emqttd_auth_riak, [SuperQueryRiak, AuthQueryRiak]).

prep_stop(State) ->
    emqttd_access_control:unregister_mod(auth, emqttd_auth_riak),
    with_acl_enabled(fun(_AclQuery) ->
        emqttd_access_control:unregister_mod(acl, emqttd_acl_riak)
    end),
    State.

stop(_State) ->
    ok.

with_acl_enabled(Fun) ->
    case gen_conf:value(?APP, aclquery) of
        {ok, AclQuery} -> Fun(AclQuery);
        undefined      -> ok
    end.

