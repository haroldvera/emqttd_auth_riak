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

-module(emqttd_auth_mysql_sup).

-include("emqttd_auth_mysql.hrl").

-behaviour(supervisor).

-export([start_link/0]).
-compile([{parse_transform, lager_transform}]).
%% Supervisor callbacks
-export([init/1]).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

init([]) ->
    %% MySQL Connection Pool.
    %{ok, PoolEnv} = gen_conf:value(?APP, mysql_pool),
    %PoolSpec = ecpool:pool_spec(?APP, ?APP, emqttd_auth_mysql_client, PoolEnv),
    {ok, PoolEnvRiak} = gen_conf:value(?APP, riak_pool),
    PoolSpecRiak = ecpool:pool_spec(?APP1, ?APP1, emqttd_riak_client, PoolEnvRiak),
    lager:info("pool spec riak ~p", [PoolSpecRiak]),
    {ok, {{one_for_one, 10, 100}, [PoolSpecRiak]}}.

