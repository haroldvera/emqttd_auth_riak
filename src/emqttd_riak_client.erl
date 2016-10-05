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
-module(emqttd_riak_client).
-author("harold").

%% API
-export([]).


-behaviour(ecpool_worker).

-include("emqttd_auth_riak.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-export([connect/1, parse_query_riak/1]).

-compile([{parse_transform, lager_transform}]).

connect(Opts) ->
  #{host := Host, port := Port} = maps:from_list(Opts),
  Result = riakc_pb_socket:start_link(Host, Port),
  lager:info("connect ~p",[Result]),
  Result.


parse_query_riak(Params) ->
  maps:from_list(Params).
