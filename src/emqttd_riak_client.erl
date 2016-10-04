%%%-------------------------------------------------------------------
%%% @author harold
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. sep 2016 10:25 PM
%%%-------------------------------------------------------------------
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
