-module(emqttd_auth_riak_SUITE).

-compile(export_all).

-define(PID, emqttd_auth_riak).

-include_lib("emqttd/include/emqttd.hrl").
-compile([{parse_transform, lager_transform}]).


all() -> 
    [{group, emqttd_auth_mysql}].

groups() -> 
    [{emqttd_auth_mysql, [sequence],
     [
      check_auth, check_acl]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    application:start(riakc),
    application:set_env(emqttd, conf, filename:join([DataDir, "emqttd.conf"])),
    application:ensure_all_started(emqttd),
    application:set_env(emqttd_auth_riak, conf, filename:join([DataDir, "emqttd_auth_riak.conf"])),
    application:set_env(lager, conf, filename:join([DataDir, "emqttd_auth_riak.conf"])),
    application:ensure_all_started(emqttd_auth_riak),
    Config.

end_per_suite(_Config) ->
    application:stop(emqttd_auth_riak),
    application:stop(ecpool),
    application:stop(mysql),
    application:stop(emqttd),
    application:stop(riakc),
    emqttd_mnesia:ensure_stopped().

check_acl(_) ->
    User1 = #mqtt_client{username = <<"user1">>},
    User2 = #mqtt_client{username = <<"user2">>},
    allow = emqttd_access_control:check_acl(User1, subscribe, <<"deliver/finished">>),
    allow = emqttd_access_control:check_acl(User2, publish, <<"deliver/finished">>),
    deny  = emqttd_access_control:check_acl(User1, subscribe, <<"$SYS/testuser/1">>),
    lager:info("erick test acl ~p",[emqttd_access_control:check_acl(User2, subscribe, <<"$SYS/testuser/1">>)]),
    allow  = emqttd_access_control:check_acl(User2, subscribe, <<"$SYS/testuser/1">>). %is superuser



check_auth(_) ->
    User1 = #mqtt_client{username = <<"user1">>},
    UserClientId1 = #mqtt_client{username = <<"user1">>, client_id = <<"fail">>},
    UserClientId2 = #mqtt_client{username = <<"user1">>, client_id = <<"edfrddd-dfffesdff">>},
    User2 = #mqtt_client{username = <<"user2">>},
    User3 = #mqtt_client{client_id = <<"user3">>},
    ok = emqttd_access_control:auth(User1, <<"123123">>),
    {error, _} = emqttd_access_control:auth(User1, <<"123">>),
    {error, password_undefined} = emqttd_access_control:auth(User1, <<>>),
    {error, password_error} = emqttd_access_control:auth(User1, <<"errorpwd">>),
    {error, clientid_undefined} = emqttd_access_control:auth(UserClientId1, <<>>),
    ok = emqttd_access_control:auth(UserClientId2, <<>>),
    ok = emqttd_access_control:auth(User2, <<"123123">>),
    ok = emqttd_access_control:auth(User2, <<>>),

    
    {error, _} = emqttd_access_control:auth(User3, <<"pwd">>).


