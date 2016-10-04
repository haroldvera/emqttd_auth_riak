%%--------------------------------------------------------------------
%% Copyright (c) 2012-2016 Feng Lee <feng@emqtt.io>.
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

-module(emqttd_auth_mysql_SUITE).

-compile(export_all).

-define(PID, emqttd_auth_mysql).

-include_lib("emqttd/include/emqttd.hrl").
-compile([{parse_transform, lager_transform}]).
%%setp1 init table
-define(DROP_ACL_TABLE, <<"DROP TABLE IF EXISTS mqtt_acl">>).

-define(CREATE_ACL_TABLE, <<"CREATE TABLE mqtt_acl ("
                            "   id int(11) unsigned NOT NULL AUTO_INCREMENT,"
                            "   allow int(1) DEFAULT NULL COMMENT '0: deny, 1: allow',"
                            "   ipaddr varchar(60) DEFAULT NULL COMMENT 'IpAddress',"
                            "   username varchar(100) DEFAULT NULL COMMENT 'Username',"
                            "   clientid varchar(100) DEFAULT NULL COMMENT 'ClientId',"
                            "   access int(2) NOT NULL COMMENT '1: subscribe, 2: publish, 3: pubsub',"
                            "   topic varchar(100) NOT NULL DEFAULT '' COMMENT 'Topic Filter',",
                            "   PRIMARY KEY (`id`)"
                            ") ENGINE=InnoDB DEFAULT CHARSET=utf8">>).

-define(INIT_ACL, <<"INSERT INTO mqtt_acl (id, allow, ipaddr, username, clientid, access, topic)"
                    "VALUES  (1,1,NULL,'$all',NULL,2,'#'),"
	                        "(2,0,NULL,'$all',NULL,1,'$SYS/#'),"
	                        "(3,0,NULL,'$all',NULL,1,'eq #'),"
	                        "(5,1,'127.0.0.1',NULL,NULL,2,'$SYS/#'),"
	                        "(6,1,'127.0.0.1',NULL,NULL,2,'#'),"
	                        "(7,1,NULL,'dashboard',NULL,1,'$SYS/#')">>).

-define(DROP_AUTH_TABLE, <<"DROP TABLE IF EXISTS `mqtt_user`">>).

-define(CREATE_AUTH_TABLE, <<"CREATE TABLE `mqtt_user` ("
                             "`id` int(11) unsigned NOT NULL AUTO_INCREMENT,"
                             "`username` varchar(100) DEFAULT NULL,"
                             "`password` varchar(100) DEFAULT NULL,"
                             "`salt` varchar(20) DEFAULT NULL,"
                             "`is_superuser` tinyint(1) DEFAULT 0,"
                             "`created` datetime DEFAULT NULL,"
                             "PRIMARY KEY (`id`),"
                             "UNIQUE KEY `mqtt_username` (`username`)"
                             ") ENGINE=MyISAM DEFAULT CHARSET=utf8">>).

-define(INIT_AUTH, <<"INSERT INTO mqtt_user (id, username, password, salt, is_superuser, created)"
                     "VALUES  (1, 'testuser1', 'pass1', 'plain', 0, now())," 
                             "(2, 'testuser2', 'pass2', 'plain', 1, now())">>).

all() -> 
    [{group, emqttd_auth_mysql}].

groups() -> 
    [{emqttd_auth_mysql, [sequence],
     [
      check_auth, check_acl]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    %application:start(lager),
    application:start(riakc),
    application:set_env(emqttd, conf, filename:join([DataDir, "emqttd.conf"])),
    application:ensure_all_started(emqttd),
    application:set_env(emqttd_auth_mysql, conf, filename:join([DataDir, "emqttd_auth_mysql.conf"])),
    application:set_env(emqttd_auth_riak, conf, filename:join([DataDir, "emqttd_auth_mysql.conf"])),
    application:set_env(lager, conf, filename:join([DataDir, "emqttd_auth_mysql.conf"])),
    application:ensure_all_started(emqttd_auth_mysql),
    Config.

end_per_suite(_Config) ->
    application:stop(emqttd_auth_mysql),
    application:stop(ecpool),
    application:stop(mysql),
    application:stop(emqttd),
    application:stop(riakc),
    emqttd_mnesia:ensure_stopped().

check_acl(_) ->
    User1 = #mqtt_client{client_id = <<"harold">>, username = <<"harold">>},
    User2 = #mqtt_client{client_id = <<"erick">>, username = <<"erick">>},
    allow = emqttd_access_control:check_acl(User1, subscribe, <<"deliver/finished">>),
    allow = emqttd_access_control:check_acl(User2, publish, <<"deliver/finished">>),
    deny  = emqttd_access_control:check_acl(User1, subscribe, <<"$SYS/testuser/1">>),
    lager:info("erick test acl ~p",[emqttd_access_control:check_acl(User2, subscribe, <<"$SYS/testuser/1">>)]),
    allow  = emqttd_access_control:check_acl(User2, subscribe, <<"$SYS/testuser/1">>). %is superuser




check_auth(_) ->
    User1 = #mqtt_client{client_id = <<"harold">>, username = <<"harold">>},

    User2 = #mqtt_client{client_id = <<"erick">>, username = <<"erick">>},
    
    User3 = #mqtt_client{client_id = <<"client3">>},
    ok = emqttd_access_control:auth(User1, <<"1231231">>),
    {error, _} = emqttd_access_control:auth(User1, <<"123">>),
    {error, password_undefined} = emqttd_access_control:auth(User1, <<>>),
    {error, password_error} = emqttd_access_control:auth(User1, <<"errorpwd">>),

    ok = emqttd_access_control:auth(User2, <<"123123">>),
    ok = emqttd_access_control:auth(User2, <<>>),

    
    {error, _} = emqttd_access_control:auth(User3, <<"pwd">>).


