emqttd_auth_riak
=================

Authentication, ACL with Riak Database


Build Plugin
-------------

make && make tests

Configure Plugin
----------------

File: etc/emqttd_auth_riak.conf

```
{riak_pool, [
  {pool_size, 8},
  {auto_reconnect, 3},

  %% Riakdb Opts
  {host, "localhost"},
  {port, 8087}
]}.

{superqueryriak,  [
  {bucket, "user"},
  {super_field, "groups"},
  {super_value, "SPUTNIK_ADMINISTRATOR"}
]}.

%% Authentication Query
{authqueryriak,  [
  {bucket, "user"},
  {password_field, "password"},
  {clientid_field = "clientid"},
  %% Hash Algorithm: plain, md5, sha, sha256, pbkdf2?
  {password_hash, sha256}
]}.


%% If no ACL rules matched, return...
{acl_nomatch, allow}.


```

Load Plugin
-----------

./bin/emqttd_ctl plugins load emqttd_auth_riak


Load Test Database Riak Document
-----------

```

curl -X POST http://192.168.0.6:8098/riak/user/user1 -H "x-riak-index-idx_username_bin: user1" -d '
 {
   "username":"user1",
   "password":"123123",
   "groups" : ["SPUTNIK_EMAILER"],
   "clientid":"edfrddd-dfffesdff",
   "pubsub" : ["deliver/new","deliver/cancel"],
   "subscribe" : ["deliver/finished"]
 }
'

curl -X POST http://192.168.0.6:8098/riak/user/user2 -H "x-riak-index-idx_username_bin: user2" -d '
 {
   "username":"user2",
   "password":"123",
   "clientid":"ggfrddd-dfffesdgg",
   "groups" : ["SPUTNIK_ADMINISTRATOR","SPUTNIK_EMAILER"],
   "pubsub" : ["deliver/new","deliver/cancel"],
   "publish" : ["deliver/finished"]

 }
'

```

License
-------

Apache License Version 2.0

Author
-------

Harold Vera Zamora <harold@dox.cl>.

=======
