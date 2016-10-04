
-define(APP, emqttd_auth_riak).


-record(superquery, {
  bucket = <<"user">>,
  super_field = <<"groups">>,
  super_value = <<"SPUTNIK_ADMINISTRATOR">>
}
).

-record(authquery, {
  bucket = <<"user">>,
  field      = <<"username">>,
  password_field = <<"password">>,
  clientid_field = <<"clientid">>,
  hash       = sha256
}
).

-record(aclquery, {bucket = <<"mqtt_acl">>}).