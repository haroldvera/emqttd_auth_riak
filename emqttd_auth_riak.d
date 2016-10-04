src/emqttd_acl_riak.erl:: include/emqttd_auth_riak.hrl src/emqttd_auth_riak.erl; @touch $@
src/emqttd_auth_riak.erl:: include/emqttd_auth_riak.hrl; @touch $@
src/emqttd_auth_riak_app.erl:: include/emqttd_auth_riak.hrl src/emqttd_riak_client.erl; @touch $@
src/emqttd_auth_riak_sup.erl:: include/emqttd_auth_riak.hrl; @touch $@
src/emqttd_riak_client.erl:: include/emqttd_auth_riak.hrl; @touch $@

COMPILE_FIRST += emqttd_auth_riak emqttd_riak_client
