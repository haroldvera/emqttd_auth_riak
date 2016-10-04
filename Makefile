PROJECT = emqttd_auth_riak
PROJECT_DESCRIPTION = emqttd Authentication/ACL against MySQL
PROJECT_VERSION = 2.0

DEPS =  ecpool gen_conf emqttd riakc lager jsx

dep_ecpool   = git https://github.com/emqtt/ecpool master
dep_emqttd   = git https://github.com/emqtt/emqttd emq20
dep_gen_conf = git https://github.com/emqtt/gen_conf master

ERLC_OPTS += +'{parse_transform, lager_transform}'



COVER = true

PACKAGES += jsx
pkg_jsx_name = jsx
pkg_jsx_description = An Erlang application for consuming, producing and manipulating JSON.
pkg_jsx_homepage = https://github.com/talentdeficit/jsx
pkg_jsx_fetch = git
pkg_jsx_repo = https://github.com/talentdeficit/jsx
pkg_jsx_commit = master

PACKAGES += riakc
pkg_riakc_name = riakc
pkg_riakc_description = Erlang clients for Riak.
pkg_riakc_homepage = https://github.com/basho/riak-erlang-client
pkg_riakc_fetch = git
pkg_riakc_repo = https://github.com/basho/riak-erlang-client
pkg_riakc_commit = master

PACKAGES += lager
pkg_lager_name = lager
pkg_lager_description = A logging framework for Erlang/OTP.
pkg_lager_homepage = https://github.com/basho/lager
pkg_lager_fetch = git
pkg_lager_repo = https://github.com/basho/lager
pkg_lager_commit = master

include erlang.mk

app:: rebar.config

