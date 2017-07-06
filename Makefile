PROJECT = esmpp_lib

DEPS = lager
dep_lager = https://github.com/basho/lager.git 3.2.4

ERLC_OPTS = "+{parse_transform, lager_transform}" "+debug_info"

include erlang.mk
