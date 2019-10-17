PROJECT = esmpp_lib

DEPS = lager
dep_lager = https://github.com/erlang-lager/lager.git 3.6.8

ERLC_OPTS = "+{parse_transform, lager_transform}" "+debug_info"

include erlang.mk
