PROJECT = kylie

DEPS = cowlib  gun shotgun jsx 
CONFIG = rel/sys.config

# Dependencies which need to be overriden to a specific version
dep_cowlib = hex 1.0.2
dep_gun    = hex 1.0.0-pre.1
dep_shotgun = git https://github.com/inaka/shotgun  0.2.3
dep_jsx = git https://github.com/talentdeficit/jsx.git 2.8.0


TEST_ERLC_OPTS += +debug_info
CT_OPTS += -cover test/cover.spec -vvv -erl_args -boot start_sasl -erl_args -config ${CONFIG}

SHELL_OPTS = -name ${PROJECT}@`hostname` -config ${CONFIG} -s ${PROJECT} -pa ebin deps/*/ebin 

include erlang.mk