PROJECT = kylie

DEPS = hackney jsx
CONFIG = rel/sys.config

# Dependencies which need to be overriden to a specific version

dep_hackney = git https://github.com/benoitc/hackney.git 1.6.1
dep_jsx = git https://github.com/talentdeficit/jsx.git 2.8.0

TEST_ERLC_OPTS += +debug_info
CT_OPTS += -cover test/cover.spec -vvv -erl_args -boot start_sasl -erl_args -config ${CONFIG}

SHELL_OPTS = -name ${PROJECT}@`hostname` -config ${CONFIG} -s ${PROJECT} -pa ebin deps/*/ebin 

include erlang.mk