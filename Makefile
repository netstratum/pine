.PHONY: default all clean compile release shell console

rebar='./rebar3'

default: compile
all: clean compile release
compile:
	@$(rebar) compile
clean:
	@$(rebar) clean
release:
	@$(rebar) release
shell:
	@$(rebar) shell
console:
	@_build/default/rel/pine/bin/pine console
tar:
	@${rebar} as prod tar
