.PHONY: default all clean compile release shell console

rebar='./rebar3'

default: compile
all: clean compile release
compile:
	@$(rebar) compile
clean:
	@$(rebar) clean
test:
	@$(rebar) ct
release:
	@$(rebar) release
shell:
	@$(rebar) shell
console:
	@$(rebar) run
tar:
	@${rebar} as prod tar
