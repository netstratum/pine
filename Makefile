.PHONY: default all build clean compile release shell powershell tar

rebar='./rebar3'

default: compile
all: clean compile test release
build: test release
compile:
	@$(rebar) compile
clean:
	@$(rebar) clean
cleanall:
	@${rebar} clean -a
test:
	@$(rebar) do ct,cover
analyse:
	@$(rebar) dialyzer
release:
	@$(rebar) release
shell:
	@$(rebar) shell
powershell:
	@$(rebar) run
tar:
	@${rebar} as prod tar

