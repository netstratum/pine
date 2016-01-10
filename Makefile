.PHONY: all clean compile release

rebar='./rebar3'

all: clean compile release
compile:
	@$(rebar) compile
clean:
	@$(rebar) clean
release:
	@$(rebar) release
