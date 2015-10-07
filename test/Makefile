.PHONY: all clean compile deps console

all: deps compile
interact: clean compile console
compile:
	@rebar compile

deps:
	@rebar get-deps

clean:
	@rebar -r clean

console:
	@erl -pa ebin -pa deps/*/ebin -s test_console


