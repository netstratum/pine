-module(test_console).
-export([start/0]).

start() ->
  application:start(jiffy),
  application:start(ibrowse),
  application:start(test).
