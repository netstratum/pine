-module(pine_ui).
-author("Chaitanya Chalasani <cchalasani@me.com>").

-export([init/3, handle/2, terminate/3]).

init(Type, Req, Opts) ->
  pine_log:notify(10, ["init", Type, Req, Opts]),
  {ok, Req, no_state}.

handle(Req, State) ->
  pine_log:notify(10, ["handle", Req, State]),
  {ok, Req2} = cowboy_req:reply(200,
       [{<<"content-type">>, <<"text/plain">>}], <<"Under Construction">>, Req),
  pine_log:notify(10, ["handle result", Req2]),
  {ok, Req2, State}.

terminate(Reason, Req, State) ->
  pine_log:notify(10, ["terminate", Reason, Req, State], true),
  ok.
