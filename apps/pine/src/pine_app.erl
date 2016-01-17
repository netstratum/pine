-module(pine_app).
-author("Chaitanya Chalasani <cchalasani@me.com>").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    pine_sup:start_link().

stop(_State) ->
    ok.
