-module(pine_user).
-behaviour(gen_server).

-include("pine_mnesia.hrl").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
        handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:call({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, ok}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
