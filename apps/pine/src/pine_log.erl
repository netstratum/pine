-module(pine_log).
-behaviour(gen_event).

-export([notify/2, notify/3]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3 ]).

notify(Id, Msg) ->
	notify(Id, Msg, false).

notify(Id, Msg, IsEnd) ->
	gen_event:notify(pine_gen_event, {Id, Msg, IsEnd}).

init([]) ->
	State = dict:new(),
	{ok, Path} = application:get_env(log_path),
	{ok, BaseName} = application:get_env(log_base_name),
	{ok, Extension} = application:get_env(extension),
	FileName = filename:join([Path, BaseName++Extension]),
	ok = filelib:ensure_dir(FileName),
	{ok, Fd} = file:open(FileName, [append]),
	{ok, {State, Fd}}.

handle_event({Id, Msg, IsEnd}, {State, Fd}) ->
	StateUpdate = handle_event_end(IsEnd, Id, Fd, 
		            dict:update(Id, fun(MsgList) -> [Msg|MsgList] end, [Msg], State)),
	{ok, {StateUpdate, Fd}}.

handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Arg, {_State, Fd}) ->
	file:close(Fd),
	ok. 

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

handle_event_end(true, Id, Fd, State) ->
	catch lists:foreach(fun(Msg) -> io:format(Fd, "~p~n", [Msg]) end, 
	        lists:reverse(dict:fetch(Id, State))),
	dict:erase(Id, State);
handle_event_end(_, _Id, _Fd, State) ->
	State.

