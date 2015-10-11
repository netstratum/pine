-module(pine_pins_api).
-behaviour(gen_server).

-include("pine_mnesia.hrl").

-import(pine_pins, [open_pin/2, close_pin/2, burn_pin/2]).
-import(pine_tools, [ts_to_bin/1]).

-export([start_link/0, open_pin_api/1, close_pin_api/1, burn_pin_api/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

open_pin_api(#{pin:=Pin, opened_by:=EndUser}) ->
  case open_pin(Pin, EndUser) of
    {ok, Seq, Value} ->
      {ok, [{seq, Seq}, {value, Value}]};
    {ok, Seq, Value, OpenedOn} ->
      OpenedDate = ts_to_bin(OpenedOn),
      {<<"250 AlreadyProcessed">>,
       [{seq, Seq}, {value, Value}, {opened_on, OpenedDate}]};
    Error ->
      Error
  end.

close_pin_api(#{seq:=Seq, opened_by:=EndUser}) ->
  close_pin(Seq, EndUser).

burn_pin_api(#{seq:=Seq, opened_by:=EndUser}) ->
  case burn_pin(Seq, EndUser) of
    {ok, Info} ->
      {ok, {info, Info}};
    AnythingElse ->
      AnythingElse
  end.

init([]) ->
  init_api(),
  {ok, []}.

init_api() ->
  Now = os:timestamp(),
  mnesia:dirty_write(#api_handlers{function = <<"pin.open">>,
                                   arguments = [pin, opened_by],
                                   handler = {?MODULE, open_pin_api},
                                   created_on = Now}),
  mnesia:dirty_write(#api_handlers{function = <<"pin.close">>,
                                   arguments = [seq, opened_by],
                                   handler = {?MODULE, close_pin_api},
                                   created_on = Now}),
  mnesia:dirty_write(#api_handlers{function = <<"pin.burn">>,
                                   arguments = [seq, opened_by],
                                   handler = {?MODULE, burn_pin_api},
                                   created_on = Now}).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
