-module(pine_pins_api).
-author("Chaitanya Chalasani <cchalasani@me.com>").

-behaviour(gen_server).

-include("pine_mnesia.hrl").

-import(pine_user, [validate/2]).
-import(pine_pins, [open_pin/2, close_pin/2, burn_pin/2, load_file/1,
                    list_pins/4, pin_details/1, retire_pin/1]).
-import(pine_tools, [ts_to_bin/1,
                     try_to_int/1,
                     try_to_hexbin/1,
                     try_to_iso8601/1,
                     hexbin_to_bin/1,
                     to/2]).

-export([start_link/0, open_pin_api/1, close_pin_api/1, burn_pin_api/1,
         list_pins_api/1, pin_details_api/1, retire_pin_api/1, load_file_api/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

open_pin_api(#{pin:=Pin,opened_by:=EndUser,
               http_token:=Cookie, http_source:=Source}) ->
  case validate(Cookie, Source) of
    {ok, _User} ->
      case open_pin(Pin, EndUser) of
        {ok, Seq, Value} ->
          {ok, [{seq, Seq}, {value, Value}]};
        {ok, Seq, Value, OpenedOn} ->
          OpenedDate = ts_to_bin(OpenedOn),
          {<<"250 AlreadyProcessed">>,
           [{seq, Seq}, {value, Value}, {opened_on, OpenedDate}]};
        Error ->
          Error
      end;
    AuthError ->
      AuthError
  end.

close_pin_api(#{seq:=Seq, opened_by:=EndUser,
                http_token:=Cookie, http_source:=Source}) ->
  case validate(Cookie, Source) of
    {ok, _User} ->
      close_pin(Seq, EndUser);
    AuthError ->
      AuthError
  end.

burn_pin_api(#{seq:=Seq, opened_by:=EndUser,
               http_token:=Cookie, http_source:=Source}) ->
  case validate(Cookie, Source) of
    {ok, _User} ->
      case burn_pin(Seq, EndUser) of
        {ok, Info} ->
          {ok, {info, Info}};
        AnythingElse ->
          AnythingElse
      end;
    AuthError ->
      AuthError
  end.

list_pins_api(#{id:=OrderId,
                status:=Status,
                page_no:=PageNo,
                page_size:=PageSize,
                http_token:=Cookie,
                http_source:=Source}) ->
  OrderIdBin = hexbin_to_bin(OrderId),
  PageNoInt = try_to_int(PageNo),
  PageSizeInt = try_to_int(PageSize),
  case validate(Cookie, Source) of
    {ok, _User} ->
      case list_pins(OrderIdBin, Status, PageNoInt, PageSizeInt) of
        {ok, Rows, TotalPages} ->
          PinsTupleList = mk_pinsTupleList(Rows),
          {ok, [{pins, PinsTupleList}, {total_pages, TotalPages}]};
        Other ->
          Other
      end;
    AuthError ->
      AuthError
  end.

pin_details_api(#{seq:=Seq,
                  http_token:=Cookie,
                  http_source:=Source}) ->
  case validate(Cookie, Source) of
    {ok, _User} ->
      case pin_details(Seq) of
        {ok, PinRecord} ->
          PinRecordTuple = mk_pinTuple(PinRecord),
          {ok, [{pin, PinRecordTuple}]};
        Other ->
          Other
      end;
    AuthError ->
      AuthError
  end.

retire_pin_api(#{seq:=Seq,
                 http_token:=Cookie,
                 http_source:=Source}) ->
  case validate(Cookie, Source) of
    {ok, _User} ->
      retire_pin(Seq);
    AuthError ->
      AuthError
  end.

load_file_api(#{filename:=Filename,
                http_token:=Cookie,
                http_source:=Source}) ->
  case validate(Cookie, Source) of
    {ok, _User} ->
      load_file(Filename);
    AuthError ->
      AuthError
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
                                   created_on = Now}),
  mnesia:dirty_write(#api_handlers{function = <<"pin.list">>,
                                   arguments = [id, status, page_no, page_size],
                                   handler = {?MODULE, list_pins_api},
                                   created_on = Now}),
  mnesia:dirty_write(#api_handlers{function = <<"pin.details">>,
                                   arguments = [seq],
                                   handler = {?MODULE, pin_details_api},
                                   created_on = Now}),
  mnesia:dirty_write(#api_handlers{function = <<"pin.retire">>,
                                   arguments = [seq],
                                   handler = {?MODULE, retire_pin_api},
                                   created_on = Now}),
  mnesia:dirty_write(#api_handlers{function = <<"pin.load">>,
                                   arguments = [filename],
                                   handler = {?MODULE, load_file_api},
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

mk_pinsTupleList(PinRecords) ->
  TupleListFunction = fun(PinRecord) ->
    {[{id, try_to_hexbin(PinRecord#pins.id)},
      {seq, to(binary, PinRecord#pins.seq)},
      {order, try_to_hexbin(PinRecord#pins.order)},
      {value, to(binary, PinRecord#pins.value)},
      {status, to(binary, PinRecord#pins.status)},
      {created_on, try_to_iso8601(PinRecord#pins.created_on)},
      {created_by, try_to_hexbin(PinRecord#pins.created_by)},
      {loaded_on, try_to_iso8601(PinRecord#pins.loaded_on)},
      {loaded_by, try_to_hexbin(PinRecord#pins.loaded_by)},
      {activated_on, try_to_iso8601(PinRecord#pins.activated_on)},
      {activated_by, try_to_hexbin(PinRecord#pins.activated_by)},
      {opened_on, try_to_iso8601(PinRecord#pins.opened_on)},
      {opened_by, try_to_hexbin(PinRecord#pins.opened_by)},
      {used_on, try_to_iso8601(PinRecord#pins.used_on)},
      {used_by, try_to_hexbin(PinRecord#pins.used_by)}]}
  end,
  lists:map(TupleListFunction, PinRecords).

mk_pinTuple(PinRecord) ->
    {[{id, try_to_hexbin(PinRecord#pins.id)},
      {seq, to(binary, PinRecord#pins.seq)},
      {order, try_to_hexbin(PinRecord#pins.order)},
      {value, to(binary, PinRecord#pins.value)},
      {status, to(binary, PinRecord#pins.status)},
      {created_on, try_to_iso8601(PinRecord#pins.created_on)},
      {created_by, try_to_hexbin(PinRecord#pins.created_by)},
      {loaded_on, try_to_iso8601(PinRecord#pins.loaded_on)},
      {loaded_by, try_to_hexbin(PinRecord#pins.loaded_by)},
      {activated_on, try_to_iso8601(PinRecord#pins.activated_on)},
      {activated_by, try_to_hexbin(PinRecord#pins.activated_by)},
      {opened_on, try_to_iso8601(PinRecord#pins.opened_on)},
      {opened_by, try_to_hexbin(PinRecord#pins.opened_by)},
      {used_on, try_to_iso8601(PinRecord#pins.used_on)},
      {used_by, try_to_hexbin(PinRecord#pins.used_by)}]}.

