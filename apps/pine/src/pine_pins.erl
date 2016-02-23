-module(pine_pins).
-author("Chaitanya Chalasani <cchalasani@me.com>").

-behaviour(gen_server).

-include("pine_mnesia.hrl").

-import(pine_tools, [uuid/0, int_to_list_pad/3, to/2,
                     get_keysforpage/3]).
-import(pine_mnesia, [create_table/2]).

-export([generate/6, load_file/1, open_pin/2, close_pin/2, burn_pin/2,
         list_pins/4, pin_details/1, retire_pin/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

load_file(Filename) ->
  gen_server:call(?MODULE, {load_file, Filename}).

open_pin(Pin, EndUser) ->
  gen_server:call(?MODULE, {open_pin, Pin, EndUser}).

close_pin(Seq, EndUser) ->
  gen_server:call(?MODULE, {close_pin, Seq, EndUser}).

burn_pin(Seq, EndUser) ->
  gen_server:call(?MODULE, {burn_pin, Seq, EndUser}).

list_pins(OrderId, Status, PageNo, PageSize) ->
  gen_server:call(?MODULE, {list_pins, OrderId, Status, PageNo, PageSize}).

pin_details(Seq) ->
  gen_server:call(?MODULE, {pin_details, Seq}).

retire_pin(Seq) ->
  gen_server:call(?MODULE, {retire_pin, Seq}).

init([]) ->
  random:seed(os:timestamp()),
  init_tables(),
  {ok, ok}.

handle_call({load_file, Filename}, _From, State) ->
  Reply = handle_load_file(Filename),
  {reply, Reply, State};
handle_call({open_pin, Pin, EndUser}, _From, State) ->
  Reply = handle_open_pin(Pin, EndUser),
  {reply, Reply, State};
handle_call({close_pin, Seq, EndUser}, _From, State) ->
  Reply = handle_close_pin(Seq, EndUser),
  {reply, Reply, State};
handle_call({burn_pin, Seq, EndUser}, _From, State) ->
  Reply = handle_burn_pin(Seq, EndUser),
  {reply, Reply, State};
handle_call({list_pins, OrderId, Status, PageNo, PageSize}, _From, State) ->
  Reply = handle_list_pins(OrderId, Status, PageNo, PageSize),
  {reply, Reply, State};
handle_call({pin_details, Seq}, _From, State) ->
  Reply = handle_pin_details(Seq),
  {reply, Reply, State};
handle_call({retire_pin, Seq}, _From, State) ->
  Reply = handle_retire_pin(Seq),
  {reply, Reply, State};
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

init_tables() ->
  lists:map(
    fun({Table, Options}) -> create_table(Table, Options) end,
    [{pins, [{disc_copies, [node()]},{attributes, record_info(fields, pins)},
             {index, [seq, pin, order]}]},
     {usedpins, [{disc_copies, [node()]},{attributes, record_info(fields, usedpins)},
                 {index, [seq, pin, order]}]}]
    ),
  mnesia:wait_for_tables([pins, usedpins], 2500).

generate({PrinterCode, BrandCode, ExpiryDate, PrinterSeqNumber, SerialLength,
          CountryCode, RegionCode, OrderId},
          OrderName, FaceValue, Quantity, Length, Form) ->
  random:seed(os:timestamp()),
  case file:open(OrderName, [write]) of
    {ok, Fd} ->
      write_header(Fd, {PrinterCode, BrandCode, ExpiryDate, PrinterSeqNumber},
                   FaceValue, Quantity),
      write_pins(Fd, SerialLength, CountryCode, RegionCode, OrderId, Quantity, Length, Form),
      file:close(Fd);
    AnythingElse ->
      AnythingElse
  end.

write_header(Fd, {PrinterCode, BrandCode, ExpiryDate, PrinterSeqNumber},
             FaceValue, Quantity) ->
  io:format(Fd, "~s,~s,~s,~s,~p,~s~n", [FaceValue,
                                        PrinterCode,
                                        BrandCode,
                                        ExpiryDate,
                                        Quantity,
                                        PrinterSeqNumber]).

write_pins(Fd, SerialLength, CountryCode, RegionCode, OrderId, Quantity, Length, Form) ->
  write_pins(Fd, SerialLength, CountryCode, RegionCode, OrderId, Quantity, Length, Form, 0).

write_pins(_Fd, _SerialLength, _CountryCode,
           _RegionCode, _OrderId, Quantity, _Length, numeric, Quantity) ->
  ok;
write_pins(Fd, SerialLength, CountryCode, RegionCode, OrderId,
           Quantity, Length, numeric, SerialNumber) ->
  Pin = lists:foldl(fun(_X, N) -> N * 10 + random:uniform(10) -1 end,
                           random:uniform(9), lists:seq(1, Length-1)),
  Serial = CountryCode ++ RegionCode ++ OrderId ++
           int_to_list_pad(SerialNumber + 1, SerialLength, $0),
  io:format(Fd, "~p,~s~n", [Pin, Serial]),
  write_pins(Fd, SerialLength, CountryCode, RegionCode, OrderId,
             Quantity, Length, numeric, SerialNumber + 1).

handle_load_file(Filename) ->
  case file:open(Filename, [read]) of
    {error, Reason} ->
      io:format("Failed to open file ~p~n", [Reason]),
      {error, Reason};
    {ok, Fd} ->
      case read_file_header(Fd) of
        {error, ReasonRead} ->
          io:format("Failed to open file ~p~n", [ReasonRead]),
          {error, ReasonRead};
        {Value, ExpiryDate} ->
          ExpiryEDate = {list_to_integer(string:right(ExpiryDate, 4)),
                         list_to_integer(string:substr(ExpiryDate, 2, 2)),
                         list_to_integer(string:left(ExpiryDate, 2))},
          case load_pins(Fd, Value, ExpiryEDate) of
            ok ->
              ok;
            {error, ReasonLoad} ->
              io:format("Failed to load file ~p~n", [ReasonLoad]),
              {error, ReasonLoad}
          end
      end
  end.

read_file_header(Fd) ->
  case file:read_line(Fd) of
    {ok, DataLine} ->
      case catch string:tokens(DataLine, ",\n") of
        [Value, _, _, ExpiryDate, _, _] ->
          {Value, ExpiryDate};
        Header ->
          io:format("Header is ~p~n", [Header]),
          {error, "bad header"}
      end;
    eof ->
      {error, "no header"};
    {error, Reason} ->
      {error, Reason}
  end.

load_pins(Fd, Value, ExpiryEDate) ->
  case file:read_line(Fd) of
    {ok, DataLine} ->
      case catch string:tokens(DataLine, ",\n") of
        {'EXIT', Reason} ->
          {error, Reason};
        [Pin, Seq] ->
          Id = uuid(),
          Now = os:timestamp(),
          mnesia:dirty_write(#pins{id=Id, seq=list_to_binary(Seq),
                                   pin=list_to_binary(Pin),
                                   value=list_to_binary(Value), status=active,
                                   created_on=Now, expires_on=ExpiryEDate}),
          load_pins(Fd, Value, ExpiryEDate);
        AnythingElse ->
          io:format("Received Something Else ~p~n", [AnythingElse]),
          {error, "Format Error"}
      end;
    eof ->
      ok;
    {error, Reason} ->
      {error, Reason}
  end.

handle_open_pin(Pin, EndUser) ->
  Now = os:timestamp(),
  OpenPinFun = fun() ->
    case mnesia:index_read(pins, Pin, #pins.pin) of
      [] ->
        {error, not_found};
      [PinRecord] ->
        case PinRecord#pins.status of
          active ->
            mnesia:write(PinRecord#pins{status=open,
                                              opened_on=Now,
                                              opened_by=EndUser}),
            {ok, PinRecord#pins.seq, PinRecord#pins.value};
          open ->
            if
              PinRecord#pins.opened_by == EndUser,
                  EndUser =/= undefined ->
                {ok, PinRecord#pins.seq, PinRecord#pins.value,
                 PinRecord#pins.opened_on};
              true ->
                {error, not_found}
            end;
          _ ->
            {error, not_found}
        end
    end
  end,
  mnesia:activity(transaction, OpenPinFun).

handle_close_pin(Seq, EndUser) ->
  ClosePinFun = fun() ->
    case mnesia:index_read(pins, Seq, #pins.seq) of
      [] ->
        {error, not_found};
      [PinRecord] ->
        if
          PinRecord#pins.opened_by == EndUser,
              EndUser =/= undefined ->
            case PinRecord#pins.status of
              active ->
                {ok, already_closed};
              open ->
                mnesia:write(PinRecord#pins{status=active}),
                ok;
              _ ->
                {error, not_allowd}
            end;
          true ->
            {error, not_found}
        end
    end
  end,
  mnesia:activity(transaction, ClosePinFun).

handle_burn_pin(Seq, EndUser) ->
  BurnPinFun = fun() ->
    case mnesia:index_read(pins, Seq, #pins.seq) of
      [] ->
        {error, not_found};
      [PinRecord] ->
        if
          PinRecord#pins.opened_by == EndUser,
              EndUser =/= undefined ->
            case PinRecord#pins.status of
              active ->
                {ok, not_allowed};
              open ->
                Now = os:timestamp(),
                mnesia:write(PinRecord#pins{status=used,
                                                  used_on=Now,
                                                  used_by=EndUser}),
                ok;
              used ->
                {ok, already_used}
            end;
          true ->
            {error, not_found}
        end
    end
  end,
  mnesia:activity(transaction, BurnPinFun).

handle_list_pins(OrderId, Status, PageNo, PageSize) ->
  Pins = mnesia:dirty_index_read(pins, OrderId, #pins.order),
  PinsFiltered = handle_list_pins_status(Pins, Status),
  case get_keysforpage(PinsFiltered, to(int, PageNo), to(int, PageSize)) of
    {error, Reason} ->
      {error, Reason};
    {ok, PinsSubSet, TotalPages} ->
      {ok, PinsSubSet, TotalPages}
  end.

handle_list_pins_status(Pins, Status) ->
  lists:filter(
    fun(Pin) ->
      Pin#pins.status == Status
    end,
    Pins
  ).

handle_pin_details(Seq) ->
  case mnesia:dirty_index_read(pins, Seq, #pins.seq) of
    [] ->
      {error, not_found};
    [PinRecord] ->
      {ok, PinRecord}
  end.

handle_retire_pin(Seq) ->
  case mnesia:dirty_index_read(pins, Seq, #pins.seq) of
    [] ->
      {error, not_found};
    [PinRecord] ->
      case PinRecord#pins.status of
        retire ->
          {error, not_found};
        _ ->
          mnesia:dirty_write(PinRecord#pins{status=retire})
      end
  end.
