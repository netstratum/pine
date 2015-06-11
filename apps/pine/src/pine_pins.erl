-module(pine_pins).
-behaviour(gen_server).

-include("pine_mnesia.hrl").

-import(pine_tools, [int_to_list_pad/3]).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-export([generate/6]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  random:seed(os:timestamp()), 
  {ok, ok}.

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

generate({PrinterCode, BrandCode, ExpiryDate, PrinterSeqNumber, SerialLength,
          CountryCode, RegionCode, OrderId}, 
          OrderName, FaceValue, Quantity, Length, Form) ->
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
