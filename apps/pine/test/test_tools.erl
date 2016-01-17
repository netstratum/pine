-module(test_tools).
-export([encode_json/1, decode_json/1, timestamp_diff_seconds/2,
         ts_to_str/1, floor/1, ceiling/1, split_string/2,
         int_len/1, int_to_list_pad/3, uuid/0, md5/1, uptime/0,
         hexstr_to_bin/1, bin_to_hexstr/1, hexstr_to_list/1, list_to_hexstr/1,
         get_member/2]).

encode_json(ErlangTerm) when is_tuple(ErlangTerm) ->
  jiffy:encode({[ErlangTerm]});
encode_json(ErlangTerm) when is_list(ErlangTerm) ->
  jiffy:encode({ErlangTerm});
encode_json(ErlangTerm) ->
  jiffy:encode(ErlangTerm).

decode_json(JsonBinary) ->
  {ErlangTerm} = jiffy:decode(JsonBinary),
  ErlangTerm.

timestamp_diff_seconds({ToMega, ToSec, _}, {FromMega, FromSec, _}) ->
  (ToMega*1000000 + ToSec) - (FromMega*1000000 + FromSec).

ts_to_str({MegaSecs, Secs, MicroSecs}) ->
  {{YYYY, MM, DD}, {H, M, S}} = calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
  MS = round(MicroSecs / 1000),
  lists:flatten(
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~3..0w",
                  [YYYY, MM, DD, H, M, S, MS])).

floor(X) when X < 0 ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T - 1
  end;
floor(X) ->
  trunc(X).


ceiling(X) when X < 0 ->
  trunc(X);
ceiling(X) ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T + 1
  end.

split_string(N, String) ->
  case catch lists:split(N, String) of
    {'EXIT', _Reason} ->
      {String, []};
    Split ->
      Split
  end.

int_len(N) ->
  trunc(math:log10(N)) + 1.

int_to_list_pad(N, L, C) ->
  string:right(integer_to_list(N), L, C).

uuid() ->
  uuid:v4().

md5(IoList) ->
  crypto:hash(md5, IoList).

hex(N) when N < 10 ->
      $0+N;
hex(N) when N >= 10, N < 16 ->
      $a+(N-10).

int(C) when $0 =< C, C =< $9 ->
      C - $0;
int(C) when $A =< C, C =< $F ->
      C - $A + 10;
int(C) when $a =< C, C =< $f ->
      C - $a + 10.

to_hex(N) when N < 256 ->
      [hex(N div 16), hex(N rem 16)].

list_to_hexstr([]) ->
      [];
list_to_hexstr([H|T]) ->
      to_hex(H) ++ list_to_hexstr(T).

bin_to_hexstr(Bin) ->
      list_to_hexstr(binary_to_list(Bin)).

hexstr_to_bin(S) ->
      list_to_binary(hexstr_to_list(S)).

hexstr_to_list([X,Y|T]) ->
      [int(X)*16 + int(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
      [].

uptime() ->
  {UpTime, _} = erlang:statistics(wall_clock),
  {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
  lists:flatten(
    io_lib:format("~p days, ~p hours, ~p minutes and ~p seconds",
                  [D,H,M,S])).

get_member([], Value) ->
  {ok, binary_to_list(Value)};
get_member(_KeyTree, []) ->
  {error, not_found};
get_member(KeyTree, {Object}) ->
  get_member(KeyTree, Object);
get_member(KeyTree, [{Object}|_]) ->
  get_member(KeyTree, Object);
get_member([Key|KeyTree], Object) ->
  case lists:keyfind(list_to_binary(Key), 1, Object) of
    false ->
      {error, not_found};
    {_, Value} ->
      get_member(KeyTree, Value)
  end.
