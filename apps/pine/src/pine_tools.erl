%%%=======================================================================%%%
%%% PIN Engine : Utility Tools (pure funtions)
%%%=======================================================================%%%
-module(pine_tools).

%% API functions
-export([encode_json/1,
         decode_json/1,
         timestamp_diff_seconds/2,
         now_to_seconds/1,
         did_it_happen/3,
         ts_to_str/1,
         floor/1,
         ceiling/1,
         split_string/2,
         int_len/1,
         int_to_list_pad/3,
         uuid/0,
         md5/1,
         uptime/0,
         hexstr_to_bin/1,
         bin_to_hexstr/1,
         hexstr_to_list/1,
         list_to_hexstr/1,
         to/2,
         get_missing/2,
         encode_params/1,
         decode_params/1,
         hexbin_to_bin/1,
         bin_to_hexbin/1,
         ts_to_bin/1,
         try_find_map/2]).

%%=======================================================================%%
%% API functions
%%=======================================================================%%

%%-----------------------------------------------------------------------%%
%% @doc
%% Encode erlang param list to json
%%
%% @spec encode_params(ErlangParams) -> JsonParams
%% @end

encode_params(ErlangParams) ->
  encode_json(ErlangParams).
%%-----------------------------------------------------------------------%%
%% @doc
%% Encode erlang term to json
%%
%% @spec encode_json(ErlangTerm) -> JsonTerm
%% @end

encode_json(ErlangTerm) when is_tuple(ErlangTerm) ->
  jiffy:encode({[ErlangTerm]});
encode_json(ErlangTerm) when is_list(ErlangTerm) ->
  jiffy:encode({ErlangTerm});
encode_json(ErlangTerm) ->
  jiffy:encode(ErlangTerm).

%%-----------------------------------------------------------------------%%
%% @doc
%% Decode json to erlang param list
%%
%% @spec decode_params(JsonParams) -> ErlangTermList
%% @end
decode_params(JsonParams) ->
  ErlangParams = decode_json(JsonParams),
  [{to(atom, Param), Value} || {Param, Value} <- ErlangParams].

%%-----------------------------------------------------------------------%%
%% @doc
%% Decode json to erlang term
%%
%% @spec decode_json(JsonParams) -> ErlangTerm
%% @end
decode_json(JsonBinary) ->
  {ErlangTerm} = jiffy:decode(JsonBinary),
  ErlangTerm.

%%-----------------------------------------------------------------------%%
%% @doc
%% Convert erlang timestamp to seconds
%%
%%
now_to_seconds({Mega, Sec, _}) ->
  Mega*1000000 + Sec.

timestamp_diff_seconds(ToTS, FromTS) ->
  now_to_seconds(ToTS) - now_to_seconds(FromTS).

did_it_happen(In, From, To) ->
  (now_to_seconds(From) < now_to_seconds(In)) andalso
  (now_to_seconds(In) < now_to_seconds(To)).

ts_to_bin(ErlangNow) ->
  list_to_binary(ts_to_str(ErlangNow)).

ts_to_str({MegaSecs, Secs, MicroSecs}) ->
  {{YYYY, MM, DD}, {H, M, S}} =
    calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
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

hexbin_to_bin(B) ->
  hexstr_to_bin(binary_to_list(B)).

bin_to_hexbin(B) ->
  list_to_binary(bin_to_hexstr(B)).

uptime() ->
  {UpTime, _} = erlang:statistics(wall_clock),
  {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
  lists:flatten(
    io_lib:format("~p days, ~p hours, ~p minutes and ~p seconds",
                  [D,H,M,S])).

to(int, Data) ->
  to_int(Data);
to(float, Data) ->
  to_float(Data);
to(list, Data) ->
  to_list(Data);
to(atom, Data) ->
  to_atom(Data);
to(string, Data) ->
  to_string(Data);
to(binary, Data) ->
  to_binary(Data).

to_int(Data) when is_atom(Data) ->
  to_int(atom_to_list(Data));
to_int(Data) when is_binary(Data) ->
  to_int(binary_to_list(Data));
to_int(Data) when is_list(Data) ->
  list_to_integer(Data);
to_int(Data) when is_float(Data) ->
  round(Data);
to_int(Data) when is_integer(Data) ->
  Data.

to_float(Data) when is_binary(Data) ->
  binary_to_float(Data);
to_float(Data) when is_atom(Data) ->
  to_float(atom_to_list(Data));
to_float(Data) when is_list(Data) ->
  list_to_float(Data);
to_float(Data) when is_integer(Data) ->
  Data/1;
to_float(Data) when is_float(Data) ->
  Data.

to_list(Data) when is_binary(Data) ->
  binary_to_list(Data);
to_list(Data) when is_atom(Data) ->
  atom_to_list(Data);
to_list(Data) when is_float(Data) ->
  float_to_list(Data, [{decimal, 2}, compact]);
to_list(Data) when is_integer(Data) ->
  integer_to_list(Data);
to_list(Data) when is_list(Data) ->
  Data.

to_atom(Data) when is_binary(Data) ->
  to_atom(binary_to_list(Data));
to_atom(Data) when is_list(Data) ->
  list_to_atom(Data);
to_atom(Data) when is_integer(Data) ->
  to_atom(integer_to_list(Data));
to_atom(Data) when is_float(Data) ->
  to_atom(float_to_list(Data, [{decimals, 2}, compact]));
to_atom(Data) when is_atom(Data) ->
  Data.

to_string(Data) when is_list(Data) ->
  case io_lib:printable_list(Data) of
    true ->
      Data;
    false ->
      lists:flatten(io_lib:format("~p", [Data]))
  end;
to_string(Data) ->
  to_string(to_list(Data)).

to_binary(Data) when is_list(Data) ->
  list_to_binary(Data);
to_binary(Data) when is_integer(Data) ->
  integer_to_binary(Data);
to_binary(Data) when is_float(Data) ->
  float_to_binary(Data, [{decimals, 2}, compact]);
to_binary(Data) when is_atom(Data) ->
  atom_to_binary(Data, utf8);
to_binary(Data) when is_binary(Data) ->
  Data.

get_missing(Map, ParamList) when is_map(Map) ->
  lists:filtermap(
    fun(Param) ->
        case maps:is_key(Param, Map) of
          true ->
            false;
          false ->
            {true, Param}
        end
    end,
    ParamList
   );
get_missing(List, ParamList) when is_list(List) ->
  lists:filtermap(
    fun(Param) ->
        case lists:keyfind(Param, 1, List) of
          false ->
            {true, Param};
          _ ->
            false
        end
    end,
    ParamList
   ).

try_find_map(Key, Map) ->
  case maps:find(Key, Map) of
    error ->
      undefined;
    {ok, Value} ->
      Value
  end.
