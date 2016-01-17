%%%=======================================================================%%%
%%% PIN Engine : Utility Tools (pure funtions)
%%%=======================================================================%%%
-module(pine_tools).
-author("Chaitanya Chalasani <cchalasani@me.com>").

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
         try_find_map/2,
         try_find_map/3,
         try_to_int/1,
         try_to_hexbin/1,
         try_to_hexbin_list/1,
         now_to_iso8601/1,
         try_to_iso8601/2,
         update/2,
         get_keysforpage/3,
         maps_to_record/3,
         record_to_tuplelist/2,
         readall/1,
         iso8601_to_ts/1,
         has/2,
         has_ic/2]).

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
now_to_seconds(Now) ->
  calendar:datetime_to_gregorian_seconds(calendar:now_to_local_time(Now)).

timestamp_diff_seconds(ToTS, FromTS) ->
  now_to_seconds(ToTS) - now_to_seconds(FromTS).

did_it_happen(0, _From, _To) ->
  true;
did_it_happen(In, From, 0) when is_integer(In), is_integer(From) ->
  From < In;
did_it_happen(In, 0, To) when is_integer(In), is_integer(To) ->
  In < To;
did_it_happen(In, From, To) when is_integer(In), is_integer(From),
                                 is_integer(To)->
  (From < In) andalso (In < To);
did_it_happen(_In, _From, _To) ->
  false.

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
  to_binary(Data);
to(seconds, Data) ->
  to_seconds(Data).

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
to_binary(undefined) ->
  <<>>;
to_binary(Data) when is_atom(Data) ->
  atom_to_binary(Data, utf8);
to_binary(Data) when is_binary(Data) ->
  Data;
to_binary({A,B,C}) ->
  to_binary(now_to_iso8601({A,B,C}));
to_binary(Data) ->
  io:format("Unable to transform ~p into binary~n", [Data]),
  <<>>.

to_seconds(Data) when is_binary(Data) ->
  to_seconds(to_list(Data));
to_seconds(Data) when is_list(Data) ->
  case string:tokens(Data, "-T:") of
    [YYYY, MM, DD, H, M, S] ->
        to_seconds({{to(int, YYYY), to(int, MM), to(int, DD)},
        {to(int, H), to(int, M), to(int, S)}});
    _ ->
      undefined
  end;
to_seconds({A, B, C}) ->
  now_to_seconds({A, B, C});
to_seconds({{YYYY, MM, SS}, {H, M, S}}) ->
  calendar:datetime_to_gregorian_seconds({{YYYY, MM, SS}, {H, M, S}});
to_seconds(Data) ->
  io:format("Unable to transform ~p into seconds~n", [Data]),
  0.

try_to_iso8601({A, B, C}, Default) ->
  case (catch now_to_iso8601({A, B, C})) of
    {'EXIT', _Reason} ->
      Default;
    Result ->
      Result
  end;
try_to_iso8601({{YYYY, MM, DD}, {H, M, S}}, Default) ->
  case (catch lists:flatten(io_lib:format("~p-~p-~pT~p:~p:~p",
                                          [YYYY, MM, DD, H, M, S]))) of
    {'EXIT', _Reason} ->
      Default;
    Result ->
      Result
  end;
try_to_iso8601(_Other, Default) ->
  Default.

now_to_iso8601({A, B, C}) ->
  {{YYYY, MM, DD}, {H, M, S}} = calendar:now_to_local_time({A,B,C}),
  lists:flatten(io_lib:format("~p-~p-~pT~p:~p:~p", [YYYY, MM, DD, H, M, S])).

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
  try_find_map(Key, Map, undefined).

try_find_map(Key, Map, Default) ->
  case maps:find(Key, Map) of
    error ->
      Default;
    {ok, Value} ->
      Value
  end.

try_to_int(Data) ->
  case (catch to_int(Data)) of
    {'EXIT', _Reason} ->
      -1;
    Int ->
      Int
  end.

try_to_hexbin(Data) ->
  case (catch bin_to_hexbin(Data)) of
    {'EXIT', _Reason} ->
      to_binary(Data);
    HexBin ->
      HexBin
  end.

try_to_hexbin_list(DataList) when is_list(DataList) ->
  [try_to_hexbin(Data)||Data<-DataList];
try_to_hexbin_list(_DataListUnknown) ->
  [].

update(Tuple1, Tuple2) when is_tuple(Tuple1),is_tuple(Tuple2) ->
  list_to_tuple(update(tuple_to_list(Tuple1), tuple_to_list(Tuple2)));
update(List1, List2) when is_list(List1),is_list(List2),
                          length(List1) == length(List2) ->
  update(List1, List2, []).

update([H|T1], [H|T2], Update) ->
  update(T1, T2, [H|Update]);
update([H1|T1], [H2|T2], Update) when H2 == undefined ->
  update(T1, T2, [H1|Update]);
update([_H1|T1], [H2|T2], Update) ->
  update(T1, T2, [H2|Update]);
update([], [], Update) -> lists:reverse(Update).

get_keysforpage(Keys, _PageNo, PageSize) when PageSize < 0 ->
  {ok, Keys, 1};
get_keysforpage(Keys, PageNo, PageSize) ->
  TotalPages = length(Keys) div PageSize + 1,
  if
    PageNo > TotalPages ->
      {error, invalid_page};
    true ->
      PageNoEnsure = if PageNo < 1 -> 1; true -> PageNo end,
      FirstPosition = (PageNoEnsure - 1) * PageSize + 1,
      {ok, lists:sublist(Keys, FirstPosition, PageSize), TotalPages}
  end.

maps_to_record(RecordName, Maps, Fields) ->
  tuplelist_to_record(RecordName, maps:to_list(Maps), Fields).

tuplelist_to_record(RecordName, TupleList, Fields) ->
  list_to_tuple([RecordName|lists:map(
                              fun(Field) ->
                                  case lists:keyfind(Field,
                                                     1,
                                                     TupleList) of
                                    false ->
                                      undefined;
                                    {_, Value} ->
                                      Value
                                  end
                              end,
                              Fields
                             )]).

record_to_tuplelist(Record, Fields) ->
  [_|ValuesList] = tuple_to_list(Record),
  lists:zip(Fields, ValuesList).

readall(Table) ->
  [io:format("~p~n", [mnesia:dirty_read(Table, Key)])||
   Key <- mnesia:dirty_all_keys(Table)].

iso8601_to_ts(Iso8601) ->
  Tokens = string:tokens(Iso8601, "-T:"),
  [YYYY, MM, DD, H, M, S] = [list_to_integer(S) || S <- Tokens],
  DateTime = {{YYYY, MM, DD}, {H, M, S}},
  Seconds = calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200,
  {Seconds div 1000000, Seconds rem 1000000, 0}.

has_ic(String, SubString) ->
  has(string:to_lower(to(string, String)),
      string:to_lower(to(string, SubString))).

has(String, SubString) ->
  case string:str(String, SubString) of
    0 ->
      false;
    _ ->
      true
  end.
