-module(pine_log).
-author("Chaitanya Chalasani <cchalasani@me.com>").

-behaviour(gen_event).

-import(pine_mnesia, [read_conf/2]).
-import(proplists, [get_value/2]).
-import(pine_tools, [timestamp_diff_seconds/2, ts_to_str/1,
                    ceiling/1, split_string/2, int_len/1, int_to_list_pad/3]).

-define(MAX_SUFFIX_LEN, 8).
-define(MAX_LINE_LEN, 80).
-define(MSG_PREFIX_LEN, 36).
-define(INDEX_FILE_EXTN, "0.idx").

-export([log/3, update/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

log(Level, Type, Msg) ->
  MsgString = case io_lib:printable_list(Msg) of
    false ->
      lists:flatten(io_lib:format("~p",[Msg]));
    true ->
      Msg
  end,
  Timestamp = os:timestamp(),
  gen_event:notify(pine_gen_event, {Timestamp, Level, Type, MsgString}).

update() ->
  gen_event:call(pine_gen_event, update).

init([]) ->
  CfgLs = get_cfgs(),
  DstLs = get_dsts(CfgLs),
  {ok, {CfgLs, DstLs}}.

handle_event(Log, {CfgLs, DstLs}) ->
  DstLsU = send_log(Log, get_dsts(CfgLs, DstLs)),
  {ok, {CfgLs, DstLsU}}.

handle_call(_Request, State) ->
	{ok, ok, State}.

handle_info(_Info, State) ->
	{ok, State}.

terminate(_Arg, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

get_cfgs() ->
  DstIdLs = read_conf(log_dest, [shell]),
  get_cfgs(DstIdLs).

get_cfgs(DstIdLs) ->
  get_cfgs(DstIdLs, []).

get_cfgs([shell|DstIdLs], CfgLs) ->
  get_cfgs(DstIdLs, [shell|CfgLs]);
get_cfgs([file|DstIdLs], CfgLs) ->
  FileCfgs = [{Cfg, read_conf(Cfg, Def)}||
      {Cfg, Def} <- [{log_path, "log/apps"},
                     {log_prefix, "pine_"},
                     {log_extension, ".log"},
                     {log_size, 100},
                     {log_duration, 60},
                     {log_rotation, 10},
                     {log_archive_path, "log/archive"},
                     {log_compress, true}]],
  get_cfgs(DstIdLs, [{file, FileCfgs}|CfgLs]);
get_cfgs([_|DstIdLs], CfgLs) ->
  get_cfgs(DstIdLs, CfgLs);
get_cfgs([], CfgLs) ->
  CfgLs.

get_dsts(CfgLs) ->
  get_dsts(CfgLs, []).

get_dsts([], DstLs) ->
  DstLs;
get_dsts([shell|CfgLs], DstLs) ->
  case lists:member(shell, DstLs) of
    true ->
      get_dsts(CfgLs, DstLs);
    false ->
      get_dsts(CfgLs, [shell|DstLs])
  end;
get_dsts([{file, FileCfgs}|CfgLs], DstLs) ->
  case lists:keyfind(file, 1, DstLs) of
    false ->
      FileDst = get_filedst(FileCfgs),
      get_dsts(CfgLs, [FileDst|DstLs]);
    FileDstOld ->
      FileDst = get_filedst(FileCfgs, FileDstOld),
      DstLsU = lists:keyreplace(file, 1, DstLs, FileDst),
      get_dsts(CfgLs, DstLsU)
  end.

get_filedst(FileCfgs) ->
  case read_fileidx(FileCfgs) of
    error ->
      get_filedst(FileCfgs, {file, undefined, []});
    {TsOld, FcOld} ->
      SizeOld = get_filesize(FcOld, FileCfgs),
      get_filedst(FileCfgs, {file, undefined,
                             [SizeOld, TsOld, FcOld]})
  end.

get_filedst(FileCfgs, {file, undefined, []}) ->
  Now = os:timestamp(),
  Fd = create_file(FileCfgs, 1, [write]),
  write_fileidx(FileCfgs, [Now, 1]),
  {file, Fd, [0, Now, 1]};
get_filedst(FileCfgs, {file, undefined, [SizeOld, TsOld, FcOld]}) ->
  case is_refresh_req(FileCfgs, [SizeOld, TsOld]) of
    no ->
      Fd = create_file(FileCfgs, FcOld, [append]),
      {file, Fd, [SizeOld, TsOld, FcOld]};
    yes ->
      Now = os:timestamp(),
      FcNew = get_nextfile(FileCfgs, FcOld),
      Fd = create_file(FileCfgs, FcNew, [write]),
      write_fileidx(FileCfgs, [Now, FcNew]),
      {file, Fd, [0, Now, FcNew]}
  end;
get_filedst(FileCfgs, {file, FdOld, [SizeOld, TsOld, FcOld]}) ->
  case is_refresh_req(FileCfgs, [SizeOld, TsOld]) of
    no ->
      {file, FdOld, [SizeOld, TsOld, FcOld]};
    yes ->
      file:close(FdOld),
      Now = os:timestamp(),
      FcNew = get_nextfile(FileCfgs, FcOld),
      FdNew = create_file(FileCfgs, FcNew, [write]),
      write_fileidx(FileCfgs, [Now, FcNew]),
      {file, FdNew, [0, Now, FcNew]}
  end.

read_fileidx(FileCfgs) ->
  Path = get_value(log_path, FileCfgs),
  Prefix = get_value(log_prefix, FileCfgs),
  IdxFile = Prefix ++ ?INDEX_FILE_EXTN,
  case file:read_file(filename:join(Path, IdxFile)) of
    {error, _Reason} ->
      error;
    {ok, IdxData} ->
      binary_to_term(IdxData)
  end.

get_filesize(FileNum, FileCfgs) ->
  LogFile = mk_filename(FileNum, FileCfgs),
  filelib:file_size(LogFile).

mk_filename(FileNum, FileCfgs) ->
  Path = get_value(log_path, FileCfgs),
  Prefix = get_value(log_prefix, FileCfgs),
  SuffixLen = case get_value(log_rotation, FileCfgs) of
    N when is_integer(N) andalso N > 0 ->
      int_len(N);
    _ ->
      ?MAX_SUFFIX_LEN
  end,
  Suffix = int_to_list_pad(FileNum, SuffixLen, $0),
  Extension = get_value(log_extension, FileCfgs),
  filename:join(Path, Prefix++Suffix++Extension).

is_refresh_req(FileCfgs, [Size, Ts]) ->
  SizeCfg = get_value(log_size, FileCfgs),
  DuraCfg = get_value(log_duration, FileCfgs),
  SizeCriteria = check_size_criteria(SizeCfg, Size),
  DuraCriteria = check_dura_criteria(DuraCfg, Ts),
  if
    SizeCriteria;DuraCriteria ->
      yes;
    true ->
      no
  end.

check_size_criteria(SizeCfg, Size) when is_integer(SizeCfg)
    andalso SizeCfg > 0 ->
  (SizeCfg * 1024 * 1024) =< Size;
check_size_criteria(_SizeCfg, _Size) ->
  true.

check_dura_criteria(DuraCfg, Ts) when is_integer(DuraCfg)
    andalso DuraCfg > 0 ->
  TimeDiff = timestamp_diff_seconds(os:timestamp(), Ts),
  (DuraCfg * 60) =< TimeDiff;
check_dura_criteria(_DuraCfg, _Ts) ->
  true.

get_nextfile(FileCfgs, FcOld) ->
  case get_value(log_rotation, FileCfgs) of
    Max when is_integer(Max) andalso Max > 0 ->
      if
        FcOld < Max -> FcOld + 1;
        true -> 1
      end;
    _ ->
      FcOld + 1
  end.

create_file(FileCfgs, Fc, Options) ->
  File = mk_filename(Fc, FileCfgs),
  {ok, Fd} = file:open(File, Options),
  Fd.

write_fileidx(FileCfgs, [Ts, Fc]) ->
  Path = get_value(log_path, FileCfgs),
  Prefix = get_value(log_prefix, FileCfgs),
  IdxFile = Prefix ++ ?INDEX_FILE_EXTN,
  file:write_file(filename:join(Path, IdxFile),
                  term_to_binary({Ts, Fc})).

send_log(Log, DstLs) ->
  send_log(Log, DstLs, []).

send_log({Ts, Level, Type, Msg},
         [{file, Fd, [SizeOld, TsOld, FcOld]}|DstLs], DstLsU) ->
  write_log({Ts, Level, Type, Msg}, Fd),
  Len = length(Msg),
  Size = ceiling(Len / ?MAX_LINE_LEN) * ?MSG_PREFIX_LEN + Len,
  send_log({Ts, Level, Type, Msg}, DstLs,
           [{file, Fd, [SizeOld + Size, TsOld, FcOld]}|DstLsU]);
send_log({Ts, Level, Type, Msg}, [shell|DstLs],
         DstLsU) ->
  write_log({Ts, Level, Type, Msg}, standard_io),
  send_log({Ts, Level, Type, Msg}, DstLs,
           [shell|DstLsU]);
send_log(_Log, [], DstLsU) ->
  lists:reverse(DstLsU).

write_log({Ts, Level, Type, Msg}, Fd) ->
  [io:format(Fd, "~s ~s ~s ~s~n",
             [ts_to_str(Ts), format_type(Type),
              format_level(Level), MsgPart])
   || MsgPart <- split_message(Msg)].

format_level(info) ->
  "[INFO]";
format_level(debug) ->
  "[DBUG]";
format_level(warn) ->
  "[WARN]";
format_level(error) ->
  "[ERRO]";
format_level(fatal) ->
  "[FATL]";
format_level(_) ->
  "[DBUG]".

format_type(in) ->
  "-->";
format_type(out) ->
  "<--";
format_type(exec) ->
  "---";
format_type(_) ->
  "---".

split_message(Message) ->
  split_message(Message, []).

split_message([], MessageSplitList) ->
  lists:reverse(MessageSplitList);
split_message(Message, MessageSplitList) ->
  {Split, Rest} = split_string(?MAX_LINE_LEN, Message),
  split_message(Rest, [Split|MessageSplitList]).
