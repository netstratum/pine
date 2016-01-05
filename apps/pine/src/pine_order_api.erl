-module(pine_order_api).

-behaviour(gen_server).

-include("pine_mnesia.hrl").

-import(pine_tools, [maps_to_record/3,
                     try_find_map/2,
                     try_to_int/1,
                     record_to_tuplelist/2,
                     hexbin_to_bin/1]).
-import(pine_user, [validate/2]).
-import(pine_order, [create/2,
                     modify/2,
                     search/7,
                     list/2,
                     lock/3,
                     unlock/3,
                     retire/3,
                     approve/3,
                     reject/3,
                     activate/3]).

%% API functions
-export([start_link/0,
         create_api/1,
         modify_api/1,
         search_api/1,
         list_api/1,
         lock_api/1,
         unlock_api/1,
         retire_api/1,
         approve_api/1,
         reject_api/1,
         activate_api/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% API functions
%%%===================================================================

create_api(#{http_token:=Cookie,
             http_source:=Source} = Maps) ->
  Order = maps_to_record(orders,
                         Maps,
                         record_info(fields, orders)),
  case validate(Cookie, Source) of
    {ok, User} ->
      create(User, Order);
    Error ->
      Error
  end.

modify_api(#{http_token:=Cookie,
             http_source:=Source} = Maps) ->
  Order = maps_to_record(orders,
                         Maps,
                         record_info(fields, orders)),
  case validate(Cookie, Source) of
    {ok, User} ->
      modify(User, Order);
    Error ->
      Error
  end.

search_api(#{http_token:=Cookie,
             http_source:=Source,
             page_no:=PageNo,
             page_size:=PageSize} = Maps) ->
  Name = try_find_map(name, Maps),
  Notes = try_find_map(notes, Maps),
  Label = try_find_map(label, Maps),
  StartTS = try_find_map(start_ts, Maps),
  EndTS = try_find_map(end_ts, Maps),
  PageNoInt = try_to_int(PageNo),
  PageSizeInt = try_to_int(PageSize),
  case validate(Cookie, Source) of
    {ok, _User} ->
      case search(Name, Notes, Label, StartTS, EndTS, PageNoInt, PageSizeInt) of
        {ok, Rows, TotalPages} ->
          OrderTupleList = mk_orderTupleList(Rows),
          {ok, [{orders, OrderTupleList},
                {total_pages, TotalPages}]};
        Other ->
          Other
      end;
    Error ->
      Error
  end.

list_api(#{http_token:=Cookie,
           http_source:=Source,
           page_no:=PageNo,
           page_size:=PageSize}) ->
  PageNoInt = try_to_int(PageNo),
  PageSizeInt = try_to_int(PageSize),
  case validate(Cookie, Source) of
    {ok, _User} ->
      case list(PageNoInt, PageSizeInt) of
        {ok, Rows, TotalPages} ->
          OrderTupleList = mk_orderTupleList(Rows),
          {ok, [{orders, OrderTupleList},
                {total_pages, TotalPages}]};
        Other ->
          Other
      end;
    Error ->
      Error
  end.

lock_api(#{http_token:=Cookie,
           http_source:=Source,
           id:=Id,
           comment:=Comment}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      lock(User, IdBin, Comment);
    Error ->
      Error
  end.

unlock_api(#{http_token:=Cookie,
           http_source:=Source,
           id:=Id,
           comment:=Comment}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      unlock(User, IdBin, Comment);
    Error ->
      Error
  end.

retire_api(#{http_token:=Cookie,
           http_source:=Source,
           id:=Id,
           comment:=Comment}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      retire(User, IdBin, Comment);
    Error ->
      Error
  end.

approve_api(#{http_token:=Cookie,
           http_source:=Source,
           id:=Id,
           comment:=Comment}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      approve(User, IdBin, Comment);
    Error ->
      Error
  end.

reject_api(#{http_token:=Cookie,
           http_source:=Source,
           id:=Id,
           comment:=Comment}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      reject(User, IdBin, Comment);
    Error ->
      Error
  end.

activate_api(#{http_token:=Cookie,
           http_source:=Source,
           id:=Id,
           comment:=Comment}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      activate(User, IdBin, Comment);
    Error ->
      Error
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

mk_orderTupleList(Rows) ->
  lists:map(
    fun(Row) ->
        {record_to_tuplelist(Row, record_info(fields, orders))}
    end,
    Rows
   ).

