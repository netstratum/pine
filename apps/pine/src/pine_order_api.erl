-module(pine_order_api).
-author("Chaitanya Chalasani <cchalasani@me.com>").

-behaviour(gen_server).

-include("pine_mnesia.hrl").

-import(pine_tools, [maps_to_record/3,
                     try_find_map/2,
                     try_to_int/1,
                     record_to_tuplelist/2,
                     iso8601_to_ts/1,
                     hexbin_to_bin/1,
                     try_to_hexbin/1,
                     try_to_iso8601/2,
                     bin_to_hexbin/1]).
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
  Order = maps_to_orderRecord(Maps),
  case validate(Cookie, Source) of
    {ok, User} ->
      create(User, Order);
    Error ->
      Error
  end.

modify_api(#{http_token:=Cookie,
             http_source:=Source} = Maps) ->
  Order = maps_to_orderRecord(Maps),
  case validate(Cookie, Source) of
    {ok, User} ->
      modify(User, Order);
    Error ->
      Error
  end.

maps_to_orderRecord(Maps) ->
  Order = maps_to_record(orders,
                         Maps,
                         record_info(fields, orders)),
  IdV2 = case Order#orders.id of
           undefined ->
             undefined;
           Id ->
             hexbin_to_bin(Id)
         end,
  ScheduleV2 = case Order#orders.schedule of
                 undefined ->
                   undefined;
                 Schedule ->
                   ScheduleList = binary_to_list(Schedule),
                   case (catch iso8601_to_ts(ScheduleList)) of
                     {'EXIT', _Reason} ->
                       undefined;
                     ErlNow ->
                       ErlNow
                   end
               end,
  CryptoKeyV2 = case Order#orders.crypto_key of
                  undefined ->
                    undefined;
                  CryptoKey ->
                    hexbin_to_bin(CryptoKey)
                end,
  PinTemplateV2 = case Order#orders.pin_template of
                    undefined ->
                      undefined;
                    PinTemplate ->
                      hexbin_to_bin(PinTemplate)
                  end,
  Order#orders{id=IdV2,
               schedule=ScheduleV2,
               pin_template=PinTemplateV2,
               crypto_key=CryptoKeyV2}.


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
  Now = os:timestamp(),
  InitApiFun = fun() ->
    mnesia:write(#api_handlers{function = <<"order.create">>,
                               arguments = [name, pin_template,
                                            pin_count],
                               handler = {?MODULE, create_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.modify">>,
                               arguments = [id, modified_comments],
                               handler = {?MODULE, modify_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.search">>,
                               arguments = [page_no, page_size],
                               handler = {?MODULE, search_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.list">>,
                               arguments = [page_no, page_size],
                               handler = {?MODULE, list_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.lock">>,
                               arguments = [id, comment],
                               handler = {?MODULE, lock_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.unlock">>,
                               arguments = [id, comment],
                               handler = {?MODULE, unlock_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.retire">>,
                               arguments = [id, comment],
                               handler = {?MODULE, retire_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.approve">>,
                               arguments = [id, comment],
                               handler = {?MODULE, approve_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.reject">>,
                               arguments = [id, comment],
                               handler = {?MODULE, reject_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.activate">>,
                               arguments = [id, comment],
                               handler = {?MODULE, activate_api},
                               created_on = Now})
  end,
  mnesia:activity(transaction, InitApiFun),
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
        RowU = Row#orders{id=bin_to_hexbin(Row#orders.id),
                          pin_template=try_to_hexbin(Row#orders.pin_template),
                          crypto_key=try_to_hexbin(Row#orders.crypto_key),
                          schedule=try_to_iso8601(Row#orders.schedule,
                                                  undefined),
                          status_on=try_to_iso8601(Row#orders.status_on,
                                                  undefined),
                          status_by=try_to_hexbin(Row#orders.status_by),
                          created_on=try_to_iso8601(Row#orders.created_on,
                                                   undefined),
                          created_by=try_to_hexbin(Row#orders.created_by),
                          modified_on=try_to_iso8601(Row#orders.modified_on,
                                                    undefined),
                          modified_by=try_to_hexbin(Row#orders.modified_by),
                          approved_on=try_to_iso8601(Row#orders.approved_on,
                                                    undefined),
                          approved_by=try_to_hexbin(Row#orders.approved_by),
                          activated_on=try_to_iso8601(Row#orders.activated_on,
                                                     undefined),
                          activated_by=try_to_hexbin(Row#orders.activated_by),
                          generate_starttime=try_to_iso8601(
                                               Row#orders.generate_starttime,
                                               undefined),
                          generate_endtime=try_to_iso8601(
                                             Row#orders.generate_endtime,
                                             undefined)},
        {record_to_tuplelist(RowU, record_info(fields, orders))}
    end,
    Rows
   ).

