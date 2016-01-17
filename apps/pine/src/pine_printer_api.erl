-module(pine_printer_api).
-author("Chaitanya Chalasani <cchalasani@me.com>").

-behaviour(gen_server).

%% Include mnesia tables
-include("pine_mnesia.hrl").

%% Import Functions
-import(pine_printer, [add/5,
                       modify/5,
                       list/2,
                       lock/3,
                       unlock/3,
                       retire/3]).
-import(pine_user, [validate/2]).
-import(pine_tools, [hexbin_to_bin/1,
                     try_find_map/2,
                     try_to_int/1,
                     bin_to_hexbin/1,
                     now_to_iso8601/1,
                     try_to_iso8601/1,
                     try_to_hexbin/1,
                     record_to_tuplelist/2]).

%% API functions
-export([start_link/0,
         add_api/1,
         modify_api/1,
         list_api/1,
         lock_api/1,
         unlock_api/1,
         retire_api/1]).

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

%%--------------------------------------------------------------------
%% @doc
%% Add a printer
%%
%% @spec add_api() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
add_api(#{http_token:=Cookie,
          http_source:=Source,
          name:=Name,
          location:=Location,
          crypto_key:=CryptoKey} = Maps) ->
  Notes = try_find_map(notes, Maps),
  case validate(Cookie, Source) of
    {ok, User} ->
      add(User, Name, Notes, Location, CryptoKey);
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% Modify a printer
%%
%% @spec modify_api() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
modify_api(#{http_token:=Cookie,
             http_source:=Source,
             id:=Id} = Maps) ->
  Notes = try_find_map(notes, Maps),
  Location = try_find_map(location, Maps),
  CryptoKey = try_find_map(crypto_key, Maps),
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      modify(User, IdBin, Notes, Location, CryptoKey);
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% List all printers
%%
%% @spec list_api() -> {ok, [{printers, Printers},
%%                           {total_pages, TotalPages}]
%%                   | {error, Error}
%% @end
%%--------------------------------------------------------------------
list_api(#{http_token:=Cookie,
           http_source:=Source,
           page_no:=PageNo,
           page_size:=PageSize}) ->
  PageNoInt = try_to_int(PageNo),
  PageNoSize = try_to_int(PageSize),
  case validate(Cookie, Source) of
    {ok, _User} ->
      case list(PageNoInt, PageNoSize) of
        {ok, Rows, TotalPages} ->
          PrintersTupleList = mk_printersTupleList(Rows),
          {ok, [{printers, PrintersTupleList},
                {total_pages, TotalPages}]};
        Other ->
          Other
      end;
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% Lock a printer
%%
%% @spec lock_api(Maps) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
lock_api(#{id:=Id,
           comment:=Comment,
           http_token:=Cookie,
           http_source:=Source}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      lock(User, IdBin, Comment);
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% Unlock a printer
%%
%% @spec unlock_api(Maps) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
unlock_api(#{id:=Id,
           comment:=Comment,
           http_token:=Cookie,
           http_source:=Source}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      unlock(User, IdBin, Comment);
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% Retire a printer
%%
%% @spec retire_api(Maps) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
retire_api(#{id:=Id,
           comment:=Comment,
           http_token:=Cookie,
           http_source:=Source}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      retire(User, IdBin, Comment);
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
    mnesia:write(#api_handlers{function = <<"order.printer.add">>,
                               arguments = [name, location, crypto_key],
                               handler = {?MODULE, add_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.printer.modify">>,
                               arguments = [id],
                               handler= {?MODULE, modify_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.printer.list">>,
                               arguments = [page_no, page_size],
                               handler = {?MODULE, list_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.printer.lock">>,
                               arguments = [id, comment],
                               handler = {?MODULE, lock_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.printer.unlock">>,
                               arguments = [id, comment],
                               handler = {?MODULE, unlock_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.printer.retire">>,
                               arguments = [id, comment],
                               handler = {?MODULE, retire_api},
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

mk_printersTupleList(Rows) ->
  lists:map(
    fun(Row) ->
        RowV2 = Row#printers{id = bin_to_hexbin(Row#printers.id),
                             crypto_key = bin_to_hexbin(
                                            Row#printers.crypto_key
                                           ),
                             created_on = now_to_iso8601(
                                            Row#printers.created_on
                                           ),
                             created_by = bin_to_hexbin(
                                            Row#printers.created_by
                                           ),
                             modified_on = try_to_iso8601(
                                             Row#printers.modified_on
                                            ),
                             modified_by = try_to_hexbin(
                                             Row#printers.modified_by
                                            )},
        {record_to_tuplelist(RowV2, record_info(fields, printers))}
    end,
    Rows
   ).
