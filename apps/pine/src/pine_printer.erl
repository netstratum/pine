-module(pine_printer).
-author("Chaitanya Chalasani <cchalasani@me.com>").

-behaviour(gen_server).

%% Mnesia Table Definitions
-include("pine_mnesia.hrl").

%% Imports
-import(pine_mnesia, [create_table/2]).
-import(pine_tools, [update/2,
                     to/2,
                     uuid/0,
                     get_keysforpage/3]).

%% API functions
-export([start_link/0,
         add/5,
         modify/5,
         list/2,
         lock/3,
         unlock/3,
         retire/3]).

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
%% @spec add(User, Name, Notes, Location, CryptoKey) ->
%%                      ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
add(User, Name, Notes, Location, CryptoKey) ->
  gen_server:call(?MODULE, {add, User, Name, Notes, Location, CryptoKey}).

%%--------------------------------------------------------------------
%% @doc
%% Modify a printer
%%
%% @spec modify(User, Id, Notes, Location, CryptoKey) ->
%%                      ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
modify(User, Id, Notes, Location, CryptoKey) ->
  gen_server:call(?MODULE, {modify, User, Id, Notes, Location, CryptoKey}).

%%--------------------------------------------------------------------
%% @doc
%% List all printers
%%
%% @spec list(PageNo, PageSize) -> {ok, Rows, TotalPages} | {error, Error}
%% @end
%%--------------------------------------------------------------------
list(PageNo, PageSize) ->
  gen_server:call(?MODULE, {list, PageNo, PageSize}).

%%--------------------------------------------------------------------
%% @doc
%% Lock a printer
%%
%% @spec lock(User, Id, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
lock(User, Id, Comment) ->
  gen_server:call(?MODULE, {lock, User, Id, Comment}).

%%--------------------------------------------------------------------
%% @doc
%% Unlock a printer
%%
%% @spec unlock(User, Id, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
unlock(User, Id, Comment) ->
  gen_server:call(?MODULE, {unlock, User, Id, Comment}).

%%--------------------------------------------------------------------
%% @doc
%% Retire a printer
%%
%% @spec retire(User, Id, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
retire(User, Id, Comment) ->
  gen_server:call(?MODULE, {retire, User, Id, Comment}).

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
  init_tables(),
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
handle_call({add, User, Name, Notes, Location, CryptoKey},
            _From, State) ->
  Reply = handle_add(User, Name, Notes, Location, CryptoKey),
  {reply, Reply, State};
handle_call({modify, User, Id, Notes, Location, CryptoKey},
            _From, State) ->
  Reply = handle_modify(User, Id, Notes, Location, CryptoKey),
  {reply, Reply, State};
handle_call({list, PageNo, PageSize}, _From, State) ->
  Reply = handle_list(PageNo, PageSize),
  {reply, Reply, State};
handle_call({lock, User, Id, Comment}, _From, State) ->
  Reply = handle_lock(User, Id, Comment),
  {reply, Reply, State};
handle_call({unlock, User, Id, Comment}, _From, State) ->
  Reply = handle_unlock(User, Id, Comment),
  {reply, Reply, State};
handle_call({retire, User, Id, Comment}, _From, State) ->
  Reply = handle_retire(User, Id, Comment),
  {reply, Reply, State};
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

init_tables() ->
  create_table(printers, [{disc_copies, [node()]},
                          {attributes, record_info(fields, printers)},
                          {index, [name]}]),
  mnesia:wait_for_tables([printers], 2500).

handle_add(User, Name, Notes, Location, CryptoKey) ->
  Id = uuid(),
  Now = os:timestamp(),
  AddPrinterFun = fun() ->
    case mnesia:index_read(printers, Name, #printers.name) of
      [] ->
        mnesia:write(#printers{id=Id,
                               name=Name,
                               notes=Notes,
                               location=Location,
                               crypto_key=CryptoKey,
                               status=active,
                               created_on=Now,
                               created_by=User});
      _ ->
        {error, already_exists}
    end
  end,
  mnesia:activity(transaction, AddPrinterFun).

handle_modify(User, Id, Notes, Location, CryptoKey) ->
  Now = os:timestamp(),
  ModifyPrinterFun = fun() ->
    case mnesia:read(printers, Id) of
      [] ->
        {error, no_printer};
      [Printer] ->
        case Printer#printers.status of
          Status when Status == active;
                      Status == lock ->
            PrinterV2 = update(Printer, #printers{notes=Notes,
                                                  location=Location,
                                                  crypto_key=CryptoKey,
                                                  modified_on=Now,
                                                  modified_by=User}),
            mnesia:write(PrinterV2);
          _ ->
            {error, not_possible}
        end
    end
  end,
  mnesia:activity(transaction, ModifyPrinterFun).

handle_list(PageNo, PageSize) ->
  Keys = mnesia:dirty_all_keys(printers),
  case get_keysforpage(Keys, to(int, PageNo), to(int, PageSize)) of
    {error, Reason} ->
      {error, Reason};
    {ok, KeysSubList, TotalPages} ->
      Rows = lists:map(
               fun(Key) ->
                   [Rec] = mnesia:dirty_read(printers, Key),
                   Rec
               end,
               KeysSubList
              ),
      {ok, Rows, TotalPages}
  end.

handle_lock(User, Id, Comment) ->
  Now = os:timestamp(),
  LockTemplateFun = fun() ->
    case mnesia:read(printers, Id) of
      [] ->
        {error, no_printer};
      [Printer] ->
        case Printer#printers.status of
          active ->
            mnesia:write(Printer#printers{status=lock,
                                          status_comment=Comment,
                                          modified_on=Now,
                                          modified_by=User});
          _ ->
            {error, not_active}
        end
    end
  end,
  mnesia:activity(transaction, LockTemplateFun).

handle_unlock(User, Id, Comment) ->
  Now = os:timestamp(),
  UnlockTemplateFun = fun() ->
    case mnesia:read(printers, Id) of
      [] ->
        {error, no_printer};
      [Printer] ->
        case Printer#printers.status of
          lock ->
            mnesia:write(Printer#printers{status=active,
                                          status_comment=Comment,
                                          modified_on=Now,
                                          modified_by=User});
          _ ->
            {error, not_locked}
        end
    end
  end,
  mnesia:activity(transaction, UnlockTemplateFun).

handle_retire(User, Id, Comment) ->
  Now = os:timestamp(),
  RetireTemplateFun = fun() ->
    case mnesia:read(printers, Id) of
      [] ->
        {error, no_printer};
      [Printer] ->
        case Printer#printers.status of
          Status when Status == lock;
                      Status == active ->
            mnesia:write(Printer#printers{status=retire,
                                          status_comment=Comment,
                                          modified_on=Now,
                                          modified_by=User});
          _ ->
            {error, not_possible}
        end
    end
  end,
  mnesia:activity(transaction, RetireTemplateFun).
