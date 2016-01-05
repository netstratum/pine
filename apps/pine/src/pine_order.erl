-module(pine_order).

-behaviour(gen_server).

-include("pine_mnesia.hrl").

-import(pine_tools, [uuid/0,
                     update/2,
                     get_keysforpage/3,
                     to/2,
                     has_ic/2,
                     did_it_happen/3]).

%% API functions
-export([start_link/0,
         create/2,
         modify/2,
         search/7,
         lock/3,
         unlock/3,
         retire/3,
         approve/3,
         reject/3,
         activate/3]).

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
%% Create an order
%%
%% @spec create(User, Order) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
create(User, Order) ->
  gen_server:call(?MODULE, {create, User, Order}).

%%--------------------------------------------------------------------
%% @doc
%% Modify an order
%%
%% @spec modify(User, Order) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
modify(User, Order) ->
  gen_server:call(?MODULE, {modify, User, Order}).

%%--------------------------------------------------------------------
%% @doc
%% Search an order
%%
%% @spec search(Name, Notes, Label, StartTS, EndTS,
%%              PageNo, PageSize) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
search(Name, Notes, Label, StartTS, EndTS, PageNo, PageSize) ->
  gen_server:call(?MODULE, {search, Name, Notes, Label, StartTS,
                            EndTS, PageNo, PageSize}).

%%--------------------------------------------------------------------
%% @doc
%% Lock an order
%%
%% @spec lock(User, Id, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
lock(User, Id, Comment) ->
  gen_server:call(?MODULE, {lock, User, Id, Comment}).

%%--------------------------------------------------------------------
%% @doc
%% Unlock an order
%%
%% @spec unlock(User, Id, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
unlock(User, Id, Comment) ->
  gen_server:call(?MODULE, {unlock, User, Id, Comment}).

%%--------------------------------------------------------------------
%% @doc
%% Retire an order
%%
%% @spec retire(User, Id, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
retire(User, Id, Comment) ->
  gen_server:call(?MODULE, {retire, User, Id, Comment}).

%%--------------------------------------------------------------------
%% @doc
%% Approve an order
%%
%% @spec approve(User, Id, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
approve(User, Id, Comment) ->
  gen_server:call(?MODULE, {approve, User, Id, Comment}).

%%--------------------------------------------------------------------
%% @doc
%% Reject an order
%%
%% @spec reject(User, Id, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
reject(User, Id, Comment) ->
  gen_server:call(?MODULE, {reject, User, Id, Comment}).

%%--------------------------------------------------------------------
%% @doc
%% Activate an order
%%
%% @spec activate(User, Id, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
activate(User, Id, Comment) ->
  gen_server:call(?MODULE, {activate, User, Id, Comment}).

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
handle_call({create, User, Order}, _From, State) ->
  Reply = handle_create(User, Order),
  {reply, Reply, State};
handle_call({modify, User, Order}, _From, State) ->
  Reply = handle_modify(User, Order),
  {reply, Reply, State};
handle_call({search, Name, Notes, Label, StartTS,
             EndTS, PageNo, PageSize}, _From, State) ->
  Reply = handle_search(Name, Notes, Label, StartTS,
                        EndTS, PageNo, PageSize),
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
handle_call({approve, User, Id, Comment}, _From, State) ->
  Reply = handle_approve(User, Id, Comment),
  {reply, Reply, State};
handle_call({reject, User, Id, Comment}, _From, State) ->
  Reply = handle_reject(User, Id, Comment),
  {reply, Reply, State};
handle_call({activate, User, Id, Comment}, _From, State) ->
  Reply = handle_activate(User, Id, Comment),
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

handle_create(User, Order) when is_record(Order, orders) ->
  Id = uuid(),
  Now = os:timestamp(),
  CreateOrderFun = fun() ->
    case mnesia:index_read(orders,
                           Order#orders.name,
                           #orders.name) of
      [] ->
        mnesia:write(Order#orders{id=Id,
                                  status=create,
                                  created_on=Now,
                                  created_by=User});
      _ ->
        {error, already_exists}
    end
  end,
  mnesia:activity(transaction, CreateOrderFun);
handle_create(_User, _Order) ->
  {error, bad_record}.

handle_modify(User, Order) when is_record(Order, orders) ->
  Now = os:timestamp(),
  ModifyOrderFun = fun() ->
    case mnesia:read(orders, Order#orders.id) of
      [] ->
        {error, no_order};
      [OrderThen] ->
        case OrderThen#orders.status of
          Status when Status == active;
                      Status == lock;
                      Status == approve ->
            {error, order_approved};
          _ ->
          OrderNow = update(OrderThen, Order),
          mnesia:write(OrderNow#orders{modified_on=Now,
                                       modified_by=User})
        end
    end
  end,
  mnesia:activity(transaction, ModifyOrderFun);
handle_modify(_User, _Order) ->
  {error, bad_record}.

%% TODO: has to improve the search
handle_search(Name, Notes, Label, StartTS, EndTS, PageNo, PageSize) ->
  Keys = mnesia:dirty_all_keys(orders),
  FilteredKeys = filter_orders(Keys, [{name, Name},
                                      {notes, Notes},
                                      {label, Label},
                                      {created, {StartTS, EndTS}}]),
  case get_keysforpage(FilteredKeys, to(int, PageNo), to(ine, PageSize)) of
    {error, Reason} ->
      {error, Reason};
    {ok, KeysSubList, TotalPages} ->
      Rows = lists:map(
               fun(Key) ->
                   [Rec] = mnesia:dirty_read(orders, Key),
                   Rec
               end,
               KeysSubList
              ),
      {ok, Rows, TotalPages}
  end.

filter_orders(Keys, Filters) ->
  lists:filter(
    fun(Key) ->
        [OrderRecord] = mnesia:dirty_read(orders, Key),
        filter_orders_bool(OrderRecord, Filters)
    end,
    Keys
   ).

filter_orders_bool(OrderRecord, [{_Param, undefined}|Filters]) ->
  filter_orders_bool(OrderRecord, Filters);
filter_orders_bool(OrderRecord, [{name, Name}|Filters]) ->
  case has_ic(OrderRecord#orders.name, Name) of
    false ->
      filter_orders_bool(OrderRecord, Filters);
    true ->
      true
  end;
filter_orders_bool(OrderRecord, [{notes, Notes}|Filters]) ->
  case has_ic(OrderRecord#orders.notes, Notes) of
    false ->
      filter_orders_bool(OrderRecord, Filters);
    true ->
      true
  end;
filter_orders_bool(OrderRecord, [{created, {StartTS, EndTS}}|Filters])
    when StartTS == undefined, EndTS == undefined ->
  filter_orders_bool(OrderRecord, Filters);
filter_orders_bool(OrderRecord, [{created, {StartTS, EndTS}}|Filters]) ->
  OrderCreatedOn = OrderRecord#orders.created_on,
  CreatedSeconds = to(seconds, OrderCreatedOn),
  StartTSSeconds = to(seconds, StartTS),
  EndTSSeconds = to(seconds, EndTS),
  case did_it_happen(CreatedSeconds, StartTSSeconds, EndTSSeconds) of
    true ->
      true;
    _ ->
      filter_orders_bool(OrderRecord, Filters)
  end;
filter_orders_bool(_OrderRecord, []) ->
  false.

handle_lock(User, Id, Comment) ->
  Now = os:timestamp(),
  LockOrderFun = fun() ->
    case mnesia:dirty_read(order, Id) of
      [] ->
        {error, no_order};
      [OrderRecord] ->
        case OrderRecord#orders.status of
          active ->
            mnesia:write(OrderRecord#orders{status=lock,
                                            status_comments=Comment,
                                            status_on=Now,
                                            status_by=User});
          _ ->
            {error, not_active}
        end
    end
  end,
  mnesia:activity(transaction, LockOrderFun).

handle_unlock(User, Id, Comment) ->
  Now = os:timestamp(),
  UnlockOrderFun = fun() ->
    case mnesia:dirty_read(order, Id) of
      [] ->
        {error, no_order};
      [OrderRecord] ->
        case OrderRecord#orders.status of
          lock ->
            mnesia:write(OrderRecord#orders{status=active,
                                            status_comments=Comment,
                                            status_on=Now,
                                            status_by=User});
          _ ->
            {error, already_locked}
        end
    end
  end,
  mnesia:activity(transaction, UnlockOrderFun).

handle_retire(User, Id, Comment) ->
  Now = os:timestamp(),
  RetireOrderFun = fun() ->
    case mnesia:dirty_read(order, Id) of
      [] ->
        {error, no_order};
      [OrderRecord] ->
        case OrderRecord#orders.status of
          Order when Order == active;
                     Order == lock ->
            mnesia:write(OrderRecord#orders{status=retire,
                                            status_comments=Comment,
                                            status_on=Now,
                                            status_by=User});
          retire ->
            {error, no_order};
          _ ->
            {error, not_active}
        end
    end
  end,
  mnesia:activity(transaction, RetireOrderFun).

handle_approve(User, Id, Comment) ->
  Now = os:timestamp(),
  ApproveOrderFun = fun() ->
    case mnesia:dirty_read(order, Id) of
      [] ->
        {error, no_order};
      [OrderRecord] ->
        case OrderRecord#orders.status of
          create ->
            mnesia:write(OrderRecord#orders{status=approve,
                                            approve_comments=Comment,
                                            approved_on=Now,
                                            approved_by=User});
          approve ->
            {error, already_approved};
          _ ->
            {error, not_possible}
        end
    end
  end,
  mnesia:activity(transaction, ApproveOrderFun).

handle_reject(User, Id, Comment) ->
  Now = os:timestamp(),
  RejectOrderFun = fun() ->
    case mnesia:dirty_read(order, Id) of
      [] ->
        {error, no_order};
      [OrderRecord] ->
        case OrderRecord#orders.status of
          create ->
            mnesia:write(OrderRecord#orders{status=reject,
                                            status_comments=Comment,
                                            status_on=Now,
                                            status_by=User});
          approve ->
            {error, already_approved};
          reject ->
            {error, already_rejected};
          _ ->
            {error, not_possible}
        end
    end
  end,
  mnesia:activity(transaction, RejectOrderFun).

handle_activate(User, Id, Comment) ->
  Now = os:timestamp(),
  ActivateOrderFun = fun() ->
    case mnesia:dirty_read(order, Id) of
      [] ->
        {error, no_order};
      [OrderRecord] ->
        case OrderRecord#orders.status of
          approve ->
            mnesia:write(OrderRecord#orders{status=active,
                                            activate_comments=Comment,
                                            activated_on=Now,
                                            activated_by=User});
          active ->
            {error, already_active};
          _ ->
            {error, not_possible}
        end
    end
  end,
  mnesia:activity(transaction, ActivateOrderFun).

