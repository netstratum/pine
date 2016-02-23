-module(pine_role).

-behaviour(gen_server).

%% mnesia reads
-include("pine_mnesia.hrl").

%% Frequently used imports
-import(pine_tools, [get_keysforpage/3,
                     to/2,
                     update/2,
                     uuid/0]).
-import(pine_mnesia, [create_table/2,
                      read_conf/2]).

%% API functions
-export([start_link/0,
         listroles/2,
         listaccess/2,
         add/4,
         modify/4,
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

%%-----------------------------------------------------------------------%%
%% @doc
%% List Roles
%%
%% @spec listroles(PageNo, PageSize) ->
%%                 {ok, RolesInfoList} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
listroles(PageNo, PageSize) ->
  gen_server:call(?MODULE, {listroles, PageNo, PageSize}).

%%-----------------------------------------------------------------------%%
%% @doc
%% List Access
%%
%% @spec listaccess(PageNo, PageSize) ->
%%                 {ok, AccessInfoList} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
listaccess(PageNo, PageSize) ->
  gen_server:call(?MODULE, {listaccess, PageNo, PageSize}).

%%-----------------------------------------------------------------------%%
%% @doc
%% Add a role
%%
%% @spec add(User, Name, Notes, AccessList) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
add(User, Name, Notes, AccessList) ->
  gen_server:call(?MODULE, {add, User, Name, Notes, AccessList}).

%%-----------------------------------------------------------------------%%
%% @doc
%% Modify a role
%%
%% @spec modify(User, Id, Notes, AccessList) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
modify(User, Id, Notes, AccessList) ->
  gen_server:call(?MODULE, {modify, User, Id, Notes, AccessList}).

%%--------------------------------------------------------------------
%% @doc
%% Lock a Role
%%
%% @spec lock(User, Id, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
lock(User, Id, Comment) ->
  gen_server:call(?MODULE, {lock, User, Id, Comment}).

%%--------------------------------------------------------------------
%% @doc
%% Unlock a Role
%%
%% @spec unlock(User, Id, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
unlock(User, Id, Comment) ->
  gen_server:call(?MODULE, {unlock, User, Id, Comment}).

%%--------------------------------------------------------------------
%% @doc
%% Retire a Role
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
  init_data(),
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
handle_call({listroles, PageNo, PageSize}, _From, State) ->
  Reply = handle_listroles(PageNo, PageSize),
  {reply, Reply, State};
handle_call({listaccess, PageNo, PageSize}, _From, State) ->
  Reply = handle_listaccess(PageNo, PageSize),
  {reply, Reply, State};
handle_call({add, User, Name, Notes, AccessList}, _From, State) ->
  Reply = handle_add(User, Name, Notes, AccessList),
  {reply, Reply, State};
handle_call({modify, User, Id, Notes, AccessList}, _From , State) ->
  Reply = handle_modify(User, Id, Notes, AccessList),
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
  create_table(roles, [{disc_copies, [node()]},
                       {attributes, record_info(fields, roles)},
                       {index, [name]}]),
  mnesia:wait_for_tables([roles], 2500).

init_data() ->
  RoleName = read_conf(role, <<"root">>),
  RoleUuid = uuid(),
  Now = os:timestamp(),
  InitRoleFun = fun() ->
    case mnesia:index_read(roles, RoleName, #roles.name) of
      [] ->
        mnesia:write(#roles{id=RoleUuid, name=RoleName, status=active,
                                  created_on=Now}),
        RoleUuid;
      [RootRole] ->
        RootRole#roles.id
    end
  end,
  mnesia:activity(transaction, InitRoleFun).

handle_listroles(PageNo, PageSize) ->
  Keys = mnesia:dirty_all_keys(roles),
   case get_keysforpage(Keys, to(int, PageNo), to(int,PageSize)) of
     {error, Reason} ->
       {error, Reason};
     {ok, KeysSublist, TotalPages} ->
        Rows = lists:flatten(lists:map(
          fun(Key) -> mnesia:dirty_read(roles, Key) end,
          KeysSublist
         )),
        {ok, Rows, TotalPages}
   end.

handle_listaccess(PageNo, PageSize) ->
  Keys = mnesia:dirty_all_keys(access),
   case get_keysforpage(Keys, to(int, PageNo), to(int,PageSize)) of
     {error, Reason} ->
       {error, Reason};
     {ok, KeysSublist, TotalPages} ->
        Rows = lists:flatten(lists:map(
          fun(Key) -> mnesia:dirty_read(access, Key) end,
          KeysSublist
         )),
        {ok, Rows, TotalPages}
   end.

handle_add(User, Name, Notes, AccessList) ->
  Id = uuid(),
  Now = os:timestamp(),
  AddRoleFun = fun() ->
    case mnesia:index_read(roles, Name, #roles.name) of
      [] ->
        mnesia:write(#roles{id=Id,
                            name=Name,
                            notes=Notes,
                            access_list=AccessList,
                            status=active,
                            created_on=Now,
                            created_by=User
                            });
      _ ->
        {error, already_exists}
    end
  end,
  mnesia:activity(transaction, AddRoleFun).

handle_modify(User, Id, Notes, AccessList) ->
  Now = os:timestamp(),
  ModifyRoleFun = fun() ->
    case mnesia:read(roles, Id) of
      [] ->
        {error, no_role};
      [Role] ->
        case Role#roles.status of
          Status when Status == active;
                      Status == lock ->
            RoleV2 = update(Role, #roles{notes=Notes,
                                         access_list=AccessList,
                                         modified_on=Now,
                                         modified_by=User}),
            mnesia:write(RoleV2);
          _ ->
            {error, not_possible}
        end
    end
  end,
  mnesia:activity(transaction, ModifyRoleFun).

handle_lock(User, Id, Comment) ->
  Now = os:timestamp(),
  LockRoleFun = fun() ->
    case mnesia:read(roles, Id) of
      [] ->
        {error, no_role};
      [Role] ->
        case Role#roles.status of
          active ->
            mnesia:write(Role#roles{status=lock,
                                          status_comment=Comment,
                                          modified_on=Now,
                                          modified_by=User});
          _ ->
            {error, not_active}
        end
    end
  end,
  mnesia:activity(transaction, LockRoleFun).

handle_unlock(User, Id, Comment) ->
  Now = os:timestamp(),
  UnlockRoleFun = fun() ->
    case mnesia:read(roles, Id) of
      [] ->
        {error, no_role};
      [Role] ->
        case Role#roles.status of
          lock ->
            mnesia:write(Role#roles{status=active,
                                          status_comment=Comment,
                                          modified_on=Now,
                                          modified_by=User});
          _ ->
            {error, not_locked}
        end
    end
  end,
  mnesia:activity(transaction, UnlockRoleFun).

handle_retire(User, Id, Comment) ->
  Now = os:timestamp(),
  RetireRoleFun = fun() ->
    case mnesia:read(roles, Id) of
      [] ->
        {error, no_role};
      [Role] ->
        case Role#roles.status of
          Status when Status == lock;
                      Status == active ->
            mnesia:write(Role#roles{status=retire,
                                          status_comment=Comment,
                                          modified_on=Now,
                                          modified_by=User});
          _ ->
            {error, not_possible}
        end
    end
  end,
  mnesia:activity(transaction, RetireRoleFun).
