%%%=======================================================================%%%
%%% PIN Engine : User Management Module
%%%=======================================================================%%%
-module(pine_user).
-behaviour(gen_server).

%% Mnesia table definitions
-include("pine_mnesia.hrl").

%% Frequently used imports
-import(pine_mnesia, [create_table/2, read_conf/2]).
-import(pine_tools, [uuid/0, md5/1, timestamp_diff_seconds/2,
                     did_it_happen/3, to/2]).

%% API functions
-export([start_link/0, login/3, logout/3, validate/2, chpassword/5,
         adduser/8, modifyuser/8, listusers/4, searchusers/8,
         getuserinfo/3, listroles/4, lockuser/4, unlockuser/4,
         retireuser/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
        handle_info/2, terminate/2, code_change/3]).

%%=======================================================================%%
%% API functions
%%=======================================================================%%

%%-----------------------------------------------------------------------%%
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Reason}
%% @end

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-----------------------------------------------------------------------%%
%% @doc
%% User logs-in
%%
%% @spec login(Username, Password, Source) -> {ok, Token} | {error, Reason}
%% @end

login(Username, Password, Source) ->
  gen_server:call(?MODULE, {login, Username, Password, Source}).

%%-----------------------------------------------------------------------%%
%% @doc
%% User Logs-out
%%
%% @spec logout(Username, Cookie, Source) -> ok | {error, Reason}
%% @end

logout(Username, Cookie, Source) ->
  gen_server:call(?MODULE, {logout, Username, Cookie, Source}).

%%-----------------------------------------------------------------------%%
%% @doc
%% Validate user session
%%
%% @spec validate(Cookie, Source) -> ok | {error, Reason}
%% @end

validate(Cookie, Source) ->
  gen_server:call(?MODULE, {validate, Cookie, Source}).

%%-----------------------------------------------------------------------%%
%% @doc
%% Change Password
%%
%% @spec chpassword(Cookie, Source, Username, OldPassword, NewPassword) ->
%%                  ok | {error, Reason}
%% @end

chpassword(Cookie, Source, Username, OldPassword, NewPassword) ->
  gen_server:call(?MODULE, {chpassword, Cookie, Source,
                            Username, OldPassword, NewPassword}).
%%-----------------------------------------------------------------------%%
%% @doc
%% Add User
%%
%% @spec adduser(Cookie, Source, Name, Notes, EmailAddress, Password,
%%               RoleId, Expiry) -> ok | {error, Reason}
%% @end

adduser(Cookie, Source, Name, Notes, EmailAddress,
        Password, RoleId, Expiry) ->
  gen_server:call(?MODULE, {adduser, Cookie, Source, Name, Notes,
                            EmailAddress, Password, RoleId, Expiry}).

%%-----------------------------------------------------------------------%%
%% @doc
%% Modify User Info
%%
%% @spec modifyuser(Cookie, Source, Id, Name, Notes, Email, Expiry, RoleId)
%%              -> ok | {error, Reason}
%% @end

modifyuser(Cookie, Source, Id, Name, Notes, Email, Expiry, RoleId) ->
  gen_server:call(?MODULE, {modifyuser, Cookie, Source, Id, Name,
                            Notes, Email, Expiry, RoleId}).
%%-----------------------------------------------------------------------%%
%% @doc
%% List Users
%%
%% @spec listusers(Cookie, Source, PageNo, PageSize) ->
%%                 {ok, UsersInfoList} | {error, Reason}
%% @end

listusers(Cookie, Source, PageNo, PageSize) ->
  gen_server:call(?MODULE, {listusers, Cookie, Source, PageNo, PageSize}).

%%-----------------------------------------------------------------------%%
%% @doc
%% Search Users
%%
%% @spec searchusers(Cookie, Source, Name, Email, StartTS, EndTS, PageNo,
%%                   PageSize) -> {ok, UserIdList} | {error, Reason}
%% @end

searchusers(Cookie, Source, Name, Email, StartTS, EndTS, PageNo, PageSize) ->
  gen_server:call(?MODULE, {searchusers, Cookie, Source, Name, Email,
                            StartTS, EndTS, PageNo, PageSize}).

%%-----------------------------------------------------------------------%%
%% @doc
%% Get a User Info
%%
%% @spec getuserinfo(Cookie, Source, Id) -> {ok, UserInfo} | {error, Reason}
%% @end

getuserinfo(Cookie, Source, Id) ->
  gen_server:call(?MODULE, {getuserinfo, Cookie, Source, Id}).

%%-----------------------------------------------------------------------%%
%% @doc
%% Lock a user temporarily
%%
%% @spec lockuser(Cookie, Source, Id, Comment) -> ok | {error, Reason}
%% @end

lockuser(Cookie, Source, Id, Comment) ->
  gen_server:call(?MODULE, {lockuser, Cookie, Source, Id, Comment}).

%%-----------------------------------------------------------------------%%
%% @doc
%% UnLock a previously locked
%%
%% @spec unlockuser(Cookie, Source, Id, Comment) -> ok | {error, Reason}
%% @end

unlockuser(Cookie, Source, Id, Comment) ->
  gen_server:call(?MODULE, {unlockuser, Cookie, Source, Id, Comment}).

%%-----------------------------------------------------------------------%%
%% @doc
%% Retire User
%%
%% @spec retireuser(Cookie, Source, Id, Comment) ->
%%                 ok | {error, Reason}
%% @end

retireuser(Cookie, Source, Id, Comment) ->
  gen_server:call(?MODULE, {retireuser, Cookie, Source, Id, Comment}).

%%-----------------------------------------------------------------------%%
%% @doc
%% List Users
%%
%% @spec listusers(Cookie, Source, PageNo, PageSize) ->
%%                 {ok, UsersInfoList} | {error, Reason}
%% @end

listroles(Cookie, Source, PageNo, PageSize) ->
  gen_server:call(?MODULE, {listroles, Cookie, Source, PageNo, PageSize}).

%%=======================================================================%%
%% gen_server callbacksf
%%=======================================================================%%
init([]) ->
  init_tables(),
  init_data(),
  {ok, ok}.

handle_call({login, Username, Password, Source}, _From, State) ->
  Reply = handle_login(Username, Password, Source),
  {reply, Reply, State};
handle_call({logout, Username, Cookie, Source}, _From, State) ->
  Reply = handle_logout(Username, Cookie, Source),
  {reply, Reply, State};
handle_call({validate, Cookie, Source}, _From, State) ->
  Reply = handle_validate(Cookie, Source),
  {reply, Reply, State};
handle_call({chpassword, Cookie, Source, Username, OldPassword, NewPassword},
            _From, State) ->
  Reply = handle_chpassword(Cookie, Source, Username, OldPassword, NewPassword),
  {reply, Reply, State};
handle_call({adduser, Cookie, Source, Name, Notes, EmailAddress, Password,
             RoleId, Expiry}, _From, State) ->
  Reply = handle_adduser(Cookie, Source, Name, Notes, EmailAddress, Password,
                         RoleId, Expiry),
  {reply, Reply, State};
handle_call({modifyuser, Cookie, Source, Id, Name, Notes, Email, Expiry, RoleId},
            _From, State) ->
  Reply = handle_modifyuser(Cookie, Source, Id, Name, Notes, Email, Expiry, RoleId),
  {reply, Reply, State};
handle_call({listusers, Cookie, Source, PageNo, PageSize}, _From , State) ->
  Reply = handle_listusers(Cookie, Source, PageNo, PageSize),
  {reply, Reply, State};
handle_call({searchusers, Cookie, Source, Name, Email, StartTS, EndTS, PageNo,
             PageSize}, _From, State) ->
  Reply = handle_searchuser(Cookie, Source, Name, Email, StartTS, EndTS, PageNo,
                           PageSize),
  {reply, Reply, State};
handle_call({getuserinfo, Cookie, Source, Id}, _From, State) ->
  Reply = handle_getuserinfo(Cookie, Source, Id),
  {reply, Reply, State};
handle_call({listroles, Cookie, Source, PageNo, PageSize}, _From, State) ->
  Reply = handle_listroles(Cookie, Source, PageNo, PageSize),
  {reply, Reply, State};
handle_call({lockuser, Cookie, Source, Id, Comment}, _From, State) ->
  Reply = handle_lockuser(Cookie, Source, Id, Comment),
  {reply, Reply, State};
handle_call({unlockuser, Cookie, Source, Id, Comment}, _From, State) ->
  Reply = handle_unlockuser(Cookie, Source, Id, Comment),
  {reply, Reply, State};
handle_call({retireuser, Cookie, Source, Id, Comment}, _From, State) ->
  Reply = handle_retireuser(Cookie, Source, Id, Comment),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%=======================================================================%%
%% Internal Functions
%%=======================================================================%%
init_tables() ->
  lists:map(
    fun({Table, Options}) -> create_table(Table, Options) end,
    [{access,[{disc_copies, [node()]},{attributes, record_info(fields, access)}]},
     {roles, [{disc_copies, [node()]},{attributes, record_info(fields, roles)},
              {index, [name]}]},
     {users, [{disc_copies, [node()]},{attributes, record_info(fields, users)},
              {index, [name]}]},
     {sessions, [{attributes, record_info(fields, sessions)},
                {index, [user]}]},
     {session_log, [{disc_copies, [node()]},
                    {attributes, record_info(fields, session_log)}]},
     {access_log, [{disc_copies, [node()]},
                    {attributes, record_info(fields, access_log)}]}]
    ),
  mnesia:wait_for_tables([access, roles, users, sessions, session_log,
                          access_log], 2500).

init_data() ->
  RoleId = init_role(),
  init_user(RoleId).

init_role() ->
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

init_user(RoleId) ->
  User = read_conf(user, <<"root">>),
  UserUuid = uuid(),
  Now = os:timestamp(),
  Password = read_conf(password, <<"pa55wdr00t">>),
  PassMd5 = md5(Password),
  InitUserFun = fun() ->
    case mnesia:index_read(users, User, #users.name) of
      [] ->
        mnesia:write(#users{id=UserUuid, name=User, role=RoleId,
                                  password=PassMd5, status=active,
                                  created_on=Now});
      _ ->
        ok
    end
  end,
  mnesia:activity(transaction, InitUserFun).

handle_login(Username, Password, Source) ->
  Cookie = uuid(),
  Now = os:timestamp(),
  Expiry = read_conf(session_timeout, 15),
  LoginSessionFun = fun() ->
    case mnesia:index_read(users, Username, #users.name) of
      [] ->
        {error, not_found};
      [UserRecord] ->
        if
          UserRecord#users.password == Password,
          UserRecord#users.status == active ->
            UserId = UserRecord#users.id,
            mnesia:write(#sessions{id=Cookie, user=UserId,
                                        source=Source, expiry=Expiry,
                                        status=active, created_on=Now,
                                        modified_on=Now}),
            {ok, Cookie};
          true ->
            {error, not_authorized}
        end
    end
  end,
  mnesia:activity(transaction, LoginSessionFun).

handle_logout(Username, Cookie, Source) ->
  LogoutSessionFun = fun() ->
    case mnesia:read(sessions, Cookie) of
      [] ->
        {error, not_found};
      [SessionRecord] ->
        if
          SessionRecord#sessions.user == Username andalso
              SessionRecord#sessions.source == Source->
            mnesia:delete(sessions, Cookie);
          true ->
            {error, not_authorized}
        end
    end
  end,
  mnesia:activity(transaction, LogoutSessionFun).

handle_validate(Cookie, Source) ->
  case mnesia:dirty_read(sessions, Cookie) of
    [] ->
      {error, not_found};
    [SessionRecord] ->
      [UserRecord] = mnesia:dirty_read(users, SessionRecord#sessions.user),
      if
        SessionRecord#sessions.source == Source,
        UserRecord#users.status == active ->
          Now = os:timestamp(),
          Then = SessionRecord#sessions.modified_on,
          ExpiryMin = SessionRecord#sessions.expiry,
          TimeDiffMin = timestamp_diff_seconds(Now, Then) div 60,
          if
            TimeDiffMin =< ExpiryMin ->
              {ok, SessionRecord#sessions.user};
            true ->
              handle_logout(UserRecord#users.name, Cookie, Source),
              {error, session_expired}
          end;
        true ->
          {error, not_authorized}
      end
  end.

handle_chpassword(Cookie, Source, Username, OldPassword, NewPassword) ->
  case handle_validate(Cookie, Source) of
    {error, Reason} ->
      {error, Reason};
    {ok, Username} ->
      handle_chpassword_self(Username, OldPassword, NewPassword);
    {ok, Requester} ->
      handle_chpassword_other(Username, NewPassword, Requester)
  end.

handle_chpassword_self(Username, OldPassword, NewPassword) ->
  Now = os:timestamp(),
  ChPasswordSelfFun = fun() ->
    [UserRecord] = mnesia:index_read(users, Username, #users.name),
    if
      UserRecord#users.password == OldPassword;
      OldPassword =/= NewPassword ->
        mnesia:write(UserRecord#users{password=NewPassword,
                                            modified_on=Now,
                                            modified_by=Username});
      true ->
        {error, wrong_password}
    end
  end,
  mnesia:activity(transaction, ChPasswordSelfFun).

handle_chpassword_other(Username, Password, Requester) ->
  Now = os:timestamp(),
  ChPasswordOther = fun() ->
    case mnesia:index_read(users, Username, #users.name) of
      [] ->
        {error, not_found};
      [UserRecord] ->
        if
          UserRecord#users.password == Password ->
            {error, wrong_password};
          true ->
            mnesia:write(UserRecord#users{password=Password,
                                                modified_on=Now,
                                                modified_by=Requester})
        end
    end
  end,
  mnesia:activity(transaction, ChPasswordOther).

handle_adduser(Cookie, Source, Name, Notes,
               EmailAddress, Password, RoleId, Expiry) ->
  case handle_validate(Cookie, Source) of
    {error, Reason} ->
      {error, Reason};
    {ok, Requester} ->
      Now = os:timestamp(),
      AddUserFun = fun() ->
        case mnesia:index_read(users, Name, #users.name) of
          [] ->
            case mnesia:read(roles, RoleId) of
              [] ->
                {error, invalid_role};
              _ ->
                mnesia:write(#users{id=uuid(), name=Name, notes=Notes,
                                      email=EmailAddress, password=Password,
                                      access_expiry=Expiry, role=RoleId,
                                      status=active, created_on=Now,
                                      created_by=Requester})
            end;
          _ ->
            {error, user_exists}
        end
      end,
      mnesia:activity(transaction, AddUserFun)
  end.

handle_modifyuser(Cookie, Source, Id, Name, Notes, Email, Expiry, RoleId) ->
  case handle_validate(Cookie, Source) of
    {error, Reason} ->
      {error, Reason};
    {ok, Requester} ->
      Now = os:timestamp(),
      ModifyUserFun = fun() ->
        case mnesia:read(users, Id) of
          [] ->
            {error, no_user};
          [UserRecord] ->
            UpdatedRecord = update_record(UserRecord, [{name, Name},
                                                       {notes, Notes},
                                                       {email, Email},
                                                       {access_expiry, Expiry},
                                                       {role, RoleId},
                                                       {modified_on, Now},
                                                       {modified_by, Requester}]),
            mnesia:write(UpdatedRecord)
        end
      end,
      mnesia:activity(transaction, ModifyUserFun)
  end.

update_record(UserRecord, []) ->
  UserRecord;
update_record(UserRecord, [ParamValue|ParamList]) ->
  NewUserRecord = update_record_param(UserRecord, ParamValue),
  update_record(NewUserRecord, ParamList).

update_record_param(UserRecord, {_ParamName, undefined}) ->
  UserRecord;
update_record_param(UserRecord, {name, Name}) ->
  UserRecord#users{name=Name};
update_record_param(UserRecord, {notes, Notes}) ->
  UserRecord#users{notes=Notes};
update_record_param(UserRecord, {email, Email}) ->
  UserRecord#users{email=Email};
update_record_param(UserRecord, {access_expiry, Expiry}) ->
  UserRecord#users{access_expiry=Expiry};
update_record_param(UserRecord, {role, RoleId}) ->
  UserRecord#users{role=RoleId};
update_record_param(UserRecord, {modified_on, Now}) ->
  UserRecord#users{modified_on=Now};
update_record_param(UserRecord, {modified_by, Requester}) ->
  UserRecord#users{modified_by=Requester}.

handle_listusers(Cookie, Source, PageNo, PageSize) ->
  case handle_validate(Cookie, Source) of
    {error, Reason} ->
      {error, Reason};
    {ok, _Requester} ->
      Keys = mnesia:dirty_all_keys(users),
       case get_keysforpage(Keys, to(int,PageNo), to(int, PageSize)) of
         {error, Reason} ->
           {error, Reason};
         {ok, KeysSublist, TotalPages} ->
            Rows = lists:flatten(lists:map(
              fun(Key) -> mnesia:dirty_read(users, Key) end,
              KeysSublist
             )),
            {ok, Rows, TotalPages}
       end
  end.

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

handle_searchuser(Cookie, Source, Name, Email, StartTS, EndTS,
                  PageNo, PageSize) ->
  case handle_validate(Cookie, Source) of
    {error, Reason} ->
      {error, Reason};
    {ok, _Requested} ->
      Keys = mnesia:dirty_all_keys(users),
      FilteredKeys = filter_users(Keys, [{name, Name}, {email, Email},
                                         {created, {StartTS, EndTS}}]),
      case get_keysforpage(FilteredKeys, to(int, PageNo), to(int, PageSize)) of
        {error, Reason} ->
          {error, Reason};
        {ok, KeysSublist, TotalPages} ->
          Rows = lists:flatten(lists:map(
                   fun(Key) -> mnesia:dirty_read(users, Key) end,
                   KeysSublist
                  )),
          {ok, Rows, TotalPages}
      end
  end.

filter_users(Keys, Filters) ->
  lists:filter(
    fun(Key) ->
        [UserRecord] = mnesia:dirty_read(users, Key),
        filter_users_bool(UserRecord, Filters)
    end,
    Keys
   ).

filter_users_bool(UserRecord, [{_Param, undefined}|Filters]) ->
  filter_users_bool(UserRecord, Filters);
filter_users_bool(UserRecord, [{name, Name}|Filters]) ->
  if
    UserRecord#users.name == Name ->
      true;
    true ->
      filter_users_bool(UserRecord, Filters)
  end;
filter_users_bool(UserRecord, [{email, Email}|Filters]) ->
  if
    UserRecord#users.email == Email ->
      true;
    true ->
      filter_users_bool(UserRecord, Filters)
  end;
filter_users_bool(UserRecord, [{created, {StartTS, EndTS}}|Filters])
    when StartTS == undefined, EndTS == undefined ->
  filter_users_bool(UserRecord, Filters);
filter_users_bool(UserRecord, [{created, {StartTS, EndTS}}|Filters]) ->
  UserCreatedOn = UserRecord#users.created_on,
  CreatedSeconds = to(seconds, UserCreatedOn),
  StartTSSeconds = to(seconds, StartTS),
  EndTSSeconds = to(seconds, EndTS),
  io:format("Times here are ~p, ~p and ~p~n", [UserCreatedOn,
                                               StartTS,
                                               EndTS]),
  io:format("Time search here is ~p, ~p, ~p~n", [CreatedSeconds,
                                                 StartTSSeconds,
                                                 EndTSSeconds]),
  case did_it_happen(CreatedSeconds, StartTSSeconds, EndTSSeconds) of
    true ->
      true;
    _ ->
      filter_users_bool(UserRecord, Filters)
  end;
filter_users_bool(_UserRecord, []) ->
  false.

handle_getuserinfo(Cookie, Source, Id) ->
  case handle_validate(Cookie, Source) of
    {error, Reason} ->
      {error, Reason};
    {ok, _Requester} ->
      case mnesia:dirty_read(users, Id) of
        [] ->
          {error, no_user};
        [UserRecord] ->
          {ok, UserRecord}
      end
  end.

handle_listroles(Cookie, Source, PageNo, PageSize) ->
  case handle_validate(Cookie, Source) of
    {error, Reason} ->
      {error, Reason};
    {ok, _Requester} ->
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
       end
  end.

handle_lockuser(Cookie, Source, Id, Comment) ->
  case handle_validate(Cookie, Source) of
    {error, Reason} ->
      {error, Reason};
    {ok, Requester} ->
      Now = os:timestamp(),
      LockUserFun = fun() ->
        case mnesia:read(users, Id) of
          [] ->
            {error, no_user};
          [UserRecord] ->
            case UserRecord#users.status of
              Status when Status =/= lock, Status =/= retire ->
                mnesia:write(UserRecord#users{status=lock,
                                                    status_comment=Comment,
                                                    modified_on=Now,
                                                    modified_by=Requester});
              lock ->
                {error, already_locked};
              retire ->
                {error, no_user}
            end
        end
      end,
      mnesia:activity(transaction, LockUserFun)
  end.

handle_unlockuser(Cookie, Source, Id, Comment) ->
  case handle_validate(Cookie, Source) of
    {error, Reason} ->
      {error, Reason};
    {ok, Requester} ->
      Now = os:timestamp(),
      UnlockUserFun = fun() ->
        case mnesia:read(users, Id) of
          [] ->
            {error, no_user};
          [UserRecord] ->
            case UserRecord#users.status of
              Status when Status =/= active, Status =/= retire ->
                mnesia:write(UserRecord#users{status=active,
                                                    status_comment=Comment,
                                                    modified_on=Now,
                                                    modified_by=Requester});
              active ->
                {error, user_active};
              retire ->
                {error, no_user}
            end
        end
      end,
      mnesia:activity(transaction, UnlockUserFun)
  end.

handle_retireuser(Cookie, Source, Id, Comment) ->
  case handle_validate(Cookie, Source) of
    {error, Reason} ->
      {error, Reason};
    {ok, Requester} ->
      Now = os:timestamp(),
      RetireUserFun = fun() ->
        case mnesia:read(users, Id) of
          [] ->
            {error, no_user};
          [UserRecord] ->
            case UserRecord#users.status of
              retire ->
                {error, no_user};
              _Status ->
                mnesia:write(UserRecord#users{status=retire,
                                                    status_comment=Comment,
                                                    modified_on=Now,
                                                    modified_by=Requester})
            end
        end
      end,
      mnesia:activity(transaction, RetireUserFun)
  end.
