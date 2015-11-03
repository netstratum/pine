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
                     did_it_happen/3]).

%% API functions
-export([start_link/0, login/3, logout/3, validate/2, chpassword/5,
         adduser/8, modifyuser/8, listusers/4, searchusers/8,
         getuserinfo/3]).

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
  Role = read_conf(role, <<"root">>),
  Now = os:timestamp(),
  RoleId = case mnesia:dirty_index_read(roles, Role, #roles.name) of
    [] ->
      RoleUuid = uuid(),
      mnesia:dirty_write(#roles{id=RoleUuid, name=Role, status=active,
                                created_on=Now, created_by = <<"sys">>}),
      RoleUuid;
    [RootRole] ->
      RootRole#roles.id
  end,
  User = read_conf(user, <<"root">>),
  case mnesia:dirty_index_read(users, User, #users.name) of
    [] ->
      UserUuid = uuid(),
      Password = read_conf(password, <<"pa55wdr00t">>),
      PassMd5 = md5(Password),
      mnesia:dirty_write(#users{id=UserUuid, name=User, role=RoleId,
                                password=PassMd5, status=active,
                                created_on=Now, created_by = <<"sys">>});
    _ ->
      ok
  end.

handle_login(Username, Password, Source) ->
  case mnesia:dirty_index_read(users, Username, #users.name) of
    [] ->
      {error, not_found};
    [UserRecord] ->
      if
        UserRecord#users.password == Password ->
          Cookie = uuid(),
          Now = os:timestamp(),
          Expiry = read_conf(session_timeout, 15),
          mnesia:dirty_write(#sessions{id=Cookie, user=Username,
                                      source=Source, expiry=Expiry,
                                      status=active, created_on=Now,
                                      modified_on=Now}),
          {ok, Cookie};
        true ->
          {error, not_authorized}
      end
  end.

handle_logout(Username, Cookie, Source) ->
  case mnesia:dirty_read(sessions, Cookie) of
    [] ->
      {error, not_found};
    [SessionRecord] ->
      if
        SessionRecord#sessions.user == Username andalso
            SessionRecord#sessions.source == Source->
          mnesia:dirty_delete(sessions, Cookie),
          ok;
        true ->
          {error, not_authorized}
      end
  end.

handle_validate(Cookie, Source) ->
  case mnesia:dirty_read(sessions, Cookie) of
    [] ->
      {error, not_found};
    [SessionRecord] ->
      if
        SessionRecord#sessions.source == Source ->
          Now = os:timestamp(),
          Then = SessionRecord#sessions.modified_on,
          ExpiryMin = SessionRecord#sessions.expiry,
          TimeDiffMin = timestamp_diff_seconds(Now, Then) div 60,
          if
            TimeDiffMin =< ExpiryMin ->
              {ok, SessionRecord#sessions.user};
            true ->
              mnesia:dirty_delete(sessions, Cookie),
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
  [UserRecord] = mnesia:dirty_index_read(users, Username, #users.name),
  if
    UserRecord#users.password == OldPassword;
    OldPassword =/= NewPassword ->
      Now = os:timestamp(),
      mnesia:dirty_write(UserRecord#users{password=NewPassword,
                                          modified_on=Now,
                                          modified_by=Username});
    true ->
      {error, wrong_password}
  end.

handle_chpassword_other(Username, Password, Requester) ->
  case mnesia:dirty_index_read(users, Username, #users.name) of
    [] ->
      {error, not_found};
    [UserRecord] ->
      if
        UserRecord#users.password == Password ->
          {error, wrong_password};
        true ->
          Now = os:timestamp(),
          mnesia:dirty_write(UserRecord#users{password=Password,
                                              modified_on=Now,
                                              modified_by=Requester})
      end
  end.

handle_adduser(Cookie, Source, Name, Notes,
               EmailAddress, Password, RoleId, Expiry) ->
  case handle_validate(Cookie, Source) of
    {error, Reason} ->
      {error, Reason};
    {ok, Requester} ->
      Now = os:timestamp(),
      case mnesia:dirty_index_read(users, Name, #users.name) of
        [] ->
          mnesia:dirty_write(#users{id=uuid(), name=Name, notes=Notes,
                                    email=EmailAddress, password=Password,
                                    access_expiry=Expiry, role=RoleId,
                                    status=active, created_on=Now,
                                    created_by=Requester});
        _ ->
          {error, user_exists}
      end
  end.

handle_modifyuser(Cookie, Source, Id, Name, Notes, Email, Expiry, RoleId) ->
  case handle_validate(Cookie, Source) of
    {error, Reason} ->
      {error, Reason};
    {ok, Requester} ->
      case mnesia:dirty_read(users, Id) of
        [] ->
          {error, no_user};
        [UserRecord] ->
          Now = os:timestamp(),
          UpdatedRecord = update_record(UserRecord, [{name, Name},
                                                     {notes, Notes},
                                                     {email, Email},
                                                     {access_expiry, Expiry},
                                                     {role, RoleId},
                                                     {modified_on, Now},
                                                     {modified_by, Requester}]),
          mnesia:dirty_write(UpdatedRecord),
          ok
      end
  end.

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
       case get_keysforpage(Keys, PageNo, PageSize) of
         {error, Reason} ->
           {error, Reason};
         {ok, KeysSublist, TotalPages} ->
            Rows = lists:map(
              fun(Key) -> mnesia:dirty_read(users, Key) end,
              KeysSublist
             ),
            {ok, Rows, TotalPages}
       end
  end.

get_keysforpage(Keys, _PageNo, PageSize) when PageSize < 0 ->
  Keys;
get_keysforpage(Keys, PageNo, PageSize) ->
  TotalPages = Keys div PageSize,
  if
    PageNo > TotalPages ->
      {error, invalid_page};
    true ->
      PageNoEnsure = if PageNo < 1 -> 1; true -> PageNo end,
      FirstPosition = (PageNoEnsure - 1) * PageSize + 1,
      {lists:sublist(Keys, FirstPosition, PageSize), TotalPages}
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
      case get_keysforpage(FilteredKeys, PageNo, PageSize) of
        {error, Reason} ->
          {error, Reason};
        {ok, KeysSublist, TotalPages} ->
          Rows = lists:map(
                   fun(Key) -> mnesia:dirty_read(users, Key) end,
                   KeysSublist
                  ),
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
filter_users_bool(UserRecord, [{created, {StartTS, EndTS}}|Filters]) ->
  UserCreatedOn = UserRecord#users.created_on,
  case did_it_happen(UserCreatedOn, StartTS, EndTS) of
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
          [_|[_|UserInfo]] = tuple_to_list(UserRecord),
          {ok, UserInfo}
      end
  end.
