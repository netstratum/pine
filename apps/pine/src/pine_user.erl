-module(pine_user).
-behaviour(gen_server).

-include("pine_mnesia.hrl").

-import(pine_mnesia, [create_table/2, read_conf/2]).
-import(pine_tools, [uuid/0, md5/1, timestamp_diff_seconds/2]).

-export([start_link/0, login/3, logout/3, validate/2]).
-export([init/1, handle_call/3, handle_cast/2,
        handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(Username, Password, Source) ->
  gen_server:call(?MODULE, {login, Username, Password, Source}).

logout(Username, Cookie, Source) ->
  gen_server:call(?MODULE, {logout, Username, Cookie, Source}).

validate(Cookie, Source) ->
  gen_server:call(?MODULE, {validate, Cookie, Source}).

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
      io:format("Here ~p and ~p ~n",
                [UserRecord, {Username, Password, Source}]),
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
      io:format("Here ~p and ~p ~n",
                [SessionRecord, {Username, Cookie, Source}]),
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
