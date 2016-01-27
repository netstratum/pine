-module(pine_user_api).
-author("Chaitanya Chalasani <cchalasani@me.com>").

-behaviour(gen_server).

-include("pine_mnesia.hrl").

-import(pine_user, [login/3, logout/3, chpassword/5, adduser/8,
                    modifyuser/8, listusers/4, searchusers/8,
                    getuserinfo/3, listroles/4, lockuser/4,
                    unlockuser/4, retireuser/4]).
-import(pine_tools, [hexbin_to_bin/1, bin_to_hexbin/1,
                     try_find_map/2, try_to_int/1, to/2,
                     try_to_hexbin/1, try_to_hexbin_list/1,
                     try_find_map/3]).

-export([start_link/0, login_api/1, logout_api/1, chpassword_api/1,
         adduser_api/1, modifyuser_api/1, listusers_api/1, searchusers_api/1,
         getuserinfo_api/1, listroles_api/1, unlockuser_api/1, lockuser_api/1,
         retireuser_api/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login_api(#{username:=Username,
            password:=Password,
            http_source:=Source}) ->
  PasswordBin = hexbin_to_bin(Password),
  case login(Username, PasswordBin, Source) of
    {ok, Cookie} ->
      {ok, {'x-pine-token', bin_to_hexbin(Cookie)}};
    Error ->
      Error
  end.

logout_api(#{username:=Username,
             http_token:=Cookie,
             http_source:=Source}) ->
  logout(Username, Cookie, Source).

chpassword_api(#{username:=Username,
                 oldpassword:=OldPassword,
                 newpassword:=NewPassword,
                 http_token:=Cookie,
                 http_source:=Source}) ->
  OldPasswordBin = if OldPassword == [] -> <<>>;
                      true -> hexbin_to_bin(OldPassword)
                   end,
  NewPasswordBin = hexbin_to_bin(NewPassword),
  chpassword(Cookie, Source, Username, OldPasswordBin, NewPasswordBin).

adduser_api(#{name:=Name,
               email:=EmailAddress,
               password:=Password,
               role:=RoleId,
               http_token:=Cookie,
               http_source:=Source} = Maps) ->
  Notes = try_find_map(notes, Maps),
  Expiry = try_find_map(expiry, Maps, <<"90">>),
  PasswordBin = hexbin_to_bin(Password),
  RoleIdBin = hexbin_to_bin(RoleId),
  adduser(Cookie, Source, Name, Notes,
          EmailAddress, PasswordBin, RoleIdBin, Expiry).
modifyuser_api(#{id:=Id,
                 http_token:=Cookie,
                 http_source:=Source} = Maps) ->
  IdBin = hexbin_to_bin(Id),
  Name = try_find_map(name, Maps),
  Notes = try_find_map(notes, Maps),
  Email = try_find_map(email, Maps),
  Expiry = try_find_map(expiry, Maps),
  RoleId = try_find_map(role, Maps),
  modifyuser(Cookie, Source, IdBin, Name, Notes, Email, Expiry, RoleId).

listusers_api(#{page_no:=PageNo,
                page_size:=PageSize,
                http_token:=Cookie,
                http_source:=Source}) ->
  PageNoInt = try_to_int(PageNo),
  PageSizeInt = try_to_int(PageSize),
  case listusers(Cookie, Source, PageNoInt, PageSizeInt) of
    {ok, Rows, TotalPages} ->
      UserTupleList = mk_userTupleList(Rows),
      {ok, [{users, UserTupleList}, {total_pages, TotalPages}]};
    Other ->
      Other
  end.

searchusers_api(#{page_no:=PageNo,
                  page_size:=PageSize,
                  http_token:=Cookie,
                  http_source:=Source} = Maps) ->
  Name = try_find_map(name, Maps),
  Email = try_find_map(email, Maps),
  StartTS = try_find_map(start_ts, Maps),
  EndTS = try_find_map(end_ts, Maps),
  case searchusers(Cookie, Source, Name, Email, StartTS, EndTS, PageNo, PageSize) of
    {ok, Rows, TotalPages} ->
      UserTupleList = mk_userTupleList(Rows),
      {ok, [{users, UserTupleList}, {total_pages, TotalPages}]};
    Other ->
      Other
  end.

getuserinfo_api(#{id:=Id,
                  http_token:=Cookie,
                  http_source:=Source}) ->
  IdBin = hexbin_to_bin(Id),
  case getuserinfo(Cookie, Source, IdBin) of
    {ok, UserRecord} ->
      UserTupleList = mk_userTupleList([UserRecord]),
      io:format("UsesData is ~p~n", [UserTupleList]),
      {ok, [{users, UserTupleList}]};
    Other ->
      Other
  end.

listroles_api(#{page_no:=PageNo,
                page_size:=PageSize,
                http_token:=Cookie,
                http_source:=Source}) ->
  PageNoInt = try_to_int(PageNo),
  PageSizeInt = try_to_int(PageSize),
  case listroles(Cookie, Source, PageNoInt, PageSizeInt) of
    {ok, Rows, TotalPages} ->
      RoleTupleList = mk_roleTupleList(Rows),
      {ok, [{roles, RoleTupleList}, {total_pages, TotalPages}]};
    Other ->
      Other
  end.

lockuser_api(#{id:=Id,
               comment:=Comment,
               http_token:=Cookie,
               http_source:=Source}) ->
  IdBin = hexbin_to_bin(Id),
  lockuser(Cookie, Source, IdBin, Comment).

unlockuser_api(#{id:=Id,
                 comment:=Comment,
                 http_token:=Cookie,
                 http_source:=Source}) ->
  IdBin = hexbin_to_bin(Id),
  unlockuser(Cookie, Source, IdBin, Comment).

retireuser_api(#{id:=Id,
                 comment:=Comment,
                 http_token:=Cookie,
                 http_source:=Source}) ->
  IdBin = hexbin_to_bin(Id),
  retireuser(Cookie, Source, IdBin, Comment).

init([]) ->
  init_api(),
  {ok, []}.

init_api() ->
  Now = os:timestamp(),
  InitApiFun = fun() ->
    mnesia:write(#api_handlers{function = <<"identity.user.login">>,
                                     arguments = [username, password],
                                     handler = {?MODULE, login_api},
                                     created_on = Now}),
    mnesia:write(#api_handlers{function = <<"identity.user.logout">>,
                                     arguments = [username],
                                     handler = {?MODULE, logout_api},
                                     created_on = Now}),
    mnesia:write(#api_handlers{function = <<"identity.user.changePassword">>,
                                     arguments = [username, oldpassword,
                                                  newpassword],
                                     handler = {?MODULE, chpassword_api},
                                     created_on = Now}),
    mnesia:write(#api_handlers{function = <<"identity.user.add">>,
                                     arguments = [name, email, password,
                                                  role],
                                     handler = {?MODULE, adduser_api},
                                     created_on = Now}),
    mnesia:write(#api_handlers{function = <<"identity.user.modify">>,
                                     arguments = [id],
                                     handler = {?MODULE, modifyuser_api},
                                     created_on = Now}),
    mnesia:write(#api_handlers{function = <<"identity.user.list">>,
                                     arguments = [page_no, page_size],
                                     handler = {?MODULE, listusers_api},
                                     created_on = Now}),
    mnesia:write(#api_handlers{function = <<"identity.user.search">>,
                                     arguments = [page_no, page_size],
                                     handler = {?MODULE, searchusers_api},
                                     created_on = Now}),
    mnesia:write(#api_handlers{function = <<"identity.user.getdetails">>,
                                     arguments = [id],
                                     handler = {?MODULE, getuserinfo_api},
                                     created_on = Now}),
    mnesia:write(#api_handlers{function = <<"identity.role.list">>,
                                     arguments = [page_no, page_size],
                                     handler = {?MODULE, listroles_api},
                                     created_on = Now}),
    mnesia:write(#api_handlers{function = <<"identity.user.lock">>,
                                     arguments = [id, comment],
                                     handler = {?MODULE, lockuser_api},
                                     created_on = Now}),
    mnesia:write(#api_handlers{function = <<"identity.user.unlock">>,
                                     arguments = [id, comment],
                                     handler = {?MODULE, unlockuser_api},
                                     created_on = Now}),
    mnesia:write(#api_handlers{function = <<"identity.user.retire">>,
                                     arguments = [id, comment],
                                     handler = {?MODULE, retireuser_api},
                                     created_on = Now})
  end,
  mnesia:transaction(InitApiFun).

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

mk_userTupleList(UserRecords) ->
  TupleListFunction = fun(UserRecord) ->
    {[{id, try_to_hexbin(UserRecord#users.id)},
      {name, to(binary,UserRecord#users.name)},
      {notes, to(binary,UserRecord#users.notes)},
      {email, to(binary,UserRecord#users.email)},
      {access_expiry, to(binary,UserRecord#users.access_expiry)},
      {role, try_to_hexbin(UserRecord#users.role)},
      {role_expiry, to(binary,UserRecord#users.role_expiry)},
      {elevate_comment, to(binary,UserRecord#users.elevate_comment)},
      {role_revent, try_to_hexbin(UserRecord#users.role_revert)},
      {status, to(binary,UserRecord#users.status)},
      {status_comment, to(binary,UserRecord#users.status_comment)},
      {created_on, to(binary,UserRecord#users.created_on)},
      {created_by, try_to_hexbin(UserRecord#users.created_by)},
      {modified_on, to(binary,UserRecord#users.modified_on)},
      {modified_by, try_to_hexbin(UserRecord#users.modified_by)}]}
  end,
  lists:map(TupleListFunction, UserRecords).

mk_roleTupleList(RoleRecords) ->
  TupleListFunction = fun(RoleRecord) ->
    {[{id, try_to_hexbin(RoleRecord#roles.id)},
      {name, to(binary,RoleRecord#roles.name)},
      {notes, to(binary,RoleRecord#roles.notes)},
      {access_list, try_to_hexbin_list(RoleRecord#roles.access_list)},
      {status, to(binary,RoleRecord#roles.status)},
      {status_comment, to(binary,RoleRecord#roles.status_comment)},
      {created_on, to(binary,RoleRecord#roles.created_on)},
      {created_by, try_to_hexbin(RoleRecord#roles.created_by)},
      {modified_on, to(binary,RoleRecord#roles.modified_on)},
      {modified_by, try_to_hexbin(RoleRecord#roles.modified_by)}]}
   end,
  lists:map(TupleListFunction, RoleRecords).
