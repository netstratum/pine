-module(pine_user_api).
-behaviour(gen_server).

-include("pine_mnesia.hrl").

-import(pine_user, [login/3, logout/3, chpassword/5, adduser/8,
                    modifyuser/8, listusers/4, searchusers/8,
                    getuserinfo/3]).
-import(pine_tools, [hexbin_to_bin/1, bin_to_hexbin/1,
                     try_find_map/2]).

-export([start_link/0, login_api/1, logout_api/1, chpassword_api/1,
         adduser_api/1, modifyuser_api/1, listusers_api/1, searchusers_api/1,
         getuserinfo_api/1]).
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
  io:format("chpassword_api triggered~n"),
  chpassword(Cookie, Source, Username, OldPasswordBin, NewPasswordBin).

adduser_api(#{name:=Name,
               email:=EmailAddress,
               password:=Password,
               role:=RoleId,
               http_token:=Cookie,
               http_source:=Source} = Maps) ->
  Notes = try_find_map(notes, Maps),
  Expiry = try_find_map(expiry, Maps),
  PasswordBin = hexbin_to_bin(Password),
  adduser(Cookie, Source, Name, Notes,
          EmailAddress, PasswordBin, RoleId, Expiry).
modifyuser_api(#{id:=Id,
                 http_token:=Cookie,
                 http_source:=Source} = Maps) ->
  Name = try_find_map(name, Maps),
  Notes = try_find_map(notes, Maps),
  Email = try_find_map(email, Maps),
  Expiry = try_find_map(expiry, Maps),
  RoleId = try_find_map(role, Maps),
  modifyuser(Cookie, Source, Id, Name, Notes, Email, Expiry, RoleId).

listusers_api(#{page_no:=PageNo,
                page_size:=PageSize,
                http_token:=Cookie,
                http_source:=Source}) ->
  listusers(Cookie, Source, PageNo, PageSize).

searchusers_api(#{page_no:=PageNo,
                  page_size:=PageSize,
                  http_token:=Cookie,
                  http_source:=Source} = Maps) ->
  Name = try_find_map(name, Maps),
  Email = try_find_map(email, Maps),
  StartTS = try_find_map(start_ts, Maps),
  EndTS = try_find_map(end_ts, Maps),
  searchusers(Cookie, Source, Name, Email, StartTS, EndTS, PageNo, PageSize).

getuserinfo_api(#{id:=Id,
                  http_token:=Cookie,
                  http_source:=Source}) ->
  getuserinfo(Cookie, Source, Id).

init([]) ->
  init_api(),
  {ok, []}.

init_api() ->
  Now = os:timestamp(),
  mnesia:dirty_write(#api_handlers{function = <<"identity.user.login">>,
                                   arguments = [username, password],
                                   handler = {?MODULE, login_api},
                                   created_on = Now}),
  mnesia:dirty_write(#api_handlers{function = <<"identity.user.logout">>,
                                   arguments = [username],
                                   handler = {?MODULE, logout_api},
                                   created_on = Now}),
  mnesia:dirty_write(#api_handlers{function = <<"identity.user.changePassword">>,
                                   arguments = [username, oldpassword,
                                                newpassword],
                                   handler = {?MODULE, chpassword_api},
                                   created_on = Now}),
  mnesia:dirty_write(#api_handlers{function = <<"identity.user.add">>,
                                   arguments = [name, email, password,
                                                role],
                                   handler = {?MODULE, adduser_api},
                                   created_on = Now}),
  mnesia:dirty_write(#api_handlers{function = <<"identity.user.modify">>,
                                   arguments = [id],
                                   handler = {?MODULE, modifyuser_api},
                                   created_on = Now}),
  mnesia:dirty_write(#api_handlers{function = <<"identity.user.list">>,
                                   arguments = [page_no, page_size],
                                   handler = {?MODULE, listusers_api},
                                   created_on = Now}),
  mnesia:dirty_write(#api_handlers{function = <<"identity.user.search">>,
                                   arguments = [page_no, page_size],
                                   handler = {?MODULE, searchusers_api},
                                   created_on = Now}),
  mnesia:dirty_write(#api_handlers{function = <<"identity.user.getdetails">>,
                                   arguments = [id],
                                   handler = {?MODULE, getuserinfo_api},
                                   created_on = Now}).

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
