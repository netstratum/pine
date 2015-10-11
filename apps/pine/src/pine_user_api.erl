-module(pine_user_api).
-behaviour(gen_server).

-include("pine_mnesia.hrl").

-import(pine_user, [login/3, logout/3]).
-import(pine_tools, [hexbin_to_bin/1, bin_to_hexbin/1]).

-export([start_link/0, login_api/1, logout_api/1]).
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
