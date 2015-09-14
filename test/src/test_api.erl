-module(test_api).
-export([login/3, logout/3]).

login(Url, Username, Password) ->
  RequestBody = test_tools:encode_json(
                  [{function, <<"identity.user.login">>},
                   {username, list_to_binary(Username)},
                   {password,
                    list_to_binary(
                      test_tools:bin_to_hexstr(erlang:md5(Password))
                     )
                   }]),
  case ibrowse:send_req(Url, [], post, RequestBody) of
    {ok, "200", _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      case lists:keyfind(<<"x-pine-token">>, 1, ResponseJson) of
        false ->
          {error, no_token};
        {_, TokenBin} ->
          {ok, binary_to_list(TokenBin)}
      end;
    {ok, StatusCode, _Headers, _ResponseBody} ->
      {error, StatusCode};
    {error, Reason} ->
      {error, Reason}
  end.

logout(Url, Username, Token) ->
  RequestBody = test_tools:encode_json(
                  [{function, <<"identity.user.logout">>},
                   {username, list_to_binary(Username)}]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, "200", _Headers, _Body} ->
      ok;
    {ok , StatusCode, _Headers, _Body} ->
      {error, StatusCode};
    {error, Reason} ->
      {error, Reason}
  end.
