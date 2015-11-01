-module(test_api).
-export([login/3, logout/3, open_pin/4,
         close_pin/4, burn_pin/4, change_password/5]).

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
    {ok, StatusCode, _Headers, ResponseBody} ->
      {error, {StatusCode, ResponseBody}};
    {error, Reason} ->
      {error, Reason}
  end.

logout(Url, Token, Username) ->
  RequestBody = test_tools:encode_json(
                  [{function, <<"identity.user.logout">>},
                   {username, list_to_binary(Username)}]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, "200", _Headers, _Body} ->
      ok;
    {ok , StatusCode, _Headers, Body} ->
      {error, {StatusCode, Body}};
    {error, Reason} ->
      {error, Reason}
  end.

open_pin(Url, Token, Pin, EndUser) ->
  RequestBody = test_tools:encode_json([{function, <<"pin.open">>},
                                        {pin, list_to_binary(Pin)},
                                        {opened_by, list_to_binary(EndUser)}]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

close_pin(Url, Token, Seq, EndUser) ->
  RequestBody = test_tools:encode_json([{function, <<"pin.close">>},
                                        {seq, list_to_binary(Seq)},
                                        {opened_by, list_to_binary(EndUser)}]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

burn_pin(Url, Token, Seq, EndUser) ->
  RequestBody = test_tools:encode_json([{function, <<"pin.burn">>},
                                        {seq, list_to_binary(Seq)},
                                        {opened_by, list_to_binary(EndUser)}]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

change_password(Url, Token, Username, OldPassword, NewPassword) ->
  RequestBody = test_tools:encode_json([
                  {function, <<"identity.user.changePassword">>},
                  {username, list_to_binary(Username)},
                  {oldpassword,
                   list_to_binary(
                      test_tools:bin_to_hexstr(erlang:md5(OldPassword))
                    )
                  },
                  {newpassword,
                   list_to_binary(
                     test_tools:bin_to_hexstr(erlang:md5(NewPassword))
                    )
                  }]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

