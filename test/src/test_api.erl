-module(test_api).
-export([login/3, logout/3, open_pin/4,
         close_pin/4, burn_pin/4, change_password/5,
         add_user/6, modify_user/8, list_users/4, search_users/8,
         getdetails_user/3, list_roles/4, lock_user/4, unlock_user/4,
         retire_user/4, create_template/3, modify_template/3]).

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

add_user(Url, Token, Name, Email, Password, Role) ->
  RequestBody = test_tools:encode_json([{function, <<"identity.user.add">>},
                                        {name, list_to_binary(Name)},
                                        {email, list_to_binary(Email)},
                                        {password, list_to_binary(
                                                     test_tools:bin_to_hexstr(erlang:md5(Password))
                                                    )},
                                        {role, list_to_binary(Role)}]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

modify_user(Url, Token, Id, Name, Notes, Email, Expiry, RoleId) ->
  RequestBody = test_tools:encode_json(
                  removeEmptyBinary([
                               {function, <<"identity.user.modify">>},
                               {id, list_to_binary(Id)},
                               {name, list_to_binary(Name)},
                               {notes, list_to_binary(Notes)},
                               {email, list_to_binary(Email)},
                               {expiry, list_to_binary(Expiry)},
                               {role, list_to_binary(RoleId)}
                              ])),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

list_users(Url, Token, PageNo, PageSize) ->
  RequestBody = test_tools:encode_json([{function, <<"identity.user.list">>},
                                        {page_no, list_to_binary(PageNo)},
                                        {page_size, list_to_binary(PageSize)}]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

search_users(Url, Token, Name, Email, StartTS, EndTS, PageNo, PageSize) ->
  RequestBody = test_tools:encode_json(
                  removeEmptyBinary([{function, <<"identity.user.search">>},
                                     {name, list_to_binary(Name)},
                                     {email, list_to_binary(Email)},
                                     {start_ts, list_to_binary(StartTS)},
                                     {end_ts, list_to_binary(EndTS)},
                                     {page_no, list_to_binary(PageNo)},
                                     {page_size, list_to_binary(PageSize)}])),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

getdetails_user(Url, Token, Id) ->
  RequestBody = test_tools:encode_json([{function, <<"identity.user.getdetails">>},
                                        {id, list_to_binary(Id)}]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

list_roles(Url, Token, PageNo, PageSize) ->
  RequestBody = test_tools:encode_json([{function, <<"identity.role.list">>},
                                        {page_no, list_to_binary(PageNo)},
                                        {page_size, list_to_binary(PageSize)}]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

lock_user(Url, Token, Id, Comment) ->
  RequestBody = test_tools:encode_json([{function, <<"identity.user.lock">>},
                                        {id, list_to_binary(Id)},
                                        {comment, list_to_binary(Comment)}]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

unlock_user(Url, Token, Id, Comment) ->
  RequestBody = test_tools:encode_json([{function, <<"identity.user.unlock">>},
                                        {id, list_to_binary(Id)},
                                        {comment, list_to_binary(Comment)}]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

retire_user(Url, Token, Id, Comment) ->
  RequestBody = test_tools:encode_json([{function, <<"identity.user.retire">>},
                                        {id, list_to_binary(Id)},
                                        {comment, list_to_binary(Comment)}]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

create_template(Url, Token, ParamList) ->
  RequestBody = test_tools:encode_json([{function,
                                         <<"order.template.create">>}|ParamList]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

modify_template(Url, Token, ParamList) ->
  RequestBody = test_tools:encode_json([{function,
                                         <<"order.template.modify">>}|ParamList]),
  case ibrowse:send_req(Url, [{"x-pine-token", Token}], post, RequestBody) of
    {ok, StatusCode, _Headers, []} ->
      {ok, StatusCode};
    {ok, StatusCode, _Headers, ResponseBody} ->
      ResponseJson = test_tools:decode_json(ResponseBody),
      {ok, StatusCode, ResponseJson};
    {error, Reason} ->
      {error, Reason}
  end.

removeEmptyBinary(ParamList) ->
  [{Param, Value}||{Param, Value} <- ParamList, Value =/= <<>>].
