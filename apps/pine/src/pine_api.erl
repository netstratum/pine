-module(pine_api).

-import(pine_tools, [encode_json/1, decode_json/1, hexstr_to_bin/1, 
                     bin_to_hexstr/1]).

-export([init/2]).

-include("pine_mnesia.hrl").

init(Req, Opts) ->
  Method = cowboy_req:method(Req),
  HasBody = cowboy_req:has_body(Req),
  Res = handle_post(Method, HasBody, Req),
  {ok, Res, Opts}.

handle_post(<<"POST">>, true, Req) ->
  {ok, Arguments, Req2} = cowboy_req:body_qs(Req),
  Token = cowboy_req:header(<<"x-pine-token">>, Req2),
  {Source, _Port} = cowboy_req:peer(Req2),
  case catch handle_command(Arguments, Token, Source) of
    {'EXIT', Reason} ->
      io:format("Exception ~p~n", [Reason]),
      cowboy_req:reply(500, Req);
    {StatusCode, []} ->
      cowboy_req:reply(StatusCode, [], [], Req2);
    {StatusCode, JsonResponse} ->
      cowboy_req:reply(StatusCode, 
             [{<<"content-type">>, <<"application/json; charset=utf-8">>}],
                       JsonResponse, Req2)
  end;
handle_post(<<"POST">>, false, Req) ->
  cowboy_req:reply(400, [], encode_json({failed_reason, <<"Missing body.">>}), Req);
handle_post(_, _, Req) ->
  cowboy_req:reply(405, Req).

handle_command([{Arguments,_True}], Token, Source) ->
  ArgumentsErl = decode_json(Arguments),
  case lists:keyfind(<<"function">>, 1, ArgumentsErl) of
    false ->
      {400, encode_json({failed_reason, <<"missing function name">>})};
    {_, FunctionName} ->
      handle_command(FunctionName, ArgumentsErl, Token, Source)
  end.

handle_command(<<"identity.user.login">>, Arguments, _Token, Source) ->
  io:format("identity.user.login - ~p ~n", [Arguments]),
  case lists:keyfind(<<"username">>, 1, Arguments) of
    false ->
      {400, encode_json({failed_reason, <<"missing username">>})};
    {_, UsernameBin} ->
      case lists:keyfind(<<"password">>, 1, Arguments) of
        false ->
          {400, encode_json({failed_reason, <<"missing password">>})};
        {_, PasswordHexBin} ->
          case catch hexstr_to_bin(binary_to_list(PasswordHexBin)) of
            {'EXIT', Reason} ->
              io:format("Exception ~p~n", [Reason]),
              {400, encode_json({failed_reason, <<"require hashed password">>})};
            PasswordBin ->
              case pine_user:login(UsernameBin, PasswordBin, Source) of
                {error, Reason} ->
                  {400, encode_json({failed_reason, Reason})};
                {ok, Cookie} ->
                  CookieBin = list_to_binary(bin_to_hexstr(Cookie)),
                  io:format("CookieBin here is ~p~n", [CookieBin]),
                  {200, encode_json({'x-pine-token', CookieBin})}
              end
          end
      end
  end;
handle_command(<<"identity.user.logout">>, Arguments, Token, Source) ->
  io:format("identity.user.logout - ~p ~p ~p ~n", [Arguments, Token, Source]),
  TokenBin = hexstr_to_bin(binary_to_list(Token)),
  case lists:keyfind(<<"username">>, 1, Arguments) of
    false ->
      {400, encode_json({failed_reason, <<"missing username">>})};
    {_, UsernameBin} ->
      case pine_user:logout(UsernameBin, TokenBin, Source) of
        {error, Reason} ->
          {400, encode_json({failed_reason, Reason})};
        ok ->
          {200, []}
      end
  end;
handle_command(FunctionName, Arguments, Token, Source) ->
  io:format("API - ~p - ~p - ~p - ~p~n", [FunctionName, Arguments, Token, Source]),
  {200, []}.
