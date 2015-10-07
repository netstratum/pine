-module(pine_api).

-import(pine_tools, [encode_params/1, decode_params/1, hexstr_to_bin/1,
                     bin_to_hexstr/1, ts_to_str/1, get_missing/2, to/2]).

-export([init/3, handle/2, terminate/3]).

-include("pine_mnesia.hrl").

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  HasBody = cowboy_req:has_body(Req2),
  {ok, Req3} = handle_post(Method, HasBody, Req2),
  io:format("Req3 here is ~p~n", [Req3]),
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.

handle_post(<<"POST">>, true, Req) ->
  {ok, Arguments, Req2} = cowboy_req:body_qs(Req),
  {Token, Req3} = cowboy_req:header(<<"x-pine-token">>, Req2),
  {{Source,_Port}, Req4} = cowboy_req:peer(Req3),
  case catch handle_command(Arguments, Token, Source) of
    {'EXIT', Reason} ->
      io:format("Exception ~p~n", [Reason]),
      cowboy_req:reply(500, Req4);
    {StatusCode, []} ->
      cowboy_req:reply(StatusCode, [], [], Req4);
    {StatusCode, JsonResponse} ->
      cowboy_req:reply(StatusCode,
             [{<<"content-type">>, <<"application/json; charset=utf-8">>}],
                       JsonResponse, Req4)
  end;
handle_post(<<"POST">>, false, Req) ->
  cowboy_req:reply(400, [], encode_params({failed_reason, <<"Missing body.">>}), Req);
handle_post(_, _, Req) ->
  cowboy_req:reply(405, Req).

handle_command([{Arguments,_True}], TokenHexString, Source) ->
  ArgumentsErl = decode_params(Arguments),
  case lists:keyfind(function, 1, ArgumentsErl) of
    false ->
      {400, encode_params({failed_reason, <<"missing function name">>})};
    {_, FunctionName} ->
      Token = case catch hexstr_to_bin(binary_to_list(TokenHexString)) of
        {'EXIT', _Reason} ->
          undefined;
        TokenBin ->
          TokenBin
      end,
      ParamsList = lists:append(ArgumentsErl,
                                [{http_token, Token}, {http_source, Source}]),
      handle_command(FunctionName, ParamsList)
  end.

handle_command(FunctionName, ParamsList) ->
  case mnesia:dirty_read(api_handlers, FunctionName) of
    [] ->
      handle_command_direct(FunctionName, ParamsList);
    [ApiHandlerRec] ->
      handle_command_specs(ApiHandlerRec, ParamsList)
  end.

handle_command_specs(ApiHandlerRec, ParamsList) ->
  ParamsRequired = ApiHandlerRec#api_handlers.arguments,
  io:format("here ~p and ~p ~n", [ParamsList, ParamsRequired]),
  case get_missing(ParamsList, ParamsRequired) of
    [] ->
      {Module, Function} = ApiHandlerRec#api_handlers.handler,
      ParamsMap = maps:from_list(ParamsList),
      case erlang:apply(Module, Function, [ParamsMap]) of
        {error, Reason} ->
          {400, encode_params({failed_reason, Reason})};
        ok ->
          {200, []};
        {ok, ReplyList} ->
          {200, encode_params(ReplyList)};
        {Status, ReplyList} ->
          StatusCodes = ApiHandlerRec#api_handlers.statusCodes,
          StatusCode = case lists:keyfind(Status, 1, StatusCodes) of
            false ->
              200;
            {_, Code} ->
              Code
          end,
          {StatusCode, encode_params(ReplyList)}
      end;
    [MissingParam|_] ->
      MissingParamString = to(string, MissingParam),
      {400, encode_params({failed_reason,
                           list_to_binary("missing "++MissingParamString)})}
  end.

handle_command_direct(<<"identity.user.login">>, Arguments) ->
  io:format("identity.user.login - ~p ~n", [Arguments]),
  {_, Source} = lists:keyfind(http_source, 1, Arguments),
  case lists:keyfind(<<"username">>, 1, Arguments) of
    false ->
      {400, encode_params({failed_reason, <<"missing username">>})};
    {_, UsernameBin} ->
      case lists:keyfind(<<"password">>, 1, Arguments) of
        false ->
          {400, encode_params({failed_reason, <<"missing password">>})};
        {_, PasswordHexBin} ->
          case catch hexstr_to_bin(binary_to_list(PasswordHexBin)) of
            {'EXIT', Reason} ->
              io:format("Exception ~p~n", [Reason]),
              {400, encode_params({failed_reason, <<"require hashed password">>})};
            PasswordBin ->
              case pine_user:login(UsernameBin, PasswordBin, Source) of
                {error, Reason} ->
                  {400, encode_params({failed_reason, Reason})};
                {ok, Cookie} ->
                  CookieBin = list_to_binary(bin_to_hexstr(Cookie)),
                  io:format("CookieBin here is ~p~n", [CookieBin]),
                  {200, encode_params({'x-pine-token', CookieBin})}
              end
          end
      end
  end;
handle_command_direct(<<"identity.user.logout">>, Arguments) ->
  {_, Token} = lists:keyfind(http_token, 1, Arguments),
  {_, Source} = lists:keyfind(http_source, 1, Arguments),
  io:format("identity.user.logout - ~p ~p ~p ~n", [Arguments, Token, Source]),
  case lists:keyfind(<<"username">>, 1, Arguments) of
    false ->
      {400, encode_params({failed_reason, <<"missing username">>})};
    {_, UsernameBin} ->
      case pine_user:logout(UsernameBin, Token, Source) of
        {error, Reason} ->
          {400, encode_params({failed_reason, Reason})};
        ok ->
          {200, []}
      end
  end;
handle_command_direct(<<"pin.open">>, Arguments) ->
  {_, Token} = lists:keyfind(http_token, 1, Arguments),
  {_, Source} = lists:keyfind(http_source, 1, Arguments),
  io:format("pin.open - ~p ~p ~p ~n", [Arguments, Token, Source]),
  case pine_user:validate(Token, Source) of
    {error, Reason} ->
      {401, encode_params({failed_reason, Reason})};
    {ok, _User} ->
      case lists:keyfind(pin, 1, Arguments) of
        false ->
          {400, encode_params({failed_reason, <<"missing pin">>})};
        {_, PinBin} ->
          case lists:keyfind(opened_by, 1, Arguments) of
            false ->
              {400, encode_params({failed_reason, <<"missing opened_by">>})};
            {_, OpenedByBin} ->
              case pine_pins:open_pin(PinBin, OpenedByBin) of
                {error, Reason} ->
                  {400, encode_params({failed_reason, Reason})};
                {ok, Seq, Value} ->
                  {200, encode_params([{seq, Seq}, {value, Value}])};
                {ok, Seq, Value, OpenedOn} ->
                  OpenedDate = list_to_binary(ts_to_str(OpenedOn)),
                  {<<"250 AlreadyProcessed">>, encode_params([{seq, Seq},
                                {value, Value}, {opened_on, OpenedDate}])}
              end
          end
      end
  end;
handle_command_direct(<<"pin.close">>, Arguments) ->
  {_, Token} = lists:keyfind(http_token, 1, Arguments),
  {_, Source} = lists:keyfind(http_source, 1, Arguments),
  io:format("pin.close - ~p ~p ~p ~n", [Arguments, Token, Source]),
  case pine_user:validate(Token, Source) of
    {error, Reason} ->
      {401, encode_params({failed_reason, Reason})};
    {ok, _User} ->
      case lists:keyfind(seq, 1, Arguments) of
        false ->
          {400, encode_params({failed_reason, <<"missing seq">>})};
        {_, SeqBin} ->
          case lists:keyfind(opened_by, 1, Arguments) of
            false ->
              {400, encode_params({failed_reason, <<"missing opened_by">>})};
            {_, OpenedByBin} ->
              case pine_pins:close_pin(SeqBin, OpenedByBin) of
                {error, Reason} ->
                  {400, encode_params({failed_reason, Reason})};
                ok ->
                  {200, []}
              end
          end
      end
  end;
handle_command_direct(<<"pin.burn">>, Arguments) ->
  {_, Token} = lists:keyfind(http_token, 1, Arguments),
  {_, Source} = lists:keyfind(http_source, 1, Arguments),
  io:format("pin.burn ~p ~p ~p ~n", [Arguments, Token, Source]),
  case pine_user:validate(Token, Source) of
    {error, Reason} ->
      {401, encode_params({failed_reason, Reason})};
    {ok, _User} ->
      case lists:keyfind(seq, 1, Arguments) of
        false ->
          {400, encode_params({failed_reason, <<"missing seq">>})};
        {_, SeqBin} ->
          case lists:keyfind(opened_by, 1, Arguments) of
            false ->
              {400, encode_params({failed_reason, <<"missing opened_by">>})};
            {_, OpenedByBin} ->
              case pine_pins:burn_pin(SeqBin, OpenedByBin) of
                {error, Reason} ->
                  {400, encode_params({failed_reason, Reason})};
                ok ->
                  {200, []};
                {ok, Info} ->
                  {200, encode_params({info, Info})}
              end
          end
      end
  end;
handle_command_direct(FunctionName, Arguments) ->
  io:format("API - ~p - ~p ~n", [FunctionName, Arguments]),
  {200, []}.

