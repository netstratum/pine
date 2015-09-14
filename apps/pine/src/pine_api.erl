-module(pine_api).

-import(pine_tools, [encode_json/1, decode_json/1, hexstr_to_bin/1,
                     bin_to_hexstr/1, ts_to_str/1]).

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
  cowboy_req:reply(400, [], encode_json({failed_reason, <<"Missing body.">>}), Req);
handle_post(_, _, Req) ->
  cowboy_req:reply(405, Req).

handle_command([{Arguments,_True}], TokenHexString, Source) ->
  ArgumentsErl = decode_json(Arguments),
  case lists:keyfind(<<"function">>, 1, ArgumentsErl) of
    false ->
      {400, encode_json({failed_reason, <<"missing function name">>})};
    {_, FunctionName} ->
      Token = case catch hexstr_to_bin(binary_to_list(TokenHexString)) of
        {'EXIT', _Reason} ->
          undefined;
        TokenBin ->
          TokenBin
      end,
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
  case lists:keyfind(<<"username">>, 1, Arguments) of
    false ->
      {400, encode_json({failed_reason, <<"missing username">>})};
    {_, UsernameBin} ->
      case pine_user:logout(UsernameBin, Token, Source) of
        {error, Reason} ->
          {400, encode_json({failed_reason, Reason})};
        ok ->
          {200, []}
      end
  end;
handle_command(<<"pin.open">>, Arguments, Token, Source) ->
  io:format("pin.open - ~p ~p ~p ~n", [Arguments, Token, Source]),
  case pine_user:validate(Token, Source) of
    {error, Reason} ->
      {401, encode_json({failed_reason, Reason})};
    {ok, _User} ->
      case lists:keyfind(<<"pin">>, 1, Arguments) of
        false ->
          {400, encode_json({failed_reason, <<"missing pin">>})};
        {_, PinBin} ->
          case lists:keyfind(<<"opened_by">>, 1, Arguments) of
            false ->
              {400, encode_json({failed_reason, <<"missing opened_by">>})};
            {_, OpenedByBin} ->
              case pine_pins:open_pin(PinBin, OpenedByBin) of
                {error, Reason} ->
                  {400, encode_json({failed_reason, Reason})};
                {ok, Seq, Value} ->
                  {200, encode_json([{seq, Seq}, {value, Value}])};
                {ok, Seq, Value, OpenedOn} ->
                  OpenedDate = list_to_binary(ts_to_str(OpenedOn)),
                  {<<"250 AlreadyProcessed">>, encode_json([{seq, Seq},
                                {value, Value}, {opened_on, OpenedDate}])}
              end
          end
      end
  end;
handle_command(<<"pin.close">>, Arguments, Token, Source) ->
  io:format("pin.close - ~p ~p ~p ~n", [Arguments, Token, Source]),
  case pine_user:validate(Token, Source) of
    {error, Reason} ->
      {401, encode_json({failed_reason, Reason})};
    {ok, _User} ->
      case lists:keyfind(<<"seq">>, 1, Arguments) of
        false ->
          {400, encode_json({failed_reason, <<"missing seq">>})};
        {_, SeqBin} ->
          case lists:keyfind(<<"opened_by">>, 1, Arguments) of
            false ->
              {400, encode_json({failed_reason, <<"missing opened_by">>})};
            {_, OpenedByBin} ->
              case pine_pins:close_pin(SeqBin, OpenedByBin) of
                {error, Reason} ->
                  {400, encode_json({failed_reason, Reason})};
                ok ->
                  {200, []}
              end
          end
      end
  end;
handle_command(<<"pin.burn">>, Arguments, Token, Source) ->
  io:format("pin.burn ~p ~p ~p ~n", [Arguments, Token, Source]),
  case pine_user:validate(Token, Source) of
    {error, Reason} ->
      {401, encode_json({failed_reason, Reason})};
    {ok, _User} ->
      case lists:keyfind(<<"seq">>, 1, Arguments) of
        false ->
          {400, encode_json({failed_reason, <<"missing seq">>})};
        {_, SeqBin} ->
          case lists:keyfind(<<"opened_by">>, 1, Arguments) of
            false ->
              {400, encode_json({failed_reason, <<"missing opened_by">>})};
            {_, OpenedByBin} ->
              case pine_pins:burn_pin(SeqBin, OpenedByBin) of
                {error, Reason} ->
                  {400, encode_json({failed_reason, Reason})};
                ok ->
                  {200, []};
                {ok, Info} ->
                  {200, encode_json({info, Info})}
              end
          end
      end
  end;
% handle_command(<<"sys.conf">>, Arguments, Token, Source) ->
%   io:format("conf.mnesia.schema ~p ~p ~p~n", [Arguments, Token, Source]),
%   case pine_user:validate(Token, Source) of
%     {error, Reason} ->
%       {401, encode_json({failed_reason, Reason})};
%     {ok, User} ->
%       case update_configs(Configs, User) of
%         ok ->
%           {200, []};
%         {ok, Results} ->
%           {207, encode_json_results(Results)};
%         {error, Reason} ->
%           {400, encode_json({failed_Reason, Reason})}
%       end;
%   end;
handle_command(FunctionName, Arguments, Token, Source) ->
  io:format("API - ~p - ~p - ~p - ~p~n", [FunctionName, Arguments, Token, Source]),
  {200, []}.

% update_configs([{Key, Value}|Configs], User) ->
%   KeyAtom = if
%     is_atom(Key) -> Key;
%     is_list(Key) -> list_to_atom(Key);
%     is_binary(Key) -> list_to_atom(binary_to_list(Key))
%   end,
%   case pine_mnesia:update_conf(KeyAtom, Value, User) of

