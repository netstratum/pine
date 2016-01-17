-module(pine_order_SUITE).

%% Common Test callbacks
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Test cases
-export([add_printer/1,
         list_printers/1,
         modify_printer/1,
         lock_printer/1,
         unlock_printer/1,
         retire_printer/1]).

-include_lib("common_test/include/ct.hrl").

-define(URL, "http://localhost:6090/api").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
  [add_printer,
   list_printers,
   modify_printer,
   lock_printer,
   unlock_printer,
   retire_printer].

suite() ->
  [{timetrap, {seconds, 30}}].

groups() ->
  [].

init_per_suite(Config) ->
  {ok, _Started} = application:ensure_all_started(pine),
  application:start(ibrowse),
  {ok, Token} = test_api:login(?URL, "root", "pa55wdr00t"),
  [{token, Token}|[{url, ?URL}|Config]].

end_per_suite(Config) ->
  Token = ?config(token, Config),
  ok = test_api:logout(?URL, Token, "root"),
  application:stop(pine),
  application:stop(ibrowse),
  ok.

group(_GroupName) ->
  [].

init_per_group(_GroupName, Config) ->
  Config.

end_per_group(_GroupName, _Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

add_printer(Config) ->
  Token = ?config(token, Config),
  Url = ?config(url, Config),
  {ok, StatusCode} = test_api:add_printer(Url,
                            Token,
                            "SpeedPayUAE",
                            "SpeedPay UAE Printer",
                            "UAE",
                            "123456789012345"),
  {comment, StatusCode}.

list_printers(Config) ->
  Token = ?config(token, Config),
  Url = ?config(url, Config),
  {ok, StatusCode, ResponseObject} = test_api:list_printer(Url,
                                                           Token,
                                                           "-1",
                                                           "-1"),
  ct:comment(ResponseObject),
  {ok, PrinterId} = test_tools:get_member(["printers","id"],
                                          ResponseObject),
  ct:comment(StatusCode),
  {save_config, [{printerId, PrinterId}]}.

modify_printer(Config) ->
  Token = ?config(token, Config),
  Url = ?config(url, Config),
  {list_printers, PrinterConfig} = ?config(saved_config, Config),
  PrinterId = ?config(printerId, PrinterConfig),
  {ok, StatusCode} = test_api:modify_printer(Url,
                               Token,
                               PrinterId,
                               "SpeedPay Soudi Printer",
                               "Soudi",
                               []),
  ct:comment(StatusCode),
  {save_config, [{printerId, PrinterId}]}.

lock_printer(Config) ->
  Token = ?config(token, Config),
  Url = ?config(url, Config),
  {modify_printer, PrinterConfig} = ?config(saved_config, Config),
  PrinterId = ?config(printerId, PrinterConfig),
  {ok, StatusCode} = test_api:lock_printer(Url,
                             Token,
                             PrinterId,
                             "Just locking it"),
  ct:comment(StatusCode),
  {save_config, [{printerId, PrinterId}]}.

unlock_printer(Config) ->
  Token = ?config(token, Config),
  Url = ?config(url, Config),
  {lock_printer, PrinterConfig} = ?config(saved_config, Config),
  PrinterId = ?config(printerId, PrinterConfig),
  {ok, StatusCode} = test_api:unlock_printer(Url,
                               Token,
                               PrinterId,
                               "Again unlocking it"),
  ct:comment(StatusCode),
  {save_config, [{printerId, PrinterId}]}.

retire_printer(Config) ->
  Token = ?config(token, Config),
  Url = ?config(url, Config),
  {unlock_printer, PrinterConfig} = ?config(saved_config, Config),
  PrinterId = ?config(printerId, PrinterConfig),
  {ok, StatusCode} = test_api:retire_printer(Url,
                               Token,
                               PrinterId,
                               "Finally retiring it"),
  ct:comment(StatusCode),
  ok.
