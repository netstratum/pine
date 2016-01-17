-module(pine_user_SUITE).

%5 Common Test include
-include_lib("common_test/include/ct.hrl").

%% Define
-define(URL, "http://localhost:6090/api").

%% Common Test Exports
-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

%% Test Cases Exports
-export([list_roles/1,
         change_password/1,
         add_user/1]).

%% Common Test Exported Functions
all() ->
	[
   list_roles,
   add_user,
   change_password
  % modify_user,
  % list_users,
  % search_users,
  % getdetails_user
  ].

init_per_suite(Config) ->
  {ok, _Started} = application:ensure_all_started(pine),
  application:start(ibrowse),
  Config.

end_per_suite(Config) ->
  application:stop(pine),
  application:stop(ibrowse),
  Config.

init_per_testcase(TestCase, Config) when TestCase == list_roles;
                                         TestCase == add_user ->
  {ok, Token} = test_api:login(?URL, "root", "pa55wdr00t"),
  [{token, Token}|Config];
init_per_testcase(change_password, Config) ->
  {ok, Token} = test_api:login(?URL, "danny", "password"),
  [{token, Token}|Config];
init_per_testcase(_TestCase, Config) ->
  {ok, Token} = test_api:login(?URL, "danny", "pa55wd2016"),
  [{token, Token}|Config].

end_per_testcase(_What, Config) ->
  Token = ?config(token, Config),
  ok = test_api:logout(?URL, Token, "root"),
  proplists:delete(token, Config).

%% Test Cases Exported Functions

list_roles(Config) ->
  Token = ?config(token, Config),
  {ok, StatusCode, Roles} = test_api:list_roles(?URL,
                                                Token,
                                                "-1",
                                                "-1"),
  ct:comment("StatusCode is ~p and Roles are ~p~n",
             [StatusCode, Roles]),
  {ok, RoleId} = extract_roleId(Roles),
  {save_config, [{roleid, RoleId}]}.

add_user(Config) ->
  Token = ?config(token, Config),
  {list_roles, RoleConfig} = ?config(saved_config, Config),
  RoleId = ?config(roleid, RoleConfig),
  {ok, StatusCode} = test_api:add_user(?URL,
                                       Token,
                                       "danny",
                                       "cchalasani@me.com",
                                       "password",
                                       RoleId
                                       ),
  ct:comment("StatusCode is ~p~n", [StatusCode]),
  ok.

change_password(Config) ->
  Token = ?config(token, Config),
  {ok, StatusCode} = test_api:change_password(?URL,
                                              Token,
                                              "danny",
                                              "password",
                                              "pa55wd2016"),
  ct:comment("StatusCode is ~p~n", [StatusCode]),
  ok.

%% Internal Functions

extract_roleId(Roles) ->
  case lists:keyfind(<<"roles">>, 1, Roles) of
    false ->
      {error, no_roles};
    {_, []} ->
      {error, roles_empty};
    {_, RoleList} ->
      extract_roleId_root(RoleList)
  end.

extract_roleId_root([{RoleRow}|Rest]) ->
  case lists:keyfind(<<"name">>, 1, RoleRow) of
    {_, <<"root">>} ->
      {_, RoleIdBin} = lists:keyfind(<<"id">>, 1, RoleRow),
      {ok, binary_to_list(RoleIdBin)};
    _ ->
      extract_roleId_root(Rest)
  end;
extract_roleId_root([]) ->
  {error, no_root_role}.
