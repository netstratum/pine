-module(test_srv).
-behaviour(gen_server).

-record(state, {
    token,
    username,
    url
  }).

-export([start/0, start/2, stop/1, open_pin/2, close_pin/2, burn_pin/2,
         change_password/2, add_user/2, list_users/2, list_roles/2,
         modify_user/2, search_users/2, getdetails_user/2, lock_user/2,
         unlock_user/2, retire_user/2, create_template/2, modify_template/2,
         list_templates/2, lock_template/2, unlock_template/2,
         retire_template/2, search_template/2, create_order/2,
         modify_order/2, search_order/2, list_order/2, lock_order/2,
         unlock_order/2, retire_order/2, approve_order/2, reject_order/2,
         activate_order/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

start() ->
  {ok, [TestName]} = io:fread("Test name: ", "~s"),
  {ok, [Url]} = io:fread("Server Url: ", "~s"),
  {ok, [Username]} = io:fread("Username: ", "~s"),
  {ok, [Password]} = io:fread("Password: ", "~s"),
  start(list_to_atom(TestName), {Url, Username, Password}).

start(TestName, {Url, Username, Password}) ->
  gen_server:start({local, TestName}, ?MODULE, [Url, Username, Password], []).

stop(TestName) ->
  gen_server:cast(TestName, stop).

open_pin(TestName, {Pin, EndUser}) ->
  gen_server:call(TestName, {open_pin, Pin, EndUser}).

close_pin(TestName, {Seq, EndUser}) ->
  gen_server:call(TestName, {close_pin, Seq, EndUser}).

burn_pin(TestName, {Seq, EndUser}) ->
  gen_server:call(TestName, {burn_pin, Seq, EndUser}).

change_password(TestName, {Username, OldPassword, NewPassword}) ->
  gen_server:call(TestName, {change_password,
                         Username,
                         OldPassword,
                         NewPassword}).
add_user(TestName, {Username, Email, Password, Role}) ->
  gen_server:call(TestName, {add_user,
                         Username,
                         Email,
                         Password,
                         Role}).

list_users(TestName, {PageNo, PageSize}) ->
  gen_server:call(TestName, {list_users, PageNo, PageSize}).

modify_user(TestName, {Id, Name, Notes, Email, Expiry, RoleId}) ->
  gen_server:call(TestName, {modify_user,
                         Id,
                         Name,
                         Notes,
                         Email,
                         Expiry,
                         RoleId}).

search_users(TestName, {Name, Email, StartTS, EndTS, PageNo, PageSize}) ->
  gen_server:call(TestName, {search_users,
                             Name,
                             Email,
                             StartTS,
                             EndTS,
                             PageNo,
                             PageSize}).

getdetails_user(TestName, Id) ->
  gen_server:call(TestName, {getdetails_user, Id}).

list_roles(TestName, {PageNo, PageSize}) ->
  gen_server:call(TestName, {list_roles, PageNo, PageSize}).

lock_user(TestName, {Id, Comment}) ->
  gen_server:call(TestName, {lock_user, Id, Comment}).

unlock_user(TestName, {Id, Comment}) ->
  gen_server:call(TestName, {unlock_user, Id, Comment}).

retire_user(TestName, {Id, Comment}) ->
  gen_server:call(TestName, {retire_user, Id, Comment}).

create_template(TestName, {Name, LabelPattern, SeqPattern,
                           ActualValue, Expiry, PinType,
                           PinPattern}) ->
  gen_server:call(TestName, {create_template,
                             Name,
                             LabelPattern,
                             SeqPattern,
                             ActualValue,
                             Expiry,
                             PinType,
                             PinPattern}).
modify_template(TestName, {Id, Notes, ActualValue}) ->
  gen_server:call(TestName, {modify_template,
                             Id,
                             Notes,
                             ActualValue}).

list_templates(TestName, {PageNo, PageSize}) ->
  gen_server:call(TestName, {list_templates,
                             PageNo,
                             PageSize}).

search_template(TestName, {Name, Notes, StartTS, EndTS, PageNo, PageSize}) ->
  gen_server:call(TestName, {search_template,
                             Name,
                             Notes,
                             StartTS,
                             EndTS,
                             PageNo,
                             PageSize}).

lock_template(TestName, {Id, Comment}) ->
  gen_server:call(TestName, {lock_template, Id, Comment}).

unlock_template(TestName, {Id, Comment}) ->
  gen_server:call(TestName, {unlock_template, Id, Comment}).

retire_template(TestName, {Id, Comment}) ->
  gen_server:call(TestName, {retire_template, Id, Comment}).

create_order(TestName, {Name, PinTemplate, PinCount}) ->
  gen_server:call(TestName, {create_order,
                             Name,
                             PinTemplate,
                             PinCount}).

modify_order(TestName, {Id, Name, Notes, PinCount, Comment}) ->
  gen_server:call(TestName, {modify_order,
                             Id,
                             Name,
                             Notes,
                             PinCount,
                             Comment}).

search_order(TestName, {Name, Notes, PageNo, PageSize}) ->
  gen_server:call(TestName, {search_order,
                             Name,
                             Notes,
                             PageNo,
                             PageSize}).

list_order(TestName, {PageNo, PageSize}) ->
  gen_server:call(TestName, {list_order, PageNo, PageSize}).

lock_order(TestName, {Id, Comment}) ->
  gen_server:call(TestName, {lock_order, Id, Comment}).

unlock_order(TestName, {Id, Comment}) ->
  gen_server:call(TestName, {unlock_order, Id, Comment}).

retire_order(TestName, {Id, Comment}) ->
  gen_server:call(TestName, {retire_order, Id, Comment}).

approve_order(TestName, {Id, Comment}) ->
  gen_server:call(TestName, {approve_order, Id, Comment}).

reject_order(TestName, {Id, Comment}) ->
  gen_server:call(TestName, {reject_order, Id, Comment}).

activate_order(TestName, {Id, Comment}) ->
  gen_server:call(TestName, {activate_order, Id, Comment}).

init([Url, Username, Password]) ->
  case test_api:login(Url, Username, Password) of
    {ok, Token} ->
      {ok, #state{token=Token, username=Username, url=Url}};
    {error, Reason} ->
      {stop, Reason}
  end.

handle_call({open_pin, Pin, EndUser}, _From, State) ->
  Reply = test_api:open_pin(State#state.url, State#state.token, Pin, EndUser),
  {reply, Reply, State};
handle_call({close_pin, Seq, EndUser}, _From, State) ->
  Reply = test_api:close_pin(State#state.url, State#state.token, Seq, EndUser),
  {reply, Reply, State};
handle_call({burn_pin, Seq, EndUser}, _From, State) ->
  Reply = test_api:burn_pin(State#state.url, State#state.token, Seq, EndUser),
  {reply, Reply, State};
handle_call({change_password, Username, OldPassword, NewPassword}, _From, State) ->
  Reply = test_api:change_password(State#state.url,
                                   State#state.token,
                                   Username,
                                   OldPassword,
                                   NewPassword),
  {reply, Reply, State};
handle_call({add_user, Name, Email, Password, Role}, _From, State) ->
  Reply = test_api:add_user(State#state.url,
                            State#state.token,
                            Name,
                            Email,
                            Password,
                            Role),
  {reply, Reply, State};
handle_call({modify_user, Id, Name, Notes, Email, Expiry, RoleId},
            _From, State) ->
  Reply = test_api:modify_user(State#state.url,
                               State#state.token,
                               Id,
                               Name,
                               Notes,
                               Email,
                               Expiry,
                               RoleId),
  {reply, Reply, State};
handle_call({search_users, Name, Email, StartTS, EndTS, PageNo, PageSize},
            _From, State) ->
  Reply = test_api:search_users(State#state.url,
                                State#state.token,
                                Name,
                                Email,
                                StartTS,
                                EndTS,
                                PageNo,
                                PageSize),
  {reply, Reply, State};
handle_call({list_users, PageNo, PageSize}, _From, State) ->
  Reply = test_api:list_users(State#state.url,
                              State#state.token,
                              PageNo,
                              PageSize),
  {reply, Reply, State};
handle_call({getdetails_user, Id}, _From, State) ->
  Reply = test_api:getdetails_user(State#state.url,
                                   State#state.token,
                                   Id),
  {reply, Reply, State};
handle_call({list_roles, PageNo, PageSize}, _From, State) ->
  Reply = test_api:list_roles(State#state.url,
                              State#state.token,
                              PageNo,
                              PageSize),
  {reply, Reply, State};
handle_call({lock_user, Id, Comment}, _From, State) ->
  Reply = test_api:lock_user(State#state.url,
                             State#state.token,
                             Id,
                             Comment),
  {reply, Reply, State};
handle_call({unlock_user, Id, Comment}, _From, State) ->
  Reply = test_api:unlock_user(State#state.url,
                             State#state.token,
                             Id,
                             Comment),
  {reply, Reply, State};
handle_call({retire_user, Id, Comment}, _From, State) ->
  Reply = test_api:retire_user(State#state.url,
                             State#state.token,
                             Id,
                             Comment),
  {reply, Reply, State};
handle_call({create_template, Name, LabelPattern, SeqPattern,
             ActualValue, Expiry, PinType, PinPattern}, _From, State) ->
  Reply = test_api:create_template(State#state.url,
                                   State#state.token,
                                   [{name, Name},
                                    {label_pattern, LabelPattern},
                                    {seq_pattern, SeqPattern},
                                    {actual_value, ActualValue},
                                    {expiry, Expiry},
                                    {pin_type, PinType},
                                    {pin_pattern, PinPattern}]),
  {reply, Reply, State};
handle_call({modify_template, Id, Notes, ActualValue}, _From, State) ->
  Reply = test_api:modify_template(State#state.url,
                                   State#state.token,
                                   [{id, Id},
                                    {notes, Notes},
                                    {actual_value, ActualValue}]),
  {reply, Reply, State};
handle_call({list_templates, PageNo, PageSize}, _From, State) ->
  Reply = test_api:list_templates(State#state.url,
                                  State#state.token,
                                  PageNo,
                                  PageSize),
  {reply, Reply, State};
handle_call({search_template, Name, Notes, StartTS, EndTS, PageNo, PageSize},
            _From, State) ->
  Reply = test_api:search_template(State#state.url,
                                   State#state.token,
                                   Name,
                                   Notes,
                                   StartTS,
                                   EndTS,
                                   PageNo,
                                   PageSize),
  {reply, Reply, State};
handle_call({lock_template, Id, Comment}, _From, State) ->
  Reply = test_api:lock_template(State#state.url,
                                 State#state.token,
                                 Id,
                                 Comment),
  {reply, Reply, State};
handle_call({unlock_template, Id, Comment}, _From, State) ->
  Reply = test_api:unlock_template(State#state.url,
                                   State#state.token,
                                   Id,
                                   Comment),
  {reply, Reply, State};
handle_call({retire_template, Id, Comment}, _From, State) ->
  Reply = test_api:retire_template(State#state.url,
                                   State#state.token,
                                   Id,
                                   Comment),
  {reply, Reply, State};
handle_call({create_order, Name, PinTemplate, PinCount}, _From, State) ->
  Reply = test_api:create_order(State#state.url,
                                State#state.token,
                                Name,
                                PinTemplate,
                                PinCount),
  {reply, Reply, State};
handle_call({modify_order, Id, Name, Notes, PinCount, Comment},
            _From, State) ->
  Reply = test_api:modify_order(State#state.url,
                                State#state.token,
                                Id, Name, Notes, PinCount,
                                Comment),
  {reply, Reply, State};
handle_call({search_order, Name, Notes, PageNo, PageSize},
            _From, State) ->
  Reply = test_api:search_order(State#state.url,
                                State#state.token,
                                Name, Notes, PageNo, PageSize),
  {reply, Reply, State};
handle_call({list_order, PageNo, PageSize}, _From, State) ->
  Reply = test_api:list_order(State#state.url,
                              State#state.token,
                              PageNo,
                              PageSize),
  {reply, Reply, State};
handle_call({lock_order, Id, Comment}, _From, State) ->
  Reply = test_api:lock_order(State#state.url,
                              State#state.token,
                              Id,
                              Comment),
  {reply, Reply, State};
handle_call({unlock_order, Id, Comment}, _From, State) ->
  Reply = test_api:unlock_order(State#state.url,
                                State#state.token,
                                Id,
                                Comment),
  {reply, Reply, State};
handle_call({retire_order, Id, Comment}, _From, State) ->
  Reply = test_api:retire_order(State#state.url,
                                State#state.token,
                                Id,
                                Comment),
  {reply, Reply, State};
handle_call({approve_order, Id, Comment}, _From, State) ->
  Reply = test_api:approve_order(State#state.url,
                                 State#state.token,
                                 Id,
                                 Comment),
  {reply, Reply, State};
handle_call({reject_order, Id, Comment}, _From, State) ->
  Reply = test_api:reject_order(State#state.url,
                                State#state.token,
                                Id,
                                Comment),
  {reply, Reply, State};
handle_call({activate_order, Id, Comment}, _From, State) ->
  Reply = test_api:activate_order(State#state.url,
                                  State#state.token,
                                  Id,
                                  Comment),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(stop, State) ->
  Response = test_api:logout(State#state.url, State#state.token, State#state.username),
  io:format("Response is ~p~n", [Response]),
  {stop, normal, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
