-module(pine_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  ok = gen_event:add_handler(pine_gen_event, pine_log, []),
  {ok, Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  OtherChild = {pine_gen_event, {gen_event, start_link,
    [{local, pine_gen_event}]}, permanent, 5000, worker, [dynamic]},
  {ok, { {one_for_one, 5, 10}, [OtherChild,
                                ?CHILD(pine_mnesia, worker),
                                ?CHILD(pine_user, worker),
                                ?CHILD(pine_user_api, worker),
                                ?CHILD(pine_pins, worker),
                                ?CHILD(pine_pins_api, worker),
                                ?CHILD(pine_template, worker),
                                ?CHILD(pine_template_api, worker),
                                ?CHILD(pine_order, worker),
                                ?CHILD(pine_order_api, worker),
                                ?CHILD(pine_web, worker)]} }.

