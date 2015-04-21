-module(pine_web).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        code_change/3, terminate/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, DispatchSource} = application:get_env(dispatch_source),
  {ok, Port} = application:get_env(port),
  {ok, PoolSize} = application:get_env(pool_size),
  Dispatch = cowboy_router:compile(DispatchSource),
  case application:get_env(secure_flag) of
    {ok, true} ->
      PrivDir = code:priv_dir(pine),
      {ok, CACERTFILE} = application:get_env(cacertfile),
      {ok, CERTFILE} = application:get_env(certfile),
      {ok, KEYFILE} = application:get_env(keyfile),
      TransOpts = [{port, Port}, 
                   {cacertfile, filename:join(PrivDir, CACERTFILE)},
                   {certfile, filename:join(PrivDir, CERTFILE)},
                   {keyfile, filename:join(PrivDir, KEYFILE)}],
      cowboy:start_https(pine, PoolSize, TransOpts, 
                         [{env, [{dispatch, Dispatch}]}]);
    _ ->
      TransOpts = [{port, Port}],
      cowboy:start_http(pine, PoolSize, TransOpts, 
                    [{env, [{dispatch, Dispatch}]}])
  end,
  {ok, ok}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
