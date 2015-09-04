-module(pine_web).
-behaviour(gen_server).

-import(pine_mnesia, [read_conf/1, read_conf/2]).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        code_change/3, terminate/2]).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  DispatchSource = read_conf(dispatch_source, []),
  Port = read_conf(port, 6090),
  PoolSize = read_conf(pool_size, 100),
  Dispatch = cowboy_router:compile(DispatchSource),
  CACERTFILE = read_conf(cacertfile),
  CERTFILE = read_conf(certfile),
  KEYFILE = read_conf(keyfile),
  case read_conf(secure_flag, false) of
    true when CACERTFILE =/= undefined; CERTFILE =/= undefined;
        KEYFILE =/= undefined ->
      PrivDir = code:priv_dir(pine),
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
