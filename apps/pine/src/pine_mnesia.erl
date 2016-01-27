-module(pine_mnesia).
-author("Chaitanya Chalasani <cchalasani@me.com>").

-behaviour(gen_server).

-include("pine_mnesia.hrl").

-export([start_link/0, read_conf/1, read_conf/2, create_table/2,
         update_conf/3, update_conf/4, update_schema/0, readall/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

read_conf(Key) ->
  read_conf(Key, undefined).

update_schema() ->
  gen_server:call(?MODULE, update_schema).

read_conf(Key, Default) ->
  EnvValue = application:get_env(pine, Key, Default),
  case catch mnesia:dirty_read(sysconf, Key) of
    {'EXIT', _Reason} ->
      EnvValue;
    [] ->
      update_conf(Key, EnvValue, undefined, "System Default"),
      EnvValue;
    [Sysconf] ->
      Sysconf#sysconf.value
  end.

update_conf(Key, Value, User) ->
  update_conf(Key, Value, User, undefined).
update_conf(Key, Value, User, Notes) ->
  Now = os:timestamp(),
  UpdateConfFun = fun() ->
    case mnesia:read(sysconf, Key) of
      [] ->
        mnesia:write(#sysconf{key=Key, value=Value,
                                    notes=Notes, created_on=Now,
                                    created_by=User});
      [ConfRecord] ->
        NotesU = if
          Notes == undefined -> ConfRecord#sysconf.notes;
          true -> Notes
        end,
        mnesia:write(ConfRecord#sysconf{value=Value, notes=NotesU,
                                              modified_on=Now,
                                              modified_by=User})
    end
  end,
  case catch mnesia:activity(transaction, UpdateConfFun) of
    {'EXIT', Reason} ->
      {error, Reason};
    ok ->
      ok
  end.

create_table(Table, Options) ->
  gen_server:call(?MODULE, {create_table, Table, Options}).

readall(Table) ->
  [mnesia:dirty_read(Table, Key)||Key<-mnesia:dirty_all_keys(Table)].

init([]) ->
  init_schema(),
  init_tables(),
  {ok, ok}.

handle_call({create_table, Table, Options}, _From, State) ->
  Reply = create_table_imp(Table, Options),
  {reply, Reply, State};
handle_call(update_schema, _From, State) ->
  init_schema(),
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

init_schema() ->
  try_connect_schema(),
  SchemaTypeNow = mnesia:table_info(schema, storage_type),
  SchemaTypeCfg = read_conf(mnesia_schema, ram),
  case {SchemaTypeNow, SchemaTypeCfg} of
    {ram_copies, disc} ->
      mnesia:change_table_copy_type(schema, node(), disc_copies);
    {disc_copies, ram} ->
      [mnesia:change_table_copy_type(X, node(), ram_copies)||
        X <- mnesia:system_info(local_tables)];
    _ ->
      ok
  end.

try_connect_schema() ->
  case read_conf(neighbour, undefined) of
    Node when is_atom(Node) ->
      Nodes = mnesia:system_info(db_nodes),
      case lists:member(Node, Nodes) of
        false ->
          case net_adm:ping(Node) of
            pong ->
              catch rpc:call(Node, mnesia, change_config,
                             [extra_db_nodes, [node()]]);
            pang ->
              ok
          end;
        true ->
          ok
      end;
    _ ->
      ok
  end.

init_tables() ->
  create_table_imp(reference, [{disc_copies, [node()]},
      {attributes, record_info(fields, reference)}]),
  create_table_imp(sysconf, [{disc_copies, [node()]},
      {attributes, record_info(fields, sysconf)}]),
  create_table_imp(api_handlers, [{ram_copies, [node()]},
      {attributes, record_info(fields, api_handlers)}]),
  create_table_imp(access, [{ram_copies, [node()]},
      {attributes, record_info(fields, access)}]),
  mnesia:wait_for_tables([reference, sysconf, api_handlers,
                          access], 2500).

create_table_imp(Table, Options) ->
  SchemaType = read_conf(mnesia_schema, ram),
  case get_table_type(Table) of
    no_table ->
      create_table_imp_new(Table, Options, SchemaType);
    unknown ->
      copy_table_imp(Table, Options, SchemaType);
    Other ->
      change_table_imp(Table, Options, SchemaType, Other)
  end.

get_table_type(Table) ->
  TablesAll = mnesia:system_info(tables),
  case lists:member(Table, TablesAll) of
    false ->
      no_table;
    true ->
      mnesia:table_info(Table, storage_type)
  end.

create_table_imp_new(Table, Options, ram) ->
  OptionsU = [{ram_copies, [node()]}|lists:filter(
        fun(X) ->
            case X of
              {disc_copies, _} -> false;
              {disc_only_copies, _} -> false;
              {ram_copies, _} -> false;
              _ -> true
            end
        end, Options
        )],
  mnesia:create_table(Table, OptionsU);
create_table_imp_new(Table, Options, _SchemaType) ->
  mnesia:create_table(Table, Options).

copy_table_imp(Table, _Options, ram) ->
  mnesia:add_table_copy(Table, node(), ram_copies);
copy_table_imp(Table, Options, _Disc) ->
  case lists:keyfind(ram_copies, 1, Options) of
    false ->
      case lists:keyfind(disc_copies, 1, Options) of
        false ->
          mnesia:add_table_copy(Table, node(), ram_copies);
        _ ->
          mnesia:add_table_copy(Table, node(), disc_copies)
      end;
    _ ->
      mnesia:add_table_copy(Table, node(), ram_copies)
  end.

change_table_imp(_Table, _Options, ram, _StorageType) ->
  doNothing;
change_table_imp(Table, Options, _Disc, ram_copies) ->
  case lists:keyfind(disc_copies, 1, Options) of
    false ->
      doNothing;
    _ ->
      mnesia:change_table_copy_type(Table, node(), ram_copies)
  end;
change_table_imp(Table, Options, _Disc, disc_copies) ->
  case lists:keyfind(ram_copies, 1, Options) of
    false ->
      doNothing;
    _ ->
      mnesia:change_table_copy_type(Table, node(), disc_copies)
  end.

