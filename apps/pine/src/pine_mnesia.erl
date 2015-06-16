-module(pine_mnesia).
-behaviour(gen_server).

-include("pine_mnesia.hrl").

-export([start_link/0, read_conf/1, read_conf/2, create_table/2,
         update_conf/3, update_conf/4, update_schema/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

read_conf(Key) ->
  read_conf(Key, undefined).

update_schema() ->
  init_schema().

read_conf(Key, Default) ->
  case catch mnesia:dirty_read(sysconf, Key) of
    {'EXIT', _Reason} ->
      application:get_env(pine, Key, Default);
    [] ->
      Value = application:get_env(pine, Key, Default),
      Now = os:timestamp(),
      mnesia:dirty_write(#sysconf{key=Key, value=Value, 
                                  notes="System Default",
                                  created_on=Now}),
      Value;
    [Sysconf] ->
      Sysconf#sysconf.value
  end.

update_conf(Key, Value, User) ->
  update_conf(Key, Value, User, undefined).
update_conf(Key, Value, User, Notes) ->
  Now = os:timestamp(),
  case catch mnesia:dirty_read(sysconf, Key) of
    {'EXIT', _Reason} ->
      {error, no_table};
    [] ->
      mnesia:dirty_write(#sysconf{key=Key, value=Value,
                                  notes=Notes, created_on=Now,
                                  created_by=User});
    [ConfRecord] ->
      NotesU = if 
        Notes == undefined -> ConfRecord#sysconf.notes;
        true -> Notes
      end,
      mnesia:dirty_write(ConfRecord#sysconf{value=Value, notes=NotesU,
                                            modified_on=Now, 
                                            modified_by=User})
  end.

create_table(Table, Options) ->
  gen_server:call(?MODULE, {create_table, Table, Options}).

init([]) ->
  init_schema(),
  init_tables(),
  {ok, ok}.

handle_call({create_table, Table, Options}, _From, State) ->
  Reply = create_table_imp(Table, Options),
  {reply, Reply, State};
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

init_tables() ->
  create_table_imp(reference, 
                   [{attributes, record_info(fields, reference)}]),
  create_table_imp(sysconf, 
                   [{attributes, record_info(fields, sysconf)}]).

create_table_imp(Table, Options) ->
  SchemaType = read_conf(mnesia_schema, ram),
  case catch mnesia:table_info(Table, storage_type) of
    {'EXIT', _Reason} ->
      create_table_imp_new(Table, Options, SchemaType);
    unknown ->
      create_table_imp_copy(Table, Options, SchemaType);
    Type ->
      create_table_imp_change(Table, Options, Type, SchemaType)
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

create_table_imp_copy(Table, _Options, ram) ->
  mnesia:add_table_copy(Table, node(), ram_copies);
create_table_imp_copy(Table, Options, _SchemaType) ->
  CopyType = case lists:keyfind(disc_only_copies, 1, Options) of
    false ->
      case lists:keyfind(disc_copies, 1, Options) of
        false ->
          ram_copies;
        _ ->
          disc_copies
      end;
    _ ->
      disc_only_copies
  end,
  mnesia:add_table_copy(Table, node(), CopyType).

create_table_imp_change(_Table, _Options, _Type, ram) ->
  ok;
create_table_imp_change(Table, Options, disc_copies, disc) ->
  case lists:keyfind(disc_only_copies, 1, Options) of
    false ->
      case lists:keyfind(ram_copies, 1, Options) of
        false ->
          ok;
        _ ->
          mnesia:change_table_copy_type(Table, node(), ram_copies)
      end;
    _ ->
      mnesia:change_table_copy_type(Table, node(), disc_only_copies)
  end.

