-module(pine_template).

-behaviour(gen_server).

-include("pine_mnesia.hrl").

%% Imports
-import(pine_mnesia, [create_table/2]).
-import(pine_tools, [uuid/0, update/2, get_keysforpage/3]).

%% API functions
-export([start_link/0,
         create/2,
         modify/2,
         list/3,
         search/5,
         lock/3,
         unlock/3,
         retire/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Create a new template
%%
%% @spec create(User, Template) -> {ok, TemplateId} | {error, Error}
%% @end
%%--------------------------------------------------------------------
create(User, Template) ->
  gen_server:call(?MODULE, {create, User, Template}).

%%--------------------------------------------------------------------
%% @doc
%% Modify a template
%%
%% @spec modify(User, Template) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
modify(User, Template) ->
  gen_server:call(?MODULE, {modify, User, Template}).

%%--------------------------------------------------------------------
%% @doc
%% List all templates
%%
%% @spec list(User, PageNo, PageSize) -> {ok, Templates} | {error, Error}
%% @end
%%--------------------------------------------------------------------
list(User, PageNo, PageSize) ->
  gen_server:call(?MODULE, {list, User, PageNo, PageSize}).

%%--------------------------------------------------------------------
%% @doc
%% Search templates
%%
%% @spec search(User, Name, Notes, StartTS, EndTS) ->
%%            {ok, Templates} | {error, Error}
%% @end
%%--------------------------------------------------------------------
search(User, Name, Notes, StartTS, EndTS) ->
  gen_server:call(?MODULE, {search, User, Name, Notes, StartTS, EndTS}).

%%--------------------------------------------------------------------
%% @doc
%% Lock a template
%%
%% @spec lock(User, TemplateId, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
lock(User, TemplateId, Comment) ->
  gen_server:call(?MODULE, {lock, User, TemplateId, Comment}).

%%--------------------------------------------------------------------
%% @doc
%% unLock a template
%%
%% @spec unlock(User, TemplateId, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
unlock(User, TemplateId, Comment) ->
  gen_server:call(?MODULE, {unlock, User, TemplateId, Comment}).

%%--------------------------------------------------------------------
%% @doc
%% Retire a template
%%
%% @spec retire(User, TemplateId, Comment) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
retire(User, TemplateId, Comment) ->
  gen_server:call(?MODULE, {retire, User, TemplateId, Comment}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  init_tables(),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({create, User, Template}, _From, State) ->
  Reply = handle_create(User, Template),
  {reply, Reply, State};
handle_call({modify, User, Template}, _From, State) ->
  Reply = handle_modify(User, Template),
  {reply, Reply, State};
handle_call({list, User, PageNo, PageSize}, _From, State) ->
  Reply = handle_list(User, PageNo, PageSize),
  {reply, Reply, State};
handle_call({search, User, Name, Notes, StartTS, EndTS},
            _From, State) ->
  Reply = handle_search(User, Name, Notes, StartTS, EndTS),
  {reply, Reply, State};
handle_call({lock, User, Id, Comment}, _From, State) ->
  Reply = handle_lock(User, Id, Comment),
  {reply, Reply, State};
handle_call({unlock, User, Id, Comment}, _From, State) ->
  Reply = handle_unlock(User, Id, Comment),
  {reply, Reply, State};
handle_call({retire, User, Id, Comment}, _From, State) ->
  Reply = handle_retire(User, Id, Comment),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_tables() ->
  lists:map(
    fun({Table, Options}) -> create_table(Table, Options) end,
    [{templates, [{disc_copies, [node()]},
                  {attributes, record_info(fields, templates)},
                  {index, [name]}]}]
   ),
  mnesia:wait_for_tables([templates], 2500).

%% TODO
%% Has to to validate the reference data references
handle_create(User, Template) when is_record(Template, templates) ->
  Id = uuid(),
  Now = os:timestamp(),
  CreateTemplateFun = fun() ->
    case mnesia:index_read(templates,
                           Template#templates.name,
                           #templates.name) of
      [] ->
        mnesia:write(Template#templates{id=Id,
                                        status=active,
                                        created_on=Now,
                                        created_by=User});
      _ ->
        {error, already_exists}
    end
  end,
  mnesia:activity(transaction, CreateTemplateFun);
handle_create(_User, _Template) ->
  {error, bad_record}.

%% TODO
%% Has to ensure the wrapper updates undefined for the fields not updated
handle_modify(User, Template) when is_record(Template, templates) ->
  Now = os:timestamp(),
  ModifyTemplateFun = fun() ->
    case mnesia:read(templates, Template#templates.id) of
      [] ->
        {error, no_record};
      PrevTemplate ->
        NewTemplate = update(PrevTemplate, Template),
        mnesia:write(NewTemplate#templates{modified_on=Now,
                                           modified_by=User})
    end
  end,
  mnesia:activity(transaction, ModifyTemplateFun);
handle_modify(_User, _Template) ->
  {error, bad_record}.

handle_list(_User, PageNo, PageSize) ->
  Keys = mnesia:dirty_all_keys(templates),
  case get_keysforpage(Keys, PageNo, PageSize) of
    {ok, KeysSublist, TotalPages} ->
      Rows = lists:flatten(lists:map(
        fun(Key) -> mnesia:dirty_read(users, Key) end,
        KeysSublist
      )),
      {ok, Rows, TotalPages};
    Error ->
      Error
  end.

handle_search(_User, _Name, _Notes, _StartTS, _EndTS) ->
  ok.

handle_lock(User, Id, Comment) ->
  Now = os:timestamp(),
  LockTemplateFun = fun() ->
    case mnesia:read(templates, Id) of
      [] ->
        {error, no_template};
      [Template] ->
        case Template#templates.status of
          lock ->
            {error, already_locked};
          retire ->
            {error, no_template};
          _->
            mnesia:write(Template#templates{status=lock,
                                            status_comment=Comment,
                                            modified_on=Now,
                                            modified_by=User})
        end
    end
  end,
  mnesia:activity(transaction, LockTemplateFun).

handle_unlock(User, Id, Comment) ->
  Now = os:timestamp(),
  UnlockTemplateFun = fun() ->
    case mnesia:read(templates, Id) of
      [] ->
        {error, no_template};
      [Template] ->
        case Template#templates.status of
          active ->
            {error, already_active};
          retire ->
            {error, no_template};
          _->
            mnesia:write(Template#templates{status=active,
                                            status_comment=Comment,
                                            modified_on=Now,
                                            modified_by=User})
        end
    end
  end,
  mnesia:activity(transaction, UnlockTemplateFun).

handle_retire(User, Id, Comment) ->
  Now = os:timestamp(),
  RetireTemplateFun = fun() ->
    case mnesia:read(templates, Id) of
      [] ->
        {error, no_template};
      [Template] ->
        case Template#templates.status of
          retire ->
            {error, no_template};
          _->
            mnesia:write(Template#templates{status=retire,
                                            status_comment=Comment,
                                            modified_on=Now,
                                            modified_by=User})
        end
    end
  end,
  mnesia:activity(transaction, RetireTemplateFun).
