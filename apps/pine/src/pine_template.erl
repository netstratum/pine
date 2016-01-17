-module(pine_template).
-author("Chaitanya Chalasani <cchalasani@me.com>").

-behaviour(gen_server).

-include("pine_mnesia.hrl").

%% Imports
-import(pine_mnesia, [create_table/2]).
-import(pine_tools, [uuid/0, update/2, get_keysforpage/3,
                     to/2, did_it_happen/3, has_ic/2]).

%% API functions
-export([start_link/0,
         create/2,
         modify/2,
         check/1,
         list/2,
         search/6,
         clone/4,
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
%% Check template
%%
%% @spec check(Template) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
check(TemplateId) ->
  gen_server:call(?MODULE, {check, TemplateId}).

%%--------------------------------------------------------------------
%% @doc
%% List all templates
%%
%% @spec list(User, PageNo, PageSize) -> {ok, Templates} | {error, Error}
%% @end
%%--------------------------------------------------------------------
list(PageNo, PageSize) ->
  gen_server:call(?MODULE, {list, PageNo, PageSize}).

%%--------------------------------------------------------------------
%% @doc
%% Search templates
%%
%% @spec search(User, Name, Notes, StartTS, EndTS) ->
%%            {ok, Templates} | {error, Error}
%% @end
%%--------------------------------------------------------------------
search(Name, Notes, StartTS, EndTS, PageNo, PageSize) ->
  gen_server:call(?MODULE, {search, Name, Notes, StartTS, EndTS, PageNo,
                            PageSize}).

%%--------------------------------------------------------------------
%% @doc
%% Clone a template
%%
%% @spec clone(User, TemplateId, Name, Notes) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
clone(User, TemplateId, Name, Notes) ->
  gen_server:call(?MODULE, {clone, User, TemplateId, Name, Notes}).

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
handle_call({check, TemplateId}, _From, State) ->
  Reply = handle_check(TemplateId),
  {reply, Reply, State};
handle_call({list, PageNo, PageSize}, _From, State) ->
  Reply = handle_list(PageNo, PageSize),
  {reply, Reply, State};
handle_call({search, Name, Notes, StartTS, EndTS, PageNo, PageSize},
            _From, State) ->
  Reply = handle_search(Name, Notes, StartTS, EndTS, PageNo, PageSize),
  {reply, Reply, State};
handle_call({clone, User, Id, Name, Notes}, _From, State) ->
  Reply = handle_clone(User, Id, Name, Notes),
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
      [PrevTemplate] ->
        NewTemplate = update(PrevTemplate, Template),
        mnesia:write(NewTemplate#templates{modified_on=Now,
                                           modified_by=User})
    end
  end,
  mnesia:activity(transaction, ModifyTemplateFun);
handle_modify(_User, _Template) ->
  {error, bad_record}.

handle_check(TemplateId) ->
  case mnesia:dirty_read(templates, TemplateId) of
    [] ->
      {error, no_template};
    [Template] ->
      case Template#templates.status of
        active ->
          ok;
        _ ->
          {error, template_not_active}
      end
  end.

handle_list(PageNo, PageSize) ->
  Keys = mnesia:dirty_all_keys(templates),
  case get_keysforpage(Keys, PageNo, PageSize) of
    {ok, KeysSublist, TotalPages} ->
      Rows = lists:filtermap(
        fun(Key) ->
            [Rec] = mnesia:dirty_read(templates, Key),
            if
              Rec#templates.status == retire -> false;
              true -> {true, Rec}
            end
        end,
        KeysSublist
      ),
      {ok, Rows, TotalPages};
    Error ->
      Error
  end.

handle_search(Name, Notes, StartTS, EndTS,
              PageNo, PageSize) ->
  Keys = mnesia:dirty_all_keys(templates),
  FilteredKeys = filter_templates(Keys, [{name, Name},
                                         {notes, Notes},
                                         {created, {StartTS, EndTS}}]),
  case get_keysforpage(FilteredKeys, to(int, PageNo), to(int, PageSize)) of
    {error, Reason} ->
      {error, Reason};
    {ok, KeysSublist, TotalPages} ->
      Rows = lists:filtermap(
               fun(Key) ->
                   [Rec] = mnesia:dirty_read(templates, Key),
                   if
                     Rec#templates.status == retire -> false;
                     true -> {true, Rec}
                   end
               end,
               KeysSublist
              ),
      {ok, Rows, TotalPages}
  end.

filter_templates(Keys, Filters) ->
  lists:filter(
    fun(Key) ->
        [TemplateRecord] = mnesia:dirty_read(templates, Key),
        filter_templates_bool(TemplateRecord, Filters)
    end,
    Keys
   ).

filter_templates_bool(TemplateRecord, [{_Param, undefined}|Filters]) ->
  filter_templates_bool(TemplateRecord, Filters);
filter_templates_bool(TemplateRecord, [{name, Name}|Filters]) ->
  case has_ic(TemplateRecord#templates.name, Name) of
    false ->
      filter_templates_bool(TemplateRecord, Filters);
    true ->
      true
  end;
filter_templates_bool(TemplateRecord, [{notes, Notes}|Filters]) ->
  case has_ic(TemplateRecord#templates.notes, Notes) of
    false ->
      filter_templates_bool(TemplateRecord, Filters);
    true ->
      true
  end;
filter_templates_bool(TemplateRecord, [{created, {StartTS, EndTS}}|Filters])
    when StartTS == undefined, EndTS == undefined ->
  filter_templates_bool(TemplateRecord, Filters);
filter_templates_bool(TemplateRecord, [{created, {StartTS, EndTS}}|Filters]) ->
  TemplateCreatedOn = TemplateRecord#templates.created_on,
  CreatedSeconds = to(seconds, TemplateCreatedOn),
  StartTSSeconds = to(seconds, StartTS),
  EndTSSeconds = to(seconds, EndTS),
  case did_it_happen(CreatedSeconds, StartTSSeconds, EndTSSeconds) of
    true ->
      true;
    _ ->
      filter_templates_bool(TemplateRecord, Filters)
  end;
filter_templates_bool(_TemplateRecord, []) ->
  false.

handle_clone(User, Id, Name, Notes) ->
  CloneId = uuid(),
  Now = os:timestamp(),
  CloneTemplateFun = fun() ->
    case mnesia:read(templates, Id) of
      [] ->
        {error, no_template};
      [OriginalTemplate] ->
        case mnesia:index_read(templates, Name, #templates.name) of
          [] ->
            ClonedTemplate = OriginalTemplate#templates{
                               id=CloneId,
                               name=Name,
                               notes=Notes,
                               status=active,
                               created_on=Now,
                               created_by=User,
                               modified_on=undefined,
                               modified_by=undefined
                              },
            mnesia:write(ClonedTemplate);
          _ ->
            {error, name_already_used}
        end
    end
  end,
  mnesia:activity(transaction, CloneTemplateFun).

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
