-module(pine_template_api).

-behaviour(gen_server).

-include("pine_mnesia.hrl").

-import(pine_template, [create/2,
                        modify/2,
                        list/2,
                        search/6,
                        lock/3,
                        unlock/3,
                        retire/3]).
-import(pine_user, [validate/2]).
-import(pine_tools, [maps_to_record/3,
                     try_to_int/1,
                     hexbin_to_bin/1,
                     record_to_tuplelist/2,
                     iso8601_to_ts/1,
                     bin_to_hexbin/1,
                     try_to_hexbin/1,
                     now_to_iso8601/1,
                     try_to_iso8601/2,
                     try_find_map/2]).

%% API functions
-export([start_link/0,
         create_api/1,
         modify_api/1,
         list_api/1,
         search_api/1,
         lock_api/1,
         unlock_api/1,
         retire_api/1]).

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
%% Create a new template api
%%
%% @spec create_api(TemplateMap) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
create_api(#{http_token:=Cookie,
             http_source:=Source} = Maps) ->
  Template = maps_to_record(templates,
                            Maps,
                            record_info(fields, templates)),
  Expiry = binary_to_list(Template#templates.expiry),
  ExpiryErlNow = case (catch iso8601_to_ts(Expiry)) of
                   {'EXIT', _Reason} ->
                     {error, invalid_expiry};
                   ErlNow ->
                     ErlNow
                 end,
  TemplateV2 = Template#templates{expiry=ExpiryErlNow},
  case validate(Cookie, Source) of
    {ok, User} ->
      create(User, TemplateV2);
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% Modify an existing template
%%
%% @spec modify_api(TemplateMap) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
modify_api(#{http_token:=Cookie,
             http_source:=Source} = Maps) ->
  Template = maps_to_record(templates,
                            Maps,
                            record_info(fields, templates)),
  Id = Template#templates.id,
  IdBin = hexbin_to_bin(Id),
  Expiry = binary_to_list(Template#templates.expiry),
  ExpiryErlNow = case (catch iso8601_to_ts(Expiry)) of
                   {'EXIT', _Reason} ->
                     undefined;
                   ErlNow ->
                     ErlNow
                 end,
  TemplateV2 = Template#templates{id=IdBin,
                                  expiry=ExpiryErlNow},
  case validate(Cookie, Source) of
    {ok, User} ->
      modify(User, TemplateV2);
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% List all templates api
%%
%% @spec list_api(ListMap) -> {ok, Templates} | {error, Error}
%% @end
%%--------------------------------------------------------------------
list_api(#{http_token:=Cookie,
           http_source:=Source,
           page_no:=PageNo,
           page_size:=PageSize}) ->
  PageNoInt = try_to_int(PageNo),
  PageSizeInt = try_to_int(PageSize),
  case validate(Cookie, Source) of
    {ok, _User} ->
      case list(PageNoInt, PageSizeInt) of
        {ok, Rows, TotalPages} ->
          TemplateTupleList = mk_templateTupleList(Rows),
          {ok, [{templates, TemplateTupleList},
                {total_pages, TotalPages}]};
        Other ->
          Other
      end;
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% Search all templates api
%%
%% @spec search_api(ListMap) -> {ok, Templates} | {error, Error}
%% @end
%%--------------------------------------------------------------------
search_api(#{http_token:=Cookie,
           http_source:=Source,
           page_no:=PageNo,
           page_size:=PageSize} = Maps) ->
  Name = try_find_map(name, Maps),
  Notes = try_find_map(notes, Maps),
  StartTS = try_find_map(start_ts, Maps),
  EndTS = try_find_map(end_ts, Maps),
  PageNoInt = try_to_int(PageNo),
  PageSizeInt = try_to_int(PageSize),
  case validate(Cookie, Source) of
    {ok, _User} ->
      case search(Name, Notes, StartTS, EndTS, PageNoInt, PageSizeInt) of
        {ok, Rows, TotalPages} ->
          TemplateTupleList = mk_templateTupleList(Rows),
          {ok, [{templates, TemplateTupleList},
                {total_pages, TotalPages}]};
        Other ->
          Other
      end;
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% Lock a template
%%
%% @spec lock_api(ListMap) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
lock_api(#{id:=Id,
           comment:=Comment,
           http_token:=Cookie,
           http_source:=Source}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      lock(User, IdBin, Comment);
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% Unlock a template
%%
%% @spec unlock_api(ListMap) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
unlock_api(#{id:=Id,
           comment:=Comment,
           http_token:=Cookie,
           http_source:=Source}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      unlock(User, IdBin, Comment);
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @doc
%% Retire a template
%%
%% @spec retire_api(ListMap) -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
retire_api(#{id:=Id,
           comment:=Comment,
           http_token:=Cookie,
           http_source:=Source}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      retire(User, IdBin, Comment);
    Error ->
      Error
  end.

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
  Now = os:timestamp(),
  InitApiFun = fun() ->
    mnesia:write(#api_handlers{function = <<"order.template.create">>,
                               arguments = [name, label_pattern, seq_pattern,
                                            actual_value, expiry, pin_type,
                                            pin_pattern],
                               handler = {?MODULE, create_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.template.modify">>,
                               arguments = [id],
                               handler = {?MODULE, modify_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.template.list">>,
                               arguments = [page_no, page_size],
                               handler = {?MODULE, list_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.template.search">>,
                               arguments = [page_no, page_size],
                               handler = {?MODULE, search_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.template.lock">>,
                               arguments = [id, comment],
                               handler = {?MODULE, lock_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.template.unlock">>,
                               arguments = [id, comment],
                               handler = {?MODULE, unlock_api},
                               created_on = Now}),
    mnesia:write(#api_handlers{function = <<"order.template.retire">>,
                               arguments = [id, comment],
                               handler = {?MODULE, retire_api},
                               created_on = Now})
  end,
  mnesia:activity(transaction, InitApiFun),
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

mk_templateTupleList(Rows) ->
  lists:map(fun(Row) ->
              RowU = Row#templates{id=bin_to_hexbin(Row#templates.id),
                                   expiry=now_to_iso8601(
                                    Row#templates.expiry
                                   ),
                                   created_on=try_to_iso8601(
                                    Row#templates.created_on,
                                    undefined
                                   ),
                                   created_by=try_to_hexbin(
                                    Row#templates.created_by
                                   ),
                                   modified_on=try_to_iso8601(
                                    Row#templates.modified_on,
                                    undefined
                                   ),
                                   modified_by=try_to_hexbin(
                                    Row#templates.modified_by
                                   )
                                  },
              {record_to_tuplelist(RowU, record_info(fields, templates))}
            end,
            Rows
          ).

