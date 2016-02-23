-module(pine_role_api).
-author("Chaitanya Chalasani <cchalasani@me.com>").

-behaviour(gen_server).

-include("pine_mnesia.hrl").

-import(pine_tools, [try_to_int/1, to/2, hexbin_to_bin/1,
                     try_to_hexbin/1, try_to_hexbin_list/1,
                     try_find_map/2]).

-import(pine_user, [validate/2]).
-import(pine_role, [listroles/2,
                    listaccess/2,
                    add/4,
                    modify/4,
                    lock/3,
                    unlock/3,
                    retire/3]).

%% API functions
-export([start_link/0,
         listroles_api/1,
         listaccess_api/1,
         add_api/1,
         modify_api/1,
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

%%%===================================================================
%%% API functions
%%%===================================================================
listroles_api(#{http_token:=Cookie,
                http_source:=Source,
                page_no:=PageNo,
                page_size:=PageSize}) ->
  PageNoInt = try_to_int(PageNo),
  PageSizeInt = try_to_int(PageSize),
  case validate(Cookie, Source) of
    {ok, _User} ->
      case listroles(PageNoInt, PageSizeInt) of
        {ok, Rows, Totalpages} ->
          RowsTupleList = mk_roleTupleList(Rows),
          {ok, [{roles, RowsTupleList},
                {total_pages, Totalpages}]};
        Other ->
          Other
      end;
    Error ->
      Error
  end.

listaccess_api(#{http_token:=Cookie,
                 http_source:=Source,
                 page_no:=PageNo,
                 page_size:=PageSize}) ->
  PageNoInt = try_to_int(PageNo),
  PageSizeInt = try_to_int(PageSize),
  case validate(Cookie, Source) of
    {ok, _User} ->
      case listaccess(PageNoInt, PageSizeInt) of
        {ok, Rows, Totalpages} ->
          RowsTupleList = mk_accessTupleList(Rows),
          {ok, [{access, RowsTupleList},
                {total_pages, Totalpages}]};
        Other ->
          Other
      end;
    Error ->
      Error
  end.

add_api(#{http_token:=Cookie,
          http_source:=Source,
          name:=Name,
          access_list:=AccessList} = Maps) ->
  Notes = try_find_map(notes, Maps),
  case validate(Cookie, Source) of
    {ok, User} ->
      add(User, Name, Notes, AccessList);
    Error ->
      Error
  end.

modify_api(#{http_token:=Cookie,
             http_source:=Source,
             id:=Id} = Maps) ->
  Notes = try_find_map(notes, Maps),
  AccessList = try_find_map(access_list, Maps),
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      modify(User, IdBin, Notes, AccessList);
    Error ->
      Error
  end.

lock_api(#{http_token:=Cookie,
           http_source:=Source,
           id:=Id,
           comment:=Comment}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      lock(User, IdBin, Comment);
    Error ->
      Error
  end.

unlock_api(#{http_token:=Cookie,
             http_source:=Source,
             id:=Id,
             comment:=Comment}) ->
  IdBin = hexbin_to_bin(Id),
  case validate(Cookie, Source) of
    {ok, User} ->
      unlock(User, IdBin, Comment);
    Error ->
      Error
  end.

retire_api(#{http_token:=Cookie,
             http_source:=Source,
             id:=Id,
             comment:=Comment}) ->
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
  init_api(),
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

init_api() ->
  Now = os:timestamp(),
  InitApiFun = fun() ->
    mnesia:write(#api_handlers{function = <<"identity.role.list">>,
                               arguments = [page_no, page_size],
                               handler = {?MODULE, listroles_api},
                               created_on = Now})
  end,
  mnesia:transaction(InitApiFun).

mk_roleTupleList(RoleRecords) ->
  TupleListFunction = fun(RoleRecord) ->
    {[{id, try_to_hexbin(RoleRecord#roles.id)},
      {name, to(binary,RoleRecord#roles.name)},
      {notes, to(binary,RoleRecord#roles.notes)},
      {access_list, try_to_hexbin_list(RoleRecord#roles.access_list)},
      {status, to(binary,RoleRecord#roles.status)},
      {status_comment, to(binary,RoleRecord#roles.status_comment)},
      {created_on, to(binary,RoleRecord#roles.created_on)},
      {created_by, try_to_hexbin(RoleRecord#roles.created_by)},
      {modified_on, to(binary,RoleRecord#roles.modified_on)},
      {modified_by, try_to_hexbin(RoleRecord#roles.modified_by)}]}
   end,
  lists:map(TupleListFunction, RoleRecords).

mk_accessTupleList(AccessRecords) ->
  TupleListFunction = fun(AccessRecord) ->
    {[{id, try_to_hexbin(AccessRecord#access.id)},
      {name, to(binary,AccessRecord#access.name)},
      {notes, to(binary,AccessRecord#access.notes)},
      {feature, to(binary,AccessRecord#access.feature)},
      {function, to(binary,AccessRecord#access.function)},
      {status, to(binary,AccessRecord#access.status)},
      {created_on, to(binary,AccessRecord#access.created_on)},
      {modified_on, to(binary,AccessRecord#access.modified_on)}]}
   end,
  lists:map(TupleListFunction, AccessRecords).
