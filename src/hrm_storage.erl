-module(hrm_storage).

-behavior(e2_service).

-export([get/1, put/1, update/2, delete/1, match/1]).

-export([start_link/1]).
-export([init/1, handle_msg/3]).

-include("../include/hrm_task.hrl").

%%%===================================================================
%%% Public API
%%%===================================================================

get(Id) ->
  e2_service:call(?MODULE, {get, Id}).

put(Task) ->
  e2_service:call(?MODULE, {put, Task}).

update(Id, Fun) ->
  e2_service:call(?MODULE, {update, Id, Fun}).

delete(Id) ->
  e2_service:call(?MODULE, {delete, Id}).

match(Pattern) ->
  e2_service:call(?MODULE, {match, Pattern}).

start_link(File) ->
  e2_service:start_link(?MODULE, File, [registered]).

%%%===================================================================
%%% e2_service callbacks
%%%===================================================================

init(File) ->
  {ok, Db} = dets:open_file(File, []),
  {ok, Db}.

handle_msg({get, Id}, _From, Db) ->
  Response = case dets:lookup(Db, Id) of
    [{Id, Task}] ->
      {ok, Task};
    [] ->
      {error, notfound}
  end,
  {reply, Response, Db};

handle_msg({put, Task}, _From, Db) ->
  ok = dets:insert(Db, {Task#task.id, Task}),
  {reply, {ok, Task}, Db};

handle_msg({update, Id, Fun}, _From, Db) ->
  Response = case dets:lookup(Db, Id) of
    [{Id, Task}] ->
      ok = dets:insert(Db, {Id, Fun(Task)}),
      {ok, Task};
    [] ->
      {error, notfound}
  end,
  {reply, Response, Db};

handle_msg({delete, Id}, _From, Db) ->
  Response = case dets:lookup(Db, Id) of
    [{Id, Task}] ->
      ok = dets:delete(Db, Id),
      {ok, Task};
    [] ->
      {error, notfound}
  end,
  {reply, Response, Db};

handle_msg({match, Pattern}, _From, Db) ->
  Response = dets:match(Db, Pattern),
  {reply, Response, Db}.
