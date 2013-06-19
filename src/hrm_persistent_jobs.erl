-module(hrm_persistent_jobs).

-behavior(e2_service).
-behavior(e2_task).

-export([start/3, status/0, status/1]).

-export([start_link/1]).
-export([init/1, handle_msg/3]).
-export([handle_task/1]).

%%%===================================================================
%%% Public API
%%%===================================================================

start(Module, Method, Args) ->
  start([Module, Method, Args]).

start(Spec) ->
  e2_service:cast(?MODULE, {start, Spec}).

status() ->
  e2_service:call(?MODULE, status).

status(SpecPattern) ->
  e2_service:call(?MODULE, {status, SpecPattern}).

start_link(File) ->
  {ok, Pid} = e2_service:start_link(?MODULE, File, [registered]),
  e2_task:start_link(?MODULE, e2_task),
  {ok, Pid}.

%%%===================================================================
%%% e2_service callbacks
%%%===================================================================

init(e2_task) ->
  {ok, []};
init(File) ->
  {ok, Db} = dets:open_file(File, []),
  {ok, Db}.

handle_msg({start, [Module, Method, Args] = Spec}, _From, Db) ->
  {ok, Pid} = apply(Module, Method, Args),
  monitor(process, Pid),
  dets:insert(Db, {Pid, Spec, running}),
  {noreply, Db};

handle_msg(status, _From, Db) ->
  Children = dets:match(Db, {'_', '$1', running}),
  {reply, Children, Db};

handle_msg({status, SpecPattern}, _From, Db) ->
  Children = dets:match(Db, {'_', SpecPattern, running}),
  {reply, Children, Db};

handle_msg(extract_lost_children, _From, Db) ->
  LostChildren = lists:filter(fun([Pid, _]) ->
    case is_process_alive(Pid) of
      true -> false;
      false -> dets:delete(Db, Pid), true
    end
  end, dets:match(Db, {'$1', '$2', running})),
  Reply = lists:map(fun([_, Spec]) -> Spec end, LostChildren),
  {reply, Reply, Db};

handle_msg({'DOWN', _Ref, process, Pid, normal}, _From, Db) ->
  dets:delete(Db, Pid),
  {noreply, Db};

handle_msg({'DOWN', _Ref, process, Pid, Reason}, _From, Db) ->
  case dets:lookup(Db, Pid) of
    [{Pid, Spec, running}] -> dets:insert(Db, {Pid, Spec, Reason})
  end,
  {noreply, Db}.

%%%===================================================================
%%% e2_task callbacks
%%%===================================================================

handle_task(_) ->
  Specs = e2_service:call(?MODULE, extract_lost_children),
  lists:foreach(fun start/1, Specs),
  {stop, normal}.
