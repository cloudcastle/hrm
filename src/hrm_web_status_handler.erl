-module(hrm_web_status_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-include("../include/hrm_task.hrl").

%% ===================================================================
%% Cowboy handler callbacks
%% ===================================================================

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {ok, Req3} = handle_method(binary_to_atom(Method, utf8), Req2),
  {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%% ===================================================================
%% API methods
%% ===================================================================

% Get JSON status/info of task by ID
handle_method('GET', Req) ->
  StoppersSpecsList = hrm_persistent_jobs:status(),
  StopperInstances = lists:map(fun([[hrm_stoppers_sup, start_stopper, [InstanceId | _]]]) ->
    list_to_binary(InstanceId)
  end, StoppersSpecsList),
  PendingTasksList = hrm_storage:match({'$1', #task{instance_id='$2', status=pending, _='_'}}),
  PendingTasks = lists:map(fun
      ([TaskId, undefined]) -> {list_to_binary(TaskId), null};
      ([TaskId, InstanceId]) -> {list_to_binary(TaskId), list_to_binary(InstanceId)}
  end, PendingTasksList),
  reply(jiffy:encode({[
    {controlled_instances, StopperInstances},
    {pending_tasks, {PendingTasks}}
  ]}), Req);

% Method not allowed.
handle_method(_, Req) ->
  cowboy_req:reply(405, Req).

%% ===================================================================
%% Private
%% ===================================================================

reply(Content, Req) ->
  reply(Content, Req, 200).

reply(Content, Req, Status) ->
  cowboy_req:reply(Status, [
    {"Content-Encoding", "utf-8"},
    {"Content-Type", "application/json"}
  ], Content, Req).
