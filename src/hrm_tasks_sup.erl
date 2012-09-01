-module(hrm_tasks_sup).

-behaviour(e2_task_supervisor).

-export([start_link/0, start_task/2]).

start_link() ->
  e2_task_supervisor:start_link(?MODULE, hrm_task, [registered]).

start_task(TaskId, Task) ->
  e2_task_supervisor:start_task(?MODULE, [TaskId, Task]).
