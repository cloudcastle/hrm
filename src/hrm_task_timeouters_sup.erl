-module(hrm_task_timeouters_sup).

-behaviour(e2_task_supervisor).

-export([start_link/0, start_task_timeouter/3]).

start_link() ->
  e2_task_supervisor:start_link(?MODULE, hrm_task_timeouter, [registered]).

start_task_timeouter(Pid, Task, StartDate) ->
  e2_task_supervisor:start_task(?MODULE, [Pid, Task, StartDate]).
