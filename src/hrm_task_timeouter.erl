-module(hrm_task_timeouter).

-behaviour(e2_task).

-export([start_link/2]).
-export([handle_task/1]).

-include("../include/hrm_task.hrl").

%%%===================================================================
%%% Public API
%%%===================================================================

start_link(Pid, Task) ->
  e2_task:start_link(?MODULE, [Pid, Task], [
    {delay, Task#task.timeout * 1000}
  ]).

%%%===================================================================
%%% e2_task callbacks
%%%===================================================================

handle_task([Pid, Task]) ->
  case hrm_storage:get(Task#task.id) of
    #task{status=pending} ->
      exit(Pid, kill),
      hrm_storage:update(Task#task.id, fun (Task2) ->
        Task2#task{status = timeout}
      end);
    _ -> ok
  end,
  {stop, normal}.
