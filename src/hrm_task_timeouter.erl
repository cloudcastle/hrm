-module(hrm_task_timeouter).

-behaviour(e2_task).

-export([start_link/3]).
-export([handle_task/1]).

-include("../include/hrm_task.hrl").

%%%===================================================================
%%% Public API
%%%===================================================================

start_link(Pid, Task, StartDate) ->
  e2_task:start_link(?MODULE, [Pid, Task#task.id], [
    {delay, delay_by_start_time(StartDate, Task#task.timeout)}
  ]).

%%%===================================================================
%%% e2_task callbacks
%%%===================================================================

handle_task([Pid, TaskId]) ->
  case hrm_storage:get(TaskId) of
    {ok, #task{status=pending}} ->
      exit(Pid, kill),
      hrm_storage:update(TaskId, fun (Task) ->
        hrm_task:do_callback_request(Task#task{status = timeout})
      end);
    _ -> ok
  end,
  {stop, normal}.

%%%===================================================================
%%% Private
%%%===================================================================

delay_by_start_time(StartTime, Timeout) ->
  TimeoutTs = calendar:datetime_to_gregorian_seconds(StartTime) + Timeout,
  CurrentTs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  if
    TimeoutTs > CurrentTs ->
      (TimeoutTs - CurrentTs) * 1000;
    true -> 0
  end.
