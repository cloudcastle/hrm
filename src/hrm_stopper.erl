-module(hrm_stopper).

-behaviour(e2_task).

-export([start_link/3, is_instance_in_use/1]).
-export([handle_task/1]).

-include("../include/hrm_task.hrl").

%%%===================================================================
%%% Public API
%%%===================================================================

start_link(InstanceId, EC2, StartTime) ->
  e2_task:start_link(?MODULE, [InstanceId, EC2], [
    {delay, delay_by_start_time(StartTime)}, % first attempt 55 minutes after start
    {repeat, 60 * 60 * 1000} % next attempts each 60 minutes
  ]).

%%%===================================================================
%%% e2_task callbacks
%%%===================================================================

handle_task([InstanceId, EC2] = State) ->
  case is_instance_in_use(InstanceId) of
    true ->
      {repeat, State};
    false ->
      [{InstanceId, "stopping", _}] = EC2:stop_instances(InstanceId, "false"),
      {stop, normal}
  end.

%%%===================================================================
%%% Private
%%%===================================================================

is_instance_in_use(InstanceId) ->
  PendingForInstancePattern = #task{instance_id=InstanceId, status=pending, _='_'},
  hrm_storage:match({'$1', PendingForInstancePattern}) =/= [].

delay_by_start_time(StartTime) ->
  {_, {_, StartMinutes, _}} = StartTime,
  {_, {_, NowMinutes, _}} = calendar:universal_time(),
  ((55 - NowMinutes + StartMinutes) rem 60) * 60 * 1000.
