-module(hrm_stoppers_sup).

-behaviour(e2_task_supervisor).

-export([start_link/0, start_stopper/2, start_stopper/3]).

start_link() ->
  e2_task_supervisor:start_link(?MODULE, hrm_stopper, [registered]).

% deprecated
start_stopper(InstanceId, EC2) ->
  start_stopper(InstanceId, EC2, calendar:universal_time()).

start_stopper(InstanceId, EC2, StartTime) ->
  e2_task_supervisor:start_task(?MODULE, [InstanceId, EC2, StartTime]).
