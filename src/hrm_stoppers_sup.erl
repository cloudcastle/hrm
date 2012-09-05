-module(hrm_stoppers_sup).

-behaviour(e2_task_supervisor).

-export([start_link/0, start_stopper/2]).

start_link() ->
  e2_task_supervisor:start_link(?MODULE, hrm_stopper, [registered]).

start_stopper(InstanceId, EC2) ->
  e2_task_supervisor:start_task(?MODULE, [InstanceId, EC2]).
