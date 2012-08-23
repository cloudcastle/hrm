-module(hrm_tasks_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(TaskId, Task) ->
    supervisor:start_child(?SERVER, [TaskId, Task]).

init([]) ->
    {ok, {{simple_one_for_one, 0, 1}, [{hrm_task, {hrm_task, start_link, []}, temporary, brutal_kill, worker, [hrm_task]}]}}.