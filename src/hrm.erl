-module(hrm).

-export([start/0, create_task/1]).

% Start HRM application
start() ->
    ok = application:start(cowboy),
    ok = application:start(hrm_app).

% Create and enqueue task
create_task(NewTask) ->
    NewTask2 = [{status, pending} | NewTask],
    create_task(NewTask2, hrm_tasks:validate(NewTask2)).

%% ===================================================================
%% Private
%% ===================================================================

create_task(NewTask, []) ->
    NewTaskId = uuid:uuid_to_string(uuid:get_v4()),
    hrm_storage:store(NewTaskId, NewTask),
    io:format("Enqueuing task '~p' with key '~p'~n", [NewTask, NewTaskId]), % todo
    {ok, NewTaskId};

create_task(_NewTask, Errors) ->
    {errors, Errors}.
