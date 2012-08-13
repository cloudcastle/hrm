-module(hrm).

-export([start/0, create_task/1, task_fields/0]).

% Start HRM application
start() ->
    ok = application:start(cowboy),
    ok = application:start(hrm_app).

% Create and enqueue task
create_task(NewTask) ->
    create_task(NewTask, errors_for_task(NewTask)).

task_fields() ->
    [action_url, callback_url, instance_id, access_key_id, access_key_secret, state, meta].

%% ===================================================================
%% Private
%% ===================================================================

create_task(NewTask, []) ->
    NewTaskId = uuid:uuid_to_string(uuid:get_v4()),
    hrm_storage:store(NewTaskId, [{state, pending} | NewTask]),
    io:format("Enqueuing task '~p' with key '~p'~n", [NewTask, NewTaskId]), % todo
    {ok, NewTaskId};

create_task(_NewTask, Errors) ->
    {errors, Errors}.

errors_for_task(Task) ->
    errors_for_task(Task, task_fields(), []).

errors_for_task(_Task, [], Errors) ->
    lists:reverse(Errors);

errors_for_task(Task, [Field | Fields], Errors) ->
    case proplists:is_defined(Field, Task) of
      true -> errors_for_task(Task, Fields, Errors);
      _ -> errors_for_task(Task, Fields, [Field | Errors])
    end.
