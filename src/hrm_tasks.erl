-module(hrm_tasks).

-export([create/1, fields/0, field_value/2]).

% Create and enqueue task
create(NewTask) ->
    NewTask2 = [{status, pending} | NewTask],
    create(NewTask2, validate(NewTask2)).

fields() ->
    [action_url, callback_url, instance_id, access_key_id, access_key_secret, status, meta].

field_value(Field, Task) ->
    case proplists:get_value(Field, Task) of
        Value when is_binary(Value) -> binary_to_list(Value);
        Value -> Value
    end.

%% ===================================================================
%% Private
%% ===================================================================

create(NewTask, []) ->
    NewTaskId = uuid:uuid_to_string(uuid:get_v4()),
    {ok, NewTask2} = hrm_storage:store(NewTaskId, NewTask),
    hrm_tasks_sup:start_task(NewTaskId, NewTask2),
    {ok, NewTaskId};

create(_NewTask, Errors) ->
    {errors, Errors}.

% Returns list of errors for Task, may be empty list.
validate(Task) ->
    FieldValidator = fun(Field) ->
        {Field, validate_field(Task, Field, field_value(Field, Task))}
    end,
    lists:filter(fun({_, Error}) ->
        Error =/= ok
    end, lists:map(FieldValidator, fields())).

% Validation of specific fields

validate_field(_Task, action_url, undefined) ->
    undefined;

validate_field(_Task, action_url, Url) ->
    validate_url(Url);

validate_field(_Task, callback_url, undefined) ->
    undefined;

validate_field(_Task, callback_url, Url) ->
    validate_url(Url);

validate_field(Task, access_key_id, Key) ->
    validate_defined_if(Key, proplists:is_defined(instance_id, Task));

validate_field(Task, access_key_secret, Key) ->
    validate_defined_if(Key, proplists:is_defined(instance_id, Task));

validate_field(_Task, status, Status) ->
    validate_inclusion(Status, [pending, in_progress, complete, error]);

validate_field(_, _, _) ->
    ok.

% Validation methods

validate_url(Url) ->
    case uri:parse(Url) of
        {ok, _} -> ok;
        {error, _} -> malformed_url
    end.

validate_defined_if(undefined, true) ->
    undefined;

validate_defined_if(_Value, _Cond) ->
    ok.

validate_inclusion(Value, List) ->
    case proplists:get_bool(Value, List) of
        true -> ok;
        false -> unknown
    end.
