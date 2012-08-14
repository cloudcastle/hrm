-module(hrm_tasks).

-export([fields/0, validate/1]).

fields() ->
    [action_url, callback_url, instance_id, access_key_id, access_key_secret, status, meta].

% Returns list of errors for Task, may be empty list.
validate(Task) ->
    FieldValidator = fun(Field) ->
        {Field, validate_field(Task, Field, proplists:get_value(Field, Task))}
    end,
    lists:filter(fun({Field, Error}) ->
        Error =/= ok
    end, lists:map(FieldValidator, fields())).

%% ===================================================================
%% Private
%% ===================================================================

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
    case uri:parse(binary_to_list(Url)) of
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
