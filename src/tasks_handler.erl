-module(tasks_handler).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

%% ===================================================================
%% Cowboy handler callbacks
%% ===================================================================

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_http_req:method(Req),
    {ok, Req3} = handle_method(Method, task_id_from_req(Req2), Req2),
    {ok, Req3, State}.

terminate(_Req, _State) ->
    ok.

%% ===================================================================
%% API methods
%% ===================================================================

% Get JSON status/info of task by ID
handle_method('GET', TaskId, Req) ->
    case hrm_storage:get(TaskId) of
        {ok, Task} -> reply(jiffy:encode({Task}), Req);
        {error, notfound} -> reply(<<"">>, Req, 404)
    end;

% Create task
handle_method('POST', _TaskId, Req) ->
    NewTask = task_from_req(Req),
    case hrm_tasks:create(NewTask) of
        {ok, NewTaskId} -> reply(NewTaskId, Req);
        {errors, Errors} -> reply(jiffy:encode({[{errors, {Errors}}]}), Req, 400)
    end;

% Update task, e.g. meta information
handle_method('PUT', TaskId, Req) ->
    ok = hrm_storage:append(TaskId, task_from_req(Req)),
    {ok, Req};

% Delete task
handle_method('DELETE', TaskId, Req) ->
    ok = hrm_storage:delete(TaskId),
    {ok, Req};

% Method not allowed.
handle_method(_, _, Req) ->
    cowboy_http_req:reply(405, Req).

%% ===================================================================
%% Private
%% ===================================================================

reply(Content, Req) ->
    reply(Content, Req, 200).

reply(Content, Req, Status) ->
    cowboy_http_req:reply(Status, [
        {<<"Content-Encoding">>, <<"utf-8">>},
        {<<"Content-Type">>, <<"application/json">>}
    ], Content, Req).

task_from_req(Req) ->
    {PostVals, _} = cowboy_http_req:body_qs(Req),
    Aliases = [{atom_to_binary(Field, utf8), Field} || Field <- hrm_tasks:fields()],
    proplists:substitute_aliases(Aliases, PostVals).

task_id_from_req(Req) ->
    {TaskId, _} = cowboy_http_req:binding(task, Req),
    if
        TaskId =:= undefined -> undefined;
        true -> binary_to_list(TaskId)
    end.
        