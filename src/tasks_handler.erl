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
    {TaskId, Req3} = cowboy_http_req:binding(task, Req2),
    {ok, Req4} = handle_method(Method, TaskId, Req3),
    {ok, Req4, State}.

terminate(_Req, _State) ->
    ok.

%% ===================================================================
%% API methods
%% ===================================================================

% Get JSON status/info of task by ID
handle_method('GET', TaskId, Req) ->
    {ok, Task} = hrm_storage:get(TaskId),
    reply(jiffy:encode(Task), Req);

% Create task
handle_method('POST', _TaskId, Req) ->
    NewTask = task_from_req(Req),
    case hrm:create_task(NewTask) of
        {ok, NewTaskId} -> reply(<<NewTaskId/binary>>, Req);
        {errors, Errors} -> reply(jiffy:encode({[{errors, Errors}]}), Req, 400)
    end;

% Update task, e.g. meta information
handle_method('PUT', TaskId, Req) ->
    {ok, OldTask} = hrm_storage:get(TaskId),
    NewTask = task_from_req(Req) ++ OldTask,
    ok = hrm_storage:store(TaskId, NewTask),
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
    Aliases = [{atom_to_binary(Field, utf8), Field} || Field <- hrm:task_fields()],
    proplists:substitute_aliases(Aliases, PostVals).