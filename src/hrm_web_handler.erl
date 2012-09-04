-module(hrm_web_handler).

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
    {ok, Task} -> reply(hrm_task:to_json(Task), Req);
    {error, notfound} -> reply(<<"">>, Req, 404)
  end;

% Create task
handle_method('POST', _TaskId, Req) ->
  [ActionUrl, CallbackUrl, InstanceId, AccessKeyId, AccessKeySecret] = post_values(Req, [
    action_url, callback_url, instance_id, access_key_id, access_key_secret
  ]),
  case hrm_task:create(ActionUrl, CallbackUrl, InstanceId, AccessKeyId, AccessKeySecret) of
    {ok, NewTaskId} -> reply(NewTaskId, Req);
    {errors, Errors} -> reply(jiffy:encode({[{errors, {Errors}}]}), Req, 400)
  end;

% Update task with meta information
handle_method('PUT', TaskId, Req) ->
  [Meta] = post_values(Req, [meta]),
  {ok, _} = hrm_task:update_meta(TaskId, Meta),
  {ok, Req};

% Delete task
handle_method('DELETE', TaskId, Req) ->
  {ok, _} = hrm_storage:delete(TaskId),
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

post_values(Req, Fields) ->
  {PostVals, _} = cowboy_http_req:body_qs(Req),
  lists:map(fun(Field) ->
    hrm_utils:thing_to_list(proplists:get_value(atom_to_binary(Field, utf8), PostVals))
  end, Fields).

task_id_from_req(Req) ->
  case cowboy_http_req:binding(task, Req) of
    {undefined, _} -> undefined;
    {TaskId, _} -> binary_to_list(TaskId)
  end.
        