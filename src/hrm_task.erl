-module(hrm_task).
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {id, task}).

-define(EC2_INTERVAL, 3000).

start_link(TaskId, Task) ->
    gen_server:start_link(?MODULE, [TaskId, Task], []).

init([TaskId, Task]) ->
    {ok, #state{id=TaskId, task=Task}, 0}.

handle_info(_Info, #state{id=TaskId, task=Task}=State) ->
    ok = ensure_instance(Task),
    ok = do_action_request(TaskId, Task),
    {stop, normal, State}.

do_action_request(TaskId, Task) ->
    Url = hrm_utils:append_query_params(hrm_tasks:field_value(action_url, Task), [{hrm_task_id, TaskId}]),
    {ok, {{_, StatusCode, StatusText}, _, _}} = httpc:request(Url),
    {ok, Task2} = if
      StatusCode == 200 ->
        hrm_storage:append(TaskId, [{status, complete}]);
      true ->
        Meta = {[{status, StatusCode}, {message, list_to_binary(StatusText)}]},
        hrm_storage:append(TaskId, [{status, error}, {meta, Meta}])
    end,
    Params = [{hrm_task_id, TaskId}, {hrm_task, jiffy:encode({Task2})}],
    Url2 = hrm_utils:append_query_params(hrm_tasks:field_value(callback_url, Task2), Params),
    {ok, _} = httpc:request(Url2),
    ok.

ensure_instance(Task) ->
  case hrm_tasks:field_value(instance_id, Task) of
    undefined ->
      ok;
    InstanceId ->
      EC2 = erlaws_ec2:new(hrm_tasks:field_value(access_key_id, Task), hrm_tasks:field_value(access_key_secret, Task), true),
      wait_for_instance(InstanceId, EC2)
  end.

wait_for_instance(InstanceId, EC2) ->
  [InstanceData] = EC2:describe_instances([InstanceId]),
  State = proplists:get_value(instance_state, InstanceData),
  io:format("State: ~p~n", [State]),
  case State of
    "stopped" ->
      EC2:start_instances([InstanceId]),
      timer:sleep(?EC2_INTERVAL),
      wait_for_instance(InstanceId, EC2);
    "pending" ->
      timer:sleep(?EC2_INTERVAL),
      wait_for_instance(InstanceId, EC2);
    "stopping" ->
      timer:sleep(?EC2_INTERVAL),
      wait_for_instance(InstanceId, EC2);
    "running" -> ok
  end.

%% Boilerplate

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
