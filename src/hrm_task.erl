-module(hrm_task).

-behaviour(e2_task).

-export([create/2, create/6, update_meta/2, to_json/1]).

-export([start_link/1]).
-export([handle_task/1]).

-define(EC2_POLL_INTERVAL, 5000).
-define(EC2_INIT_DELAY,   10000).

-include("../include/hrm_task.hrl").

%%%===================================================================
%%% Public API
%%%===================================================================

create(ActionUrl, CallbackUrl) ->
  create(ActionUrl, CallbackUrl, undefined, undefined, undefined, undefined).

create(ActionUrl, CallbackUrl, InstanceId, AccessKeyId, AccessKeySecret, Timeout) ->
  Task = #task{
    id = uuid:uuid_to_string(uuid:get_v4()),
    action_url = ActionUrl,
    callback_url = CallbackUrl,
    instance_id = InstanceId,
    access_key_id = AccessKeyId,
    access_key_secret = AccessKeySecret,
    status = pending,
    started_at = hrm_utils:current_time(),
    timeout = case Timeout of
      Timeout when is_list(Timeout) -> list_to_integer(Timeout);
      Timeout when is_integer(Timeout) -> Timeout;
      _ -> 60 * 30 % defaut to 30 minutes
    end
  },
  case validate(Task) of
    [] ->
      hrm_storage:put(Task),
      case hrm_tasks_sup:start_task_runner(Task) of
        {ok, Pid} ->
          hrm_persistent_jobs:start(hrm_task_timeouters_sup, start_task_timeouter, [Pid, Task, calendar:universal_time()]),
          {ok, Task#task.id};
        {error, Reason} ->
          {errors, Reason}
      end;
    Errors ->
      {errors, Errors}
  end.

update_meta(TaskId, Meta) ->
  hrm_storage:update(TaskId, fun (Task) ->
    Task#task{meta = Meta}
  end).

to_json(Task) when is_record(Task, task) ->
  Pairs = lists:map(fun({K, V}) ->
    {K, to_json(V)}
  end, proplists:delete(access_key_secret, to_proplist(Task))),
  jiffy:encode({Pairs});

to_json(Value) when is_list(Value) -> list_to_binary(Value);
to_json(undefined) -> null;
to_json(Value) -> Value.

start_link(Task) ->
  e2_task:start_link(?MODULE, [Task]).

%%%===================================================================
%%% e2_task callbacks
%%%===================================================================

handle_task([Task]) ->
  Task2 = try do_action(Task) of
    Value -> Value
  catch
    _:Error ->
      error_logger:error_msg("*** Error during task action:~n~p~n*** Stacktrace:~n~p~n", [Error, erlang:get_stacktrace()]),
      Task#task{status = error}
  end,
  hrm_storage:put(Task2),
  Task3 = do_callback_request(Task2),
  hrm_storage:put(Task3),
  {stop, normal}.

%%%===================================================================
%%% Private
%%%===================================================================

to_proplist(Task) ->
  lists:zip(record_info(fields, task), tl(tuple_to_list(Task))).

do_action(Task) ->
  ok = ensure_instance(
    Task#task.instance_id,
    Task#task.access_key_id,
    Task#task.access_key_secret
  ),
  do_action_request(Task).

%%% ensure_instance/3

ensure_instance(undefined, _, _) ->
  ok;
ensure_instance(InstanceId, AccessKeyId, AccessKeySecret) ->
  EC2 = erlcloud_ec2:new(AccessKeyId, AccessKeySecret),
  handle_instance_state({unknown, get_instance_state(InstanceId, EC2)}, InstanceId, EC2).

handle_instance_state({_, stopped}, InstanceId, EC2) ->
  [[_, {current_state, _, {name, State}}, {previous_state, _, {name, PrevState}}]] = erlcloud_ec2:start_instances([InstanceId], EC2),
  handle_instance_state({list_to_atom(PrevState), list_to_atom(State)}, InstanceId, EC2);

handle_instance_state({stopped, pending}, InstanceId, EC2) ->
  hrm_persistent_jobs:start(hrm_stoppers_sup, start_stopper, [InstanceId, EC2, calendar:universal_time()]),
  timer:sleep(?EC2_POLL_INTERVAL),
  handle_instance_state({pending, get_instance_state(InstanceId, EC2)}, InstanceId, EC2);

handle_instance_state({_, S}, InstanceId, EC2) when S == pending; S == stopping ->
  timer:sleep(?EC2_POLL_INTERVAL),
  handle_instance_state({S, get_instance_state(InstanceId, EC2)}, InstanceId, EC2);

handle_instance_state({pending, running}, _, _) ->
  timer:sleep(?EC2_INIT_DELAY),
  ok;

handle_instance_state({_, running}, _, _) ->
  ok.

%%% do_action_request/1

do_action_request(Task) ->
  case httpc:request(build_action_url(Task)) of
    {ok, {{_, StatusCode, _}, _, Body}} ->
      Task#task{
        status = case StatusCode of
          200 -> complete;
          _ -> error
        end,
        completed_at = hrm_utils:current_time(),
        meta = list_to_binary(io_lib:format("[~b] ~s", [StatusCode, Body]))
      };
    {error, Reason} ->
      Task#task{
        status = error,
        completed_at = hrm_utils:current_time(),
        meta = list_to_binary(io_lib:format("~p", [Reason]))
      }
  end.

build_action_url(Task) ->
  Url = case re:run(Task#task.action_url, "{\\w+}") of
    nomatch -> Task#task.action_url;
    {match, _} -> hrm_utils:replace_tokens_from_proplist(Task#task.action_url, get_instance_fields(get_instance_data(Task)))
  end,
  hrm_utils:append_query_params(Url, [{hrm_task_id, Task#task.id}]).

%%% do_callback_request/1

do_callback_request(#task{callback_url=undefined}=Task) ->
  Task;
do_callback_request(Task) ->
  Url = hrm_utils:append_query_params(Task#task.callback_url, [{hrm_task_id, Task#task.id}]),
  case httpc:request(Url) of
    {ok, {{_, StatusCode, _}, _, Body}} ->
      Task#task{
        callback_status = case StatusCode of
          200 -> complete;
          _ -> list_to_binary(io_lib:format("[~b] ~s", [StatusCode, Body]))
        end
      };
    {error, Reason} ->
      Task#task{status = list_to_binary(io_lib:format("~p", [Reason]))}
  end.

%%% validate/1

validate(Task) ->
  Results = lists:map(fun({K, V}) ->
    {K, validate_field(K, V, Task)}
  end, to_proplist(Task)),
  lists:filter(fun({_, Result}) ->
    Result =/= ok
  end, Results).

validate_field(action_url,   Url, _) -> validate_url(Url);

validate_field(callback_url, undefined, _) -> ok;
validate_field(callback_url, Url, _) -> validate_url(Url);

validate_field(access_key_id, _, #task{instance_id=undefined}) -> ok;
validate_field(access_key_id, V, _) -> validate_presence(V);
validate_field(access_key_secret, _, #task{instance_id=undefined}) -> ok;
validate_field(access_key_secret, V, _) -> validate_presence(V);

validate_field(instance_id, undefined, _) -> ok;
validate_field(instance_id, InstanceId, #task{access_key_id=AccessKeyId, access_key_secret=AccessKeySecret}) ->
  validate_ec2_instance(InstanceId, AccessKeyId, AccessKeySecret);

validate_field(_, _, _) -> ok.

%%% get_instance_state/2

get_instance_state(InstanceId, EC2) ->
  [InstanceData] = erlcloud_ec2:describe_instances([InstanceId], EC2),
  get_instance_state(InstanceData).

get_instance_state(InstanceData) ->
  InstanceState = proplists:get_value(instance_state, get_instance_fields(InstanceData)),
  list_to_atom(proplists:get_value(name, InstanceState)).

get_instance_data(#task{instance_id=undefined}) ->
  [];
get_instance_data(Task) ->
  EC2 = erlcloud_ec2:new(Task#task.access_key_id, Task#task.access_key_secret),
  [InstanceData] = erlcloud_ec2:describe_instances([Task#task.instance_id], EC2),
  InstanceData.

get_instance_fields(InstanceData) ->
  [Fields|_] = proplists:get_value(instances_set, InstanceData),
  Fields.

%%%===================================================================
%%% Common validation methods
%%%===================================================================

validate_presence(undefined) -> undefined;
validate_presence(_) -> ok.

validate_url(undefined) ->
  undefined;
validate_url(Url) ->
  case uri:parse(Url) of
    {ok, _} -> ok;
    {error, _} -> malformed_url
  end.

validate_ec2_instance(_, AccessKeyId, AccessKeySecret) when
  AccessKeyId     == undefined;
  AccessKeySecret == undefined ->
  ok;

validate_ec2_instance(InstanceId, AccessKeyId, AccessKeySecret) ->
  EC2 = erlcloud_ec2:new(AccessKeyId, AccessKeySecret),
  try erlcloud_ec2:describe_instances([InstanceId], EC2) of
    [] -> not_found;
    [InstanceData] -> validate_instance_state(get_instance_state(InstanceData))
  catch
    throw:{error, {"403", _}, _} -> forbidden
  end.

validate_instance_state(State) when
  State == stopped;
  State == pending;
  State == stopping;
  State == running
  -> ok;
validate_instance_state(_) -> unexpected_state.
