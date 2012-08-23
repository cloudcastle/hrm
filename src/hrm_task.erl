-module(hrm_task).
-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {id, task}).

start_link(TaskId, Task) ->
    gen_server:start_link(?MODULE, [TaskId, Task], []).

init([TaskId, Task]) ->
    {ok, #state{id=TaskId, task=Task}, 0}.

handle_info(_Info, #state{id=TaskId, task=Task}=State) ->
    do_action_request(TaskId, Task),
    {stop, normal, State}.

do_action_request(TaskId, Task) ->
    Url = hrm_utils:append_query_params(hrm_tasks:field_value(action_url, Task), [{hrm_task_id, TaskId}]),
    {ok, {{_, StatusCode, StatusText}, _, _}} = httpc:request(Url),
    process_action_response({StatusCode, StatusText}, TaskId),
    ok.

process_action_response({200, _}, TaskId) ->
    {ok, Task2} = hrm_storage:append(TaskId, [{status, complete}]),
    ok = do_callback(TaskId, Task2),
    ok;

process_action_response({StatusCode, StatusText}, TaskId) ->
    Meta = {[{status, StatusCode}, {message, list_to_binary(StatusText)}]},
    {ok, Task2} = hrm_storage:append(TaskId, [{status, error}, {meta, Meta}]),
    ok = do_callback(TaskId, Task2),
    ok.

do_callback(TaskId, Task) ->
    Params = [{hrm_task_id, TaskId}, {hrm_task, jiffy:encode({Task})}],
    Url = hrm_utils:append_query_params(hrm_tasks:field_value(callback_url, Task), Params),
    {ok, _} = httpc:request(Url),
    ok.

%% Boilerplate

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
