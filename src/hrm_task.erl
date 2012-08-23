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
    {ok, {{_, StatusCode, StatusText}, _, _}} = httpc:request(hrm_tasks:field_value(action_url, Task)),
    process_action_response({StatusCode, StatusText}, TaskId, Task),
    ok.

process_action_response({200, _}, TaskId, Task) ->
    ok = hrm_storage:append(TaskId, [{status, complete}]),
    ok = do_callback(TaskId, Task),
    ok;

process_action_response({StatusCode, StatusText}, TaskId, Task) ->
    Meta = {[{status, StatusCode}, {message, list_to_binary(StatusText)}]},
    ok = hrm_storage:append(TaskId, [{status, error}, {meta, Meta}]),
    ok = do_callback(TaskId, Task),
    ok.

do_callback(_TaskId, Task) ->
    {ok, _} = httpc:request(hrm_tasks:field_value(callback_url, Task)),
    ok.

%% Boilerplate

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
