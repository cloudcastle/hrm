-module(hrm_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

% Entry point
start() ->
    ok = application:start(cowboy),
    ok = inets:start(),
    ok = application:start(hrm_app).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = cowboy:start_listener(hrm_http_listener, 100,
        cowboy_tcp_transport, [{port, 8080}],
        cowboy_http_protocol, [{dispatch, [{'_', [
            {[<<"tasks">>], tasks_handler, []},
            {[<<"tasks">>, task], tasks_handler, []}
        ]}]}]
    ),
    ok = hrm_storage:start(),
    {ok, _} = hrm_sup:start_link().

stop(_State) ->
    ok.
