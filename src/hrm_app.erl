-module(hrm_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

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
    hrm_sup:start_link().

stop(_State) ->
    ok.
