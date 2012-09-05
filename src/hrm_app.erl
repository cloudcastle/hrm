-module(hrm_app).

-behavior(e2_application).

-export([init/0]).

%%%===================================================================
%%% e2_application callbacks
%%%===================================================================

init() ->
  {ok, _} = cowboy:start_listener(hrm_http_listener, 100,
    cowboy_tcp_transport, [{port, 8080}],
    cowboy_http_protocol, [{dispatch, [{'_', [
      {[<<"tasks">>], hrm_web_handler, []},
      {[<<"tasks">>, task], hrm_web_handler, []}
    ]}]}]
  ),
  {ok, [
    {hrm_storage, start_link, [config_value(db_file)]},
    hrm_tasks_sup,
    hrm_stoppers_sup
  ]}.

config_value(Key) ->
  handle_app_env(application:get_env(Key), Key).

handle_app_env({ok, Value}, _Key) -> Value;
handle_app_env(undefined, Key) -> throw({config_value_undefined, Key}).