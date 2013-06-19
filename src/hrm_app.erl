-module(hrm_app).

-behavior(e2_application).

-export([init/0]).

%%%===================================================================
%%% e2_application callbacks
%%%===================================================================

init() ->
  Dispatch = cowboy_router:compile([{'_', [
    {"/tasks", hrm_web_tasks_handler, []},
    {"/tasks/:task", hrm_web_tasks_handler, []},
    {"/status", hrm_web_status_handler, []}
  ]}]),
  {ok, _} = cowboy:start_http(hrm_http_listener, 100,
    [{port, config_value(http_port)}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  {ok, [
    {hrm_storage, start_link, [config_value(db_file)]},
    hrm_tasks_sup,
    hrm_stoppers_sup,
    hrm_task_timeouters_sup,
    {hrm_persistent_jobs, start_link, [config_value(jobs_db_file)]}
  ]}.

config_value(Key) ->
  handle_app_env(application:get_env(Key), Key).

handle_app_env({ok, Value}, _Key) -> Value;
handle_app_env(undefined, Key) -> throw({config_value_undefined, Key}).