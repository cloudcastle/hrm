-module(hrm_storage_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../include/hrm_task.hrl").

storage_test() ->
  {ok, _} = hrm_storage:start_link(os:cmd("mktemp /tmp/hrm_storage.db.XXXX")),

  ?assertEqual({error, notfound}, hrm_storage:get("foo")),
  ?assertEqual({error, notfound}, hrm_storage:update("foo", fun(Task) -> Task end)),
  ?assertEqual({error, notfound}, hrm_storage:delete("foo")),
  ?assertEqual([], hrm_storage:match('_')),

  ?assertEqual({ok, #task{id="foo"}}, hrm_storage:put(#task{id="foo"})),
  ?assertEqual({ok, #task{id="foo"}}, hrm_storage:get("foo")),
  
  ?assertEqual({ok, #task{id="foo", status=pending}}, hrm_storage:update("foo", fun(Task) -> Task#task{status=pending} end)),

  hrm_storage:put(#task{id="bar"}),

  ?assertEqual([["foo", pending], ["bar", undefined]], hrm_storage:match({'$1', #task{status='$2', _='_'}})),

  ?assertEqual({ok, #task{id="bar"}}, hrm_storage:delete("bar")),
  
  ok.
