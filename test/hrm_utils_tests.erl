-module(hrm_utils_tests).

-include_lib("eunit/include/eunit.hrl").

-import(hrm_utils, [thing_to_list/1, append_query_params/2]).

thing_to_list_test() ->
  ?assertEqual("1", thing_to_list(1)),
  ?assertEqual(float_to_list(2.1), thing_to_list(2.1)),
  ?assertEqual("test", thing_to_list(test)),
  ?assertEqual("test", thing_to_list(<<"test">>)),
  ?assertEqual("test", thing_to_list("test")),
  ok.

append_query_params_test() ->
  ?assertEqual("http://asd/?b=2", append_query_params("http://asd/?b=2", [])),
  ?assertEqual("http://asd/", append_query_params("http://asd/", [])),
  Params = [{a, 1}, {"c", <<"&3">>}],
  ?assertEqual("http://asd/?a=1&c=%263", append_query_params("http://asd/", Params)),
  ?assertEqual("http://asd/?b=2&a=1&c=%263", append_query_params("http://asd/?b=2", Params)),
  ok.
