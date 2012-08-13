-module(hrm_storage).

-export([init/0, get/1, store/2, delete/1]).

init() ->
    ok.

get(Key) ->
    io:format("Getting data for key '~p'~n", [Key]),
    {ok, <<"Test data">>}.

store(Key, Data) ->
    io:format("Storing data '~p' for key '~p'~n", [Data, Key]),
    ok.

delete(Key) ->
    io:format("Deleting data for key '~p'~n", [Key]),
    ok.
