-module(hrm_storage).

-export([start/0, get/1, store/2, delete/1, init/0]).

-record(task, {key, data}).

start() ->
    mnesia:start().

get(Key) ->
    case mnesia:dirty_read(task, Key) of
        [#task{key=Key, data=Data}] -> {ok, Data};
        [] -> {error, notfound}
    end.

store(Key, Data) ->
    mnesia:dirty_write(#task{key=Key, data=Data}).

delete(Key) ->
    mnesia:dirty_delete(task, Key).

init() ->
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {atomic, ok} = mnesia:create_table(task, [
        {disc_copies, [node()]},
        {attributes, record_info(fields, task)}
    ]),
    ok.
