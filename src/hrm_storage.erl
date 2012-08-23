-module(hrm_storage).

-export([start/0, get/1, store/2, append/2, delete/1, init/0]).

-record(task, {key, data}).

start() ->
    mnesia:start().

get(Key) ->
    case mnesia:dirty_read(task, Key) of
        [#task{key=Key, data=Data}] -> {ok, Data};
        [] -> {error, notfound}
    end.

store(Key, Data) ->
    mnesia:dirty_write(#task{key=Key, data=normalize(Data)}).

append(Key, PartialData) ->
    {ok, OldData} = hrm_storage:get(Key),
    NewData = PartialData ++ OldData,
    store(Key, NewData).

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

normalize(Data) ->
    [{Key, proplists:get_value(Key, Data)} || Key <- proplists:get_keys(Data)].
