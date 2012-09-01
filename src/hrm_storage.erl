-module(hrm_storage).

-behavior(e2_service).

-export([start_link/1, get/1, store/2, append/2, delete/1]).

-export([init/1, handle_msg/3]).

start_link(File) ->
  e2_service:start_link(?MODULE, File, [registered]).

get(Key) ->
  e2_service:call(?MODULE, {get, Key}).

store(Key, Data) ->
  e2_service:call(?MODULE, {store, Key, Data}).

append(Key, PartialData) ->
  e2_service:call(?MODULE, {append, Key, PartialData}).

delete(Key) ->
  e2_service:call(?MODULE, {delete, Key}).

init(File) ->
  {ok, Db} = dets:open_file(File, []),
  {ok, Db}.

handle_msg({get, Key}, _From, Db) ->
  Response = case dets:lookup(Db, Key) of
    [{_Key, Data}] -> {ok, Data};
    [] -> {error, notfound}
  end,
  {reply, Response, Db};

handle_msg({store, Key, Data}, _From, Db) ->
  NewData = hrm_utils:normalize_proplist(Data),
  ok = dets:insert(Db, {Key, NewData}),
  {reply, {ok, NewData}, Db};

handle_msg({append, Key, PartialData}, _From, Db) ->
  [{_Key, OldData}] = dets:lookup(Db, Key),
  NewData = hrm_utils:normalize_proplist(PartialData ++ OldData),
  ok = dets:insert(Db, {Key, NewData}),
  {reply, {ok, NewData}, Db};

handle_msg({delete, Key}, _From, Db) ->
  {reply, dets:delete(Db, Key), Db}.
