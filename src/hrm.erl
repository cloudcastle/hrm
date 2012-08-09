-module(hrm).
-behaviour(application).

-export([start/0]).

-export([start/2, stop/1]).

start() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(cowboy),
    application:start(hrm).

start(_Type, _Args) ->
    ok.

stop(_State) ->
    ok.