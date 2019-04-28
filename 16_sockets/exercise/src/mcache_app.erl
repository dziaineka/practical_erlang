-module(mcache_app).

-behaviour(application).

%% API
-export([start/0, start/2, stop/1]).


start() ->
    application:start(mcache),
    ok.


start(_StartType, _StartArgs) ->
    io:format("app mtcache started~n"),
    mcache_sup:start_link().


stop(_State) ->
    ok.
