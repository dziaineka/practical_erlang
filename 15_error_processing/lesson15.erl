-module(lesson15).

-export([run/0]).


do_something() ->
    error(something_good_happened).

run() ->
    try
        {ok, Res} = do_something(),

        Res
    catch
        throw:SomeError:ST ->
            io:format("Throw: ~p~n", [ST]),
            {error, SomeError};

        error:SomeError:ST ->
            io:format("Error: ~p~n", [ST]),
            {error, SomeError}
    end.
