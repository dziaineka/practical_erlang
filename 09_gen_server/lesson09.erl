-module(lesson09).

-export([run/0]).


run() ->
    process_flag(trap_exit, true),
    lists:foreach(
        fun (ID) -> spawn_link(fun () -> worker(ID) end) end,
        lists:seq(1, 10)
    ),
    ok.


worker(ID) ->
    io:format("Worker ID: ~p PID: ~p start~n", [ID, self()]),
    timer:sleep(rand:uniform(1000)),
    io:format("Worker ID: ~p PID: ~p stop~n", [ID, self()]).