-module(lesson08).
-export([flatten/1, test_mailbox/0, mailbox_checker/0]).

flatten([Head | Tail]) ->
    case Head of
        [] -> flatten(Tail);

        [SmallHead | SmallTail] ->
            [flatten(SmallHead) | flatten([flatten(SmallTail) | Tail])];

        _ -> [Head | flatten(Tail)]
    end;
flatten(Anything) ->
    Anything.

test_mailbox() ->
    spawn(?MODULE, mailbox_checker, []).

mailbox_checker() ->
    io:format("Checker ~p~n", [self()]),
    loop().

loop() ->
    receive
        stop ->
            io:format("Stop"),
            ok;

        Msg -> io:format("Message ~p ~p~n", [self(), Msg]),
        loop()
    after
        5000 -> io:format("No messages ~p~n", [self()]),
        loop()
    end.