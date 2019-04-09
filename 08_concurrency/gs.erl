-module(gs).

-export([start/0, add/3, remove/2, check/2, stop/1, loop/1]).


add(Pid, Key, Value) ->
    call(Pid, add, {Key, Value}).


remove(Pid, Key) ->
    call(Pid, remove, Key).


check(Pid, Key) ->
    call(Pid, check, Key).


call(ServerPid, Command, Payload) ->
    QueryRef = erlang:monitor(process, ServerPid),
    ServerPid ! {Command, QueryRef, self(), Payload},

    receive
        {reply, QueryRef, Reply} ->
            erlang:demonitor(QueryRef, [flush]),
            Reply;

        {'DOWN', QueryRef, process, ServerPid, Reason} ->
            io:format("Server crashed ~p", [Reason]),
            {error, Reason}
    after
        5000 ->
            erlang:demonitor(QueryRef, [flush]),
            {error, no_reply}
    end.



stop(Pid) ->
    Pid ! stop.


start() ->
    InitialState = #{},
    spawn(?MODULE, loop, [InitialState]).


loop(State) ->
    io:format("V5, Current state: ~p~n", [State]),

    receive
        {Command, QueryRef, ClientPid, Payload} ->
            {Reply, State2} = handle_call(Command, Payload, State),
            ClientPid ! {reply, QueryRef, Reply},
            ?MODULE:loop(State2);

        stop ->
            stop;

        Msg ->
            io:format("~p got msg ~p~n", [self(),Msg]),
            gs:loop(State)
    end.


handle_call(add, {K, V}, State) ->
    State2 = State#{K => V},
    {ok, State2};

handle_call(remove, K, State) ->
    State2 = maps:remove(K, State),
    {ok, State2};

handle_call(check, K, State) ->
    Reply = case maps:find(K, State) of
                {ok, V} -> {ok, V};
                _ -> {error, no_value}
    end,
    {Reply, State}.