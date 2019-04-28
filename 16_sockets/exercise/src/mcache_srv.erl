-module(mcache_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, accept/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {acceptors_number, max_acceptors_number}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


start_another_acceptor(ListenSocket) ->
    gen_server:cast(?MODULE, {start_another_acceptor, ListenSocket}).


acceptor_closed(ListenSocket) ->
    gen_server:call(?MODULE, {acceptor_closed, ListenSocket}).


init(_Args) ->
    {ok, Port} = application:get_env(mcache, port),
    {ok, MaxAcceptors} = application:get_env(mcache, accept_pool_size),

    io:format("start server at port ~p, max clients ~p~n",
              [Port, MaxAcceptors]),


    {ok, ListenSocket} = gen_tcp:listen(
        Port,
        [list, {active, false}, {reuseaddr, true}]
    ),

    io:format("ListenSocket ~p~n", [ListenSocket]),

    add_acceptor(ListenSocket, #state{
        max_acceptors_number = MaxAcceptors,
        acceptors_number = 0}).


handle_call({acceptor_closed, ListenSocket}, _From, State) ->
    State2 = State#state{acceptors_number = State#state.acceptors_number - 1},

    case State#state.acceptors_number == State#state.max_acceptors_number of
        true ->
            {ok, State3} = add_acceptor(ListenSocket, State2),

            io:format("acceptor closed, ~p rest~n",
                      [State3#state.acceptors_number]);

        false ->
            State3 = State2
    end,

    io:format("acceptor closed, ~p rest~n", [State3#state.acceptors_number]),
    {reply, ok, State3};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({start_another_acceptor, ListenSocket}, State) ->
    io:format("add new acceptor ~p~n", [ListenSocket]),
    {ok, NewState} = add_acceptor(ListenSocket, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


add_acceptor(ListenSocket, State) ->
    case State#state.acceptors_number >= State#state.max_acceptors_number of
        true ->
            io:format("Max acceptors amount reached: ~p~n",
                      [State#state.acceptors_number]),

            {ok, State};

        false ->
            Acceptor = spawn(?MODULE, accept, [ListenSocket]),

            io:format("Acceptor ~p, amount - ~p~n",
                    [Acceptor, State#state.acceptors_number + 1]),

            {ok, State#state{
                acceptors_number = State#state.acceptors_number + 1
            }}
    end.


accept(ListenSocket) ->
    io:format("Acceptor ~p~n", [self()]),
    io:format("Waiting for client~n"),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    io:format("Got client ~p~n", [AcceptSocket]),
    start_another_acceptor(ListenSocket),
    handle_connection(ListenSocket, AcceptSocket).


handle_connection(ListenSocket, AcceptSocket) ->
    case gen_tcp:recv(AcceptSocket, 0) of
        {ok, "stop\r\n"} ->
            io:format("Connection stopped~n"),
            gen_tcp:send(AcceptSocket, ["Goodbye!\r\n"]),
            acceptor_closed(ListenSocket);

        {ok, Message} ->
            io:format("~p Got message from client: ~p~n", [self(), Message]),
            {ok, Reply} = process_command(Message),
            gen_tcp:send(AcceptSocket, [Reply]),
            handle_connection(ListenSocket, AcceptSocket);

        {error, closed} ->
            io:format("Connection closed~n"),
            acceptor_closed(ListenSocket)
    end.


process_command(Command) ->
    Funcs = [
        fun set/1,
        fun gets/1,
        fun get/1,
        fun delete/1,
        fun add/1,
        fun replace/1,
        fun append/1,
        fun prepend/1
    ],

    Result = lists:foldl(
        fun
            (_Fun, {ok, Result}) ->
                {ok, Result};

            (Fun, {nomatch, _Result}) ->
                Fun(Command)
        end,
        {nomatch, ""},
        Funcs
    ),

    case Result of
        {ok, Reply} ->
            {ok, Reply ++ "\r\n"};

        {nomatch, Reply} ->
            {ok, Reply ++ "\r\n"}
    end.


find_command(Command, Fun, Keyword) ->
    case string:find(string:lowercase(Command), Keyword) of
        nomatch ->
            {nomatch, "UNKNOWN COMMAND"};

        _ ->
            [_Operation, Parameters] = string:split(string:chomp(Command), " "),
            Fun(Parameters)
    end.


set(Command) ->
    Fun = fun
        (Parameters) ->
            [Key | Value] = string:split(Parameters, " "),
            mcache:set(Key, Value);

        (_Cmd) ->
            {ok, "INVALID PARAMETERS"}
    end,

    find_command(Command, Fun, "set").


get(Command) ->
    Fun = fun
        ("") ->
            {ok, "INVALID PARAMETERS"};

        (Key) ->
            mcache:get(Key)
    end,

    find_command(Command, Fun, "get").


gets(Command) ->
    Fun = fun
        ("") ->
            {ok, "INVALID PARAMETERS"};

        (Parameters) ->
            Keys = string:split(Parameters, " ", all),
            mcache:gets(Keys)
    end,

    find_command(Command, Fun, "gets").


delete(Command) ->
    Fun = fun (_Parameters) ->
        {ok, "DELETE"}
    end,

    find_command(Command, Fun, "delete").


add(Command) ->
    Fun = fun (_Parameters) ->
        {ok, "ADD"}
    end,

    find_command(Command, Fun, "add").


replace(Command) ->
    Fun = fun (_Parameters) ->
        {ok, "REPLACE"}
    end,

    find_command(Command, Fun, "replace").


append(Command) ->
    Fun = fun (_Parameters) ->
        {ok, "APPEND"}
    end,

    find_command(Command, Fun, "append").


prepend(Command) ->
    Fun = fun (_Parameters) ->
        {ok, "PREPEND"}
    end,

    find_command(Command, Fun, "prepend").

