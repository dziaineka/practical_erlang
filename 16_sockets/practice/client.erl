-module(client).

-behaviour(gen_server).

%% API
-export([start/0, start/1, send/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {socket}).

start() ->
    start(1234).

start(Port) ->
    Options = [{port, Port}, {host, {127,0,0,1}}],
    gen_server:start(?MODULE, Options, []).

stop(ClientPid) ->
    gen_server:call(ClientPid, {stop}).

send(ClientPid, Message) ->
    gen_server:call(ClientPid, {send, Message}).

init(Options) ->
    io:format("~p init with options ~p~n", [self(), Options]),
    Host = proplists:get_value(host, Options),
    Port = proplists:get_value(port, Options),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, 2}]),
    io:format("Connected ~p~n", [Socket]),
    {ok, #state{socket=Socket}}.

handle_call({send, Message}, _From, #state{socket = Socket} = State) ->
    gen_tcp:send(Socket, Message),
    {reply, ok, State};

handle_call({stop}, _From, #state{socket = Socket} = State) ->
    gen_tcp:close(Socket),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
