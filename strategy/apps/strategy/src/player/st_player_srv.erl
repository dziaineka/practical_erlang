-module(st_player_srv).
-behavior(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("st_player.hrl").

-record(state, {
    socket :: pid(),
    player :: #player{}
}).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).


%%% gen_server API

init(Socket) ->
    State = #state{
        socket = Socket,
        player = #player{}
    },

    st_player_storage:add_player(Socket, self()),
    lager:info("Player created ~p~n", [State]),
    {ok, State}.

handle_call(_Request, _From, #player{} = State) ->
    {reply, ok, State}.

handle_cast(_Request, #player{} = State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
{ok, State}.