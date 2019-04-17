-module(my_crypt).

-behaviour(gen_server).

%% API
-export([start_link/0, encode/1, get_key/0, set_key/1, hash/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {key, hash_size}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).


encode(Data) ->
    gen_server:call(?MODULE, {encode, Data}).


get_key() ->
    gen_server:call(?MODULE, {get_key}).


set_key(Key) ->
    gen_server:call(?MODULE, {set_key, Key}).


hash(Data) ->
    gen_server:call(?MODULE, {hash, Data}).


%============================================================

init(_Args) ->
    {ok, Key} = application:get_env(crypt_key),
    {ok, HashSize} = application:get_env(hash_size),
    io:format("~p~n~p~n", [Key, HashSize]),
    {ok, #state{key=Key, hash_size=HashSize}}.


handle_call({get_key}, _From, State) ->
    {reply, State#state.key, State};

handle_call({set_key, Key}, _From, State) ->
    NewState = State#state{key=Key},
    {reply, ok, NewState}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





