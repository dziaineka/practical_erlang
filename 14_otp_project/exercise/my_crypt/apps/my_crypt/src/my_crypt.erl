-module(my_crypt).

-behaviour(gen_server).

%% API
-export([start_link/0, encode/1, get_key/0, set_key/1, hash/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {key, hash_size}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


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
    {ok, #state{key=binary_to_list(Key), hash_size=HashSize}}.


get_crypt_key_symbol(Key, Index) ->
    KeyLength = length(Key),

    if
        Index =< KeyLength ->
            lists:nth(Index, Key);

        true ->
            lists:nth(Index rem KeyLength, Key)
    end.


xor_list([], _Key, _Index) ->
    [];

xor_list([Head | Tail], Key, Index) ->
    [
        (Head bxor get_crypt_key_symbol(Key, Index)) |
        xor_list(Tail, Key, Index + 1)
    ].


encrypt(Key, Data) ->
    DataList = binary_to_list(Data),
    StartIndex = 1,
    EncryptedData = xor_list(DataList, Key, StartIndex),
    list_to_binary(EncryptedData).


get_hash(HashSize, Data) ->
    lists:foldl(
        fun
            (Char, Hash) ->
                ((Hash * 17) + Char) div HashSize
        end,
        0,
        Data
    ).


handle_call({get_key}, _From, State) ->
    {reply, State#state.key, State};

handle_call({set_key, Key}, _From, State) ->
    NewState = State#state{key=binary_to_list(Key)},
    {reply, ok, NewState};

handle_call({encode, InitialData}, _From, State) ->
    EncryptedData = encrypt(State#state.key, InitialData),
    {reply, EncryptedData, State};

handle_call({hash, InitialData}, _From, State) ->
    DataList = binary_to_list(InitialData),
    Hash = get_hash(State#state.hash_size, DataList),
    {reply, Hash, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





