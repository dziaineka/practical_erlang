-module(mcache).

-behaviour(gen_server).

%% API
-export([start_link/0, set/2, get/1, gets/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {storage}).


set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).


get(Key) ->
    gen_server:call(?MODULE, {get, Key}).


gets(Keys) ->
    gen_server:call(?MODULE, {gets, Keys}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_Args) ->
    io:format("mcache storage~n"),
    {ok, #state{storage = #{}}}.


handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({set, Key, Value}, _From, State) ->
    io:format("store ~p:~p~n", [Key, Value]),
    Storage = State#state.storage,
    NewState = #state{storage = Storage#{Key => Value}},
    {reply, {ok, "STORED"}, NewState};

handle_call({get, Key}, _From, State) ->
    Storage = State#state.storage,

    case maps:find(Key, Storage) of
        {ok, Value} ->
            {reply, {ok, ["VALUE ", Key, " ", Value, "\r\nEND"]}, State};

        error ->
            {reply, {ok, "NOT FOUND"}, State}
    end;


handle_call({gets, Keys}, _From, State) ->
    Storage = State#state.storage,

    Values = lists:foldr(
        fun (Key, Result) ->
            case maps:find(Key, Storage) of
                {ok, Value} ->
                    ["VALUE ", Key, " ", Value, "\r\n" | Result];

                error ->
                    ["VALUE ", Key, " ", "NOT FOUND", "\r\n" | Result]
            end
        end,
        [],
        Keys
    ),

    {reply, {ok, [Values | "END"]}, State};

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
