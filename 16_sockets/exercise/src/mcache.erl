-module(mcache).

-behaviour(gen_server).

%% API
-export([start_link/0, set/2, get/1, gets/1, delete/1, add/2, replace/2,
         append/2, prepend/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {storage}).


set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).


get(Key) ->
    gen_server:call(?MODULE, {get, Key}).


gets(Keys) ->
    gen_server:call(?MODULE, {gets, Keys}).


delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).


add(Key, Value) ->
    gen_server:call(?MODULE, {add, Key, Value}).


replace(Key, Value) ->
    gen_server:call(?MODULE, {replace, Key, Value}).


append(Key, Value) ->
    gen_server:call(?MODULE, {append, Key, Value}).


prepend(Key, Value) ->
    gen_server:call(?MODULE, {prepend, Key, Value}).


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
    case maps:find(Key, State#state.storage) of
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

handle_call({delete, Key}, _From, State) ->
    case maps:find(Key, State#state.storage) of
        {ok, _Value} ->
            {
                reply,
                {ok, "DELETED"},
                State#state{storage = maps:remove(Key, State#state.storage)}
            };

        error ->
            {reply, {ok, "NOT FOUND"}, State}
    end;

handle_call({add, Key, Value}, _From, State) ->
    case maps:find(Key, State#state.storage) of
        {ok, _Value} ->
            {reply, {ok, "EXISTS"}, State};

        error ->
            Storage = State#state.storage,
            NewState = #state{storage = Storage#{Key => Value}},
            {reply, {ok, "STORED"}, NewState}
    end;

handle_call({replace, Key, Value}, _From, State) ->
    case maps:find(Key, State#state.storage) of
        {ok, _Value} ->
            Storage = State#state.storage,
            NewState = #state{storage = Storage#{Key => Value}},
            {reply, {ok, "STORED"}, NewState};

        error ->
            {reply, {ok, "NOT FOUND"}, State}
    end;

handle_call({append, Key, Value}, _From, State) ->
    case maps:find(Key, State#state.storage) of
        {ok, _Value} ->
            Storage = State#state.storage,
            AppendedValue = lists:append(maps:get(Key, Storage), Value),
            NewState = #state{storage = Storage#{Key => AppendedValue}},

            {reply, {ok, "STORED"}, NewState};

        error ->
            {reply, {ok, "NOT FOUND"}, State}
    end;

handle_call({prepend, Key, Value}, _From, State) ->
    case maps:find(Key, State#state.storage) of
        {ok, _Value} ->
            Storage = State#state.storage,
            AppendedValue = lists:append(Value, maps:get(Key, Storage)),
            NewState = #state{storage = Storage#{Key => AppendedValue}},

            {reply, {ok, "STORED"}, NewState};

        error ->
            {reply, {ok, "NOT FOUND"}, State}
    end;

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
