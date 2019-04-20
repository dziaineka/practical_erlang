-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {id}).

start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

ping(Pid) ->
    gen_server:call(Pid, {ping}).

init([Id]) ->
    {ok, #state{id=Id}}.

handle_call({ping}, _From, State) ->
    {reply, {State#state.id, self()}, State};

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

