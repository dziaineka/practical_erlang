-module(strategy_worker).
-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
    user_id,
    user_name
}).


start_link(UserId, UserName) ->
    gen_server:start_link(?MODULE, {UserId, UserName}, []).

init({UserId, UserName}) ->
    State = #state{
            user_id=UserId,
            user_name=UserName
        },

    lager:info("HELLO~n~p~n", [State]),
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

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
