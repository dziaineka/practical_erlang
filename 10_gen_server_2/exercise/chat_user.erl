-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {messages = []}).


%%%%%%%% client side %%%%%%%%

start_link() ->
    gen_server:start_link(?MODULE, [], []).


add_message(UserPid, Author, Text) ->
    gen_server:call(UserPid, {add_message, UserPid, Author, Text}).


get_messages(UserPid) ->
    gen_server:call(UserPid, {get_messages, UserPid}).


%%%%%%%% server side %%%%%%%%

init([]) ->
    {ok, #state{}}.


handle_call({add_message, _UserPid, Author, Text}, _From, State) ->
    NewState = State#state{
        messages=[{Author, Text} | State#state.messages]
    },
    {reply, ok, NewState};

handle_call({get_messages, _UserPid}, _From, State) ->
    {reply, lists:reverse(State#state.messages), State}.


handle_cast(_Msg, State) ->
   {noreply, State}.


handle_info(_Info, State) ->
   {noreply, State}.


terminate(_Reason, _State) ->
   ok.


code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
