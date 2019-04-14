-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2,
         get_users/1, add_message/3, get_history/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
    users = #{},
    messages = []
}).

%%%%%%%% client side %%%%%%%%

start_link() ->
   gen_server:start_link(?MODULE, [], []).


add_user(RoomPid, UserName, UserPid) ->
    gen_server:call(RoomPid, {add_user, UserName, UserPid}).


remove_user(RoomPid, UserPid) ->
    gen_server:call(RoomPid, {remove_user, UserPid}).


get_users(RoomPid) ->
    gen_server:call(RoomPid, {get_users}).


add_message(RoomPid, Author, Text) ->
    gen_server:call(RoomPid, {add_message, Author, Text}).


get_history(RoomPid) ->
    gen_server:call(RoomPid, {get_history}).


%%%%%%%% server side %%%%%%%%

init(_Args) ->
   {ok, #state{}}.


handle_call({add_user, UserName, UserPid}, _From, State) ->
    Users = State#state.users,

    NewState = State#state{
        users=Users#{UserPid => UserName}
    },

    {reply, ok, NewState};

handle_call({remove_user, UserPid}, _From, State) ->
    Users = State#state.users,

    case maps:find(UserPid, Users) of
        {ok, _} ->
            NewState = State#state{
                users=maps:remove(UserPid, Users)
            },

            {reply, ok, NewState};

        error ->
            {reply, {error, user_not_found}, State}
    end;

handle_call({get_users}, _From, State) ->
    Users = State#state.users,

    UsersList = lists:map(
        fun ({Key, Value}) -> {Value, Key} end,
        maps:to_list(Users)
    ),

    {reply, UsersList, State};

handle_call({add_message, Author, Text}, _From, State) ->
    Users = maps:keys(State#state.users),
    Messages = State#state.messages,

    lists:foreach(
        fun (User) -> chat_user:add_message(User, Author, Text) end,
        Users
    ),

    NewState = State#state{
        messages=[{Author, Text} | Messages]
    },

    {reply, ok, NewState};

handle_call({get_history}, _From, State) ->
    {reply, lists:reverse(State#state.messages), State}.


handle_cast(_Msg, State) ->
   {noreply, State}.


handle_info(_Info, State) ->
   {noreply, State}.


terminate(_Reason, _State) ->
   ok.


code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
