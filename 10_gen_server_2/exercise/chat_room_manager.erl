-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {rooms = #{}}).


%%%%%%%% client side %%%%%%%%

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


create_room(RoomName) ->
    gen_server:call(?MODULE, {create_room, RoomName}).


get_rooms() ->
    gen_server:call(?MODULE, {get_rooms}).


add_user(RoomPid, UserName, UserPid) ->
    gen_server:call(?MODULE, {add_user, RoomPid, UserName, UserPid}).


remove_user(RoomPid, UserPid) ->
    gen_server:call(?MODULE, {remove_user, RoomPid, UserPid}).


get_users(RoomPid) ->
    gen_server:call(?MODULE, {get_users, RoomPid}).


send_message(RoomPid, Author, Message) ->
    gen_server:call(?MODULE, {send_message, RoomPid, Author, Message}).


get_history(RoomPid) ->
    gen_server:call(?MODULE, {get_history, RoomPid}).


% close_room() ->
%     gen_server:call(?MODULE, {close_room, }).


%%%%%%%% server side %%%%%%%%

init(_Args) ->
   {ok, #state{}}.


room_operation(Operation, RoomPid, Rooms) ->
    case maps:find(RoomPid, Rooms) of
        {ok, _} ->
            Operation();

        error ->
            {error, room_not_found}
    end.


handle_call({create_room, RoomName}, _From, State) ->
    Rooms = State#state.rooms,
    {ok, RoomPid} = chat_room:start_link(),
    NewState = State#state{rooms = Rooms#{RoomPid => RoomName}},

    {reply, {RoomName, RoomPid}, NewState};

handle_call({get_rooms}, _From, State) ->
    Rooms = State#state.rooms,

    RoomList = lists:map(
        fun ({Key, Value}) -> {Value, Key} end,
        maps:to_list(Rooms)
    ),

    {reply, RoomList, State};

handle_call({add_user, RoomPid, UserName, UserPid}, _From, State) ->
    Adding = fun () -> chat_room:add_user(RoomPid, UserName, UserPid) end,
    {
        reply,
        room_operation(Adding, RoomPid, State#state.rooms),
        State
    };

handle_call({remove_user, RoomPid, UserPid}, _From, State) ->
    Removing = fun () -> chat_room:remove_user(RoomPid, UserPid) end,
    {
        reply,
        room_operation(Removing, RoomPid, State#state.rooms),
        State
    };

handle_call({get_users, RoomPid}, _From, State) ->
    Getting = fun () -> {ok, chat_room:get_users(RoomPid)} end,
    {
        reply,
        room_operation(Getting, RoomPid, State#state.rooms),
        State
    };

handle_call({send_message, RoomPid, Author, Text}, _From, State) ->
    Sending = fun () -> chat_room:add_message(RoomPid, Author, Text) end,
    {
        reply,
        room_operation(Sending, RoomPid, State#state.rooms),
        State
    };

handle_call({get_history, RoomPid}, _From, State) ->
    Getting = fun () -> {ok, chat_room:get_history(RoomPid)} end,
    {
        reply,
        room_operation(Getting, RoomPid, State#state.rooms),
        State
    }.


handle_cast(_Msg, State) ->
   {noreply, State}.


handle_info(_Info, State) ->
   {noreply, State}.


terminate(_Reason, _State) ->
   ok.


code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
