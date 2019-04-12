-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2]).

-export([loop/1, handle_call/3]).

-define(MAX_ROOMS, 5).


loop(State) ->
    io:format("Current state: ~p~n", [State]),

    receive
        {Command, QueryRef, ClientPid, Payload} ->
            case handle_call(Command, Payload, State) of
                {ok, Reply, State2} ->
                    ClientPid ! {reply, QueryRef, Reply},
                    ?MODULE:loop(State2);

                {error, Reason} ->
                    ClientPid ! {reply, QueryRef, {error, Reason}},
                    ?MODULE:loop(State)
            end;

        stop ->
            stop;

        Msg ->
            io:format("~p got msg: ~p~n", [self(),Msg]),
            ?MODULE:loop(State)
    end.


call(Server, Command, Payload) ->
    QueryRef = erlang:monitor(process, Server),
    Server ! {Command, QueryRef, self(), Payload},

    receive
        {reply, QueryRef, Reply} ->
            erlang:demonitor(QueryRef, [flush]),
            Reply;

        {'DOWN', QueryRef, process, ServerPid, Reason} ->
            io:format("Server ~p crashed ~p", [ServerPid, Reason]),
            {error, Reason}
    after
        5000 ->
            erlang:demonitor(QueryRef, [flush]),
            {error, no_reply}
    end.


start() ->
    InitialState = #{},
    spawn(?MODULE, loop, [InitialState]).


room_content_operation({Action, UserShouldExist, NegativeResult},
                       {RoomId, UserName},
                       State) ->
    Operation = fun
        ({room, Name, Users, Messages}) ->
            case lists:member(UserName, Users) of
                UserShouldExist ->
                    State2 = State#{
                        RoomId => Action({room, Name, Users, Messages})
                    },

                    {ok, ok, State2};

                _ ->
                    {error, NegativeResult}
            end
    end,

    room_operation(Operation, RoomId, State).


room_operation(Action, RoomId, State) ->
    case maps:find(RoomId, State) of
        {ok, Room} ->
            Action(Room);

        error ->
            {error, room_not_found}
    end.


handle_call(create_room, RoomName, State) ->
    case maps:keys(State) of
        [] ->
            ID = 1;

        KeysList ->
            ID = lists:max(KeysList) + 1
    end,

    CanAppend = maps:size(State) < ?MAX_ROOMS,

    if
        CanAppend ->
            State2 = State#{ID => {room, RoomName, [], []}},
            {ok, {ok, ID}, State2};

        true ->
            {error, room_limit}
    end;

handle_call(remove_room, RoomId, State) ->
    AlteredState = maps:remove(RoomId, State),
    RoomExisted = maps:size(AlteredState) < maps:size(State),

    if
        RoomExisted ->
            {ok, ok, AlteredState};

        true ->
            {error, room_not_found}
    end;

handle_call(get_rooms, _, State) ->
    {
        ok,
        lists:zip(
            maps:keys(State),
            lists:map(
                fun
                    ({room, RoomName, _, _}) -> RoomName
                end,
                maps:values(State)
            )
        ),
        State
    };

handle_call(add_user, {RoomId, UserName}, State) ->
    Adding = fun ({room, RoomName, Users, Messages}) ->
        {room, RoomName, [UserName | Users], Messages} end,

    room_content_operation({Adding, false, user_is_in_room},
                           {RoomId, UserName},
                           State);

handle_call(remove_user, {RoomId, UserName}, State) ->
    Removing = fun ({room, RoomName, Users, Messages}) ->
        {room, RoomName, lists:delete(UserName, Users), Messages}
    end,

    room_content_operation({Removing, true, user_not_in_room},
                           {RoomId, UserName},
                           State);

handle_call(get_users_list, RoomId, State) ->
    Fun = fun
        ({room, _Name, Users, _Messages}) ->
            {ok, {ok, Users}, State}
    end,

    room_operation(Fun, RoomId, State);

handle_call(send_message, {RoomId, UserName, Message}, State) ->
    Fun = fun ({room, RoomName, Users, Messages}) ->
        {room, RoomName, Users, [{UserName, Message} | Messages]}
    end,

    room_content_operation({Fun, true, user_not_in_room},
                           {RoomId, UserName},
                           State);

handle_call(get_messages_history, RoomId, State) ->
    Fun = fun
        ({room, _Name, _Users, Messages}) ->
            {ok, {ok, Messages}, State}
    end,

    room_operation(Fun, RoomId, State).


create_room(Server, RoomName) ->
    call(Server, create_room, RoomName).


remove_room(Server, RoomId) ->
    call(Server, remove_room, RoomId).


get_rooms(Server) ->
    call(Server, get_rooms, Server).


add_user(Server, RoomId, UserName) ->
    call(Server, add_user, {RoomId, UserName}).


remove_user(Server, RoomId, UserName) ->
    call(Server, remove_user, {RoomId, UserName}).


get_users_list(Server, RoomId) ->
    call(Server, get_users_list, RoomId).


send_message(Server, RoomId, UserName, Message) ->
    call(Server, send_message, {RoomId, UserName, Message}).


get_messages_history(Server, RoomId) ->
    call(Server, get_messages_history, RoomId).
