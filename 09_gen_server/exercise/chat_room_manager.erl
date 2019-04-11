-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2]).

-export([loop/1, handle_call/3]).


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


user_operation(Action, {RoomId, UserName}, State) ->
    case maps:find(RoomId, State) of
        {ok, {room, Name, Users, Messages}} ->
            case lists:member(UserName, Users) of
                false ->
                    State2 = State#{
                        RoomId => {
                            room,
                            Name,
                            Action(UserName, Users),
                            Messages
                        }
                    },

                    {ok, ok, State2};

                true ->
                    {error, user_is_in_room}
            end;

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

    CanAppend = maps:size(State) < 5,

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
    Adding = fun (Name, Users) -> [Name | Users] end,
    user_operation(Adding, {RoomId, UserName}, State);

handle_call(remove_user, {RoomId, UserName}, State) ->
    Removing = fun (Name, Users) -> lists:delete(Name, Users) end,
    user_operation(Removing, {RoomId, UserName}, State).


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
