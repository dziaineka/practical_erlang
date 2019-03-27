-module(lesson2).

-export([init/0,
         get_names/1,
         get_females/1,
         get_females2/1,
         get_gender/2,
         partition_by_age/2]).

init() ->
    [
        {user, "Bob", 23, male},
        {user, "Alex", 24, female},
        {user, "dsfdfsd", 3535, male}
    ].

get_names(Users) ->
    get_names(Users, []).

    get_names([], Names) -> lists:reverse(Names);
    get_names([User|Tail], Names) ->
        {user, Name, _, _} = User,
        get_names(Tail, [Name|Names]).

get_females(Users) ->
    get_females(Users, []).

get_females([], Names) -> lists:reverse(Names);
get_females([User|Tail], Names) ->
    case User of
        {user, _, _, male} -> get_females(Tail, Names);
        {user, Name, _, female} -> get_females(Tail, [Name|Names])
    end.

get_females2([]) -> [];
get_females2([User|Tail]) ->
    case User of
        {user, _, _, male} -> [User | get_females2(Tail)];
        {user, _, _, female} -> get_females2(Tail)
    end.

get_gender(Users, Gender) when Gender == male orelse Gender == female ->
    get_gender(Users, Gender, []).

get_gender([], _Gender, Names) -> lists:reverse(Names);
get_gender([User|Tail], Gender, Names) ->
    {user, Name, _Age, UserGender} = User,

    if
        Gender == UserGender -> get_gender(Tail, Gender, [Name|Names]);

        true -> get_gender(Tail, Gender, Names)

    end.

partition_by_age(Users, Age) ->
    partition_by_age(Users, Age, {[], []}).

partition_by_age([], _Age, {Older, Younger}) -> {lists:reverse(Older),
                                                 lists:reverse(Younger)};

partition_by_age([User|Tail], Age, {Older, Younger}) ->
    {user, Name, UserAge, _Gender} = User,

    if
        UserAge > Age -> partition_by_age(Tail, Age, {[Name|Older], Younger});

        true -> partition_by_age(Tail, Age, {Older, [Name|Younger]})

    end.


