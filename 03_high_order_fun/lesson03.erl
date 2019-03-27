-module(lesson03).

-export([init/0,
         get_names/1,
         filter_by_age/2,
         partition_by_age/2,
         filter/1,
         get_stat/1]).

init() ->
    [
        {user, "Bob", 23, male},
        {user, "Alex", 24, female},
        {user, "dsfdfsd", 3535, male}
    ].

get_names(Users) ->
    lists:map(fun greet/1, Users).

greet({_, Name, _, _}) ->
    "Hello " ++ Name.

filter_by_age(Users, Age) ->
    F = fun({user, _, CurAge, _}) -> CurAge > Age end,
    lists:filter(F, Users).

filter(Users) ->
    lists:filtermap(
        fun({user, Name, _Age, Gender}) ->
            case Gender of
                female ->
                    Greet = greet(Name),
                    {true, Greet};
                male -> false
            end
        end,
    Users).

-record(stat, {
    total_users = 0,
    total_male = 0,
    total_female = 0,
    average_age = 0
}).

get_stat(Users) ->
    lists:foldl(
        fun(User, Acc) ->
            {user, _, _Age, _Gender} = User,
            Acc
        end,
    #stat{},
    Users).

partition_by_age(Users, Age) ->
    Fun = fun(User, Acc) ->
        {Younger, Older} = Acc,
        {user, _, CurrAge, _} = User,

        case CurrAge < Age of
            false -> {Younger, [User | Older]};
            true -> {[User | Younger], Older}
        end,
        Acc
    end,

    lists:foldl(Fun, {[],[]}, Users).
