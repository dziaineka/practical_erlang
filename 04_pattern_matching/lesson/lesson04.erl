-module(lesson04).
-export([max/1,
         fizzbuzz/1,
         users_upper/1,
         to_map/1]).

max([]) ->
    undefined;
max([Head|Tail]) ->
    lists:foldl(
        fun (Value, PrevMax) ->
            case Value > PrevMax of
                false -> PrevMax;
                true -> Value
            end
        end, Head, Tail).


fizzbuzz(List) ->
    lists:map(
        fun (Value) ->
            Rem3 = Value rem 3,
            Rem5 = Value rem 5,

            case {Rem3, Rem5} of
                {0, 0} -> fizzbuzz;
                {0, _} -> fizz;
                {_, 0} -> buzz;
                {_, _} -> Value
            end
        end, List).


users_upper(Users) ->
    maps:map(
        fun
            (ID, User) when ID rem 2 == 0 ->
                {user, Name, Age, Gender} = User,
                {user, string:to_upper(Name), Age, Gender};
            (_ID, User) -> User
        end,
        Users).


to_map(PL) ->
    lists:foldl(
        fun
            ({ID, Value}, Map) ->
                Map#{ID => Value};
            (Atom, Map) ->
                Map#{Atom => true}
        end,
        maps:new(),
        PL
    ).