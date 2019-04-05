-module(lesson06).

-export([init/1, get_users/0, get_answers/1, count_records/1, move/2]).

-include_lib("stdlib/include/ms_transform.hrl").

init(Amount) ->
    Tid = ets:new(my_table, [named_table]),
    ets:new(dest_table, [named_table]),
    get_answers(Amount),
    Tid.


get_answers(Amount) ->
    Sources = [source1, source2, source3, source4, source5],

    if
        Amount > 0 ->
            Response = {
                    Amount,
                    lists:nth(rand:uniform(5), Sources),
                    rand:uniform(100500)
                },

            ets:insert(my_table, Response),
            get_answers(Amount - 1);
        true ->
            ok
    end.

count_records(Source) ->
    length(ets:match_object(my_table, {'_', Source, '_'})).

move(Min, Max) ->
    ets:delete_all_objects(dest_table),

    MS = ets:fun2ms(
        fun({ID, Source, Value})
            when Value > Min andalso Value < Max ->
    {ID, Source, Value}
        end
    ),

    ets:insert(dest_table, ets:select(my_table, MS)).


-record(user, {
    name,
    age,
    gender
}).

get_users() ->
    [
        #user{name = "Bob", age = 21, gender = male},
        #user{name = "Bill", age = 23, gender = male},
        #user{name = "Helen", age = 17, gender = female},
        #user{name = "Kate", age = 25, gender = female},
        #user{name = "John", age = 20, gender = male}
    ].