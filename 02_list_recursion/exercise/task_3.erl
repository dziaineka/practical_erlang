-module(task_3).

-export([member/2, filter/2]).

-include_lib("eunit/include/eunit.hrl").


%% implement lists:member/2
%% http://www.erlang.org/doc/man/lists.html#member-2
member(_Elem, []) ->
    false;
member(Elem, [Head|Tale]) ->
    case Head == Elem of
        false -> member(Elem, Tale);
        true -> true
    end.


member_test() ->
    ?assertEqual(true, member(55, [1,2,55,77])),
    ?assertEqual(false, member(55, [])),
    ?assertEqual(false, member(55, [1,2,77])),
    ?assertEqual(true, member("ab", ["dd", "bd", "ab"])),
    ok.



%% implement lists:filter/2
%% http://www.erlang.org/doc/man/lists.html#filter-2
filter(Pred, List) ->
    filter(Pred, List, []).

filter(_Pred, [], FilteredList) ->
    task_2:reverse(FilteredList);
filter(Pred, [Head|Tale], FilteredList) ->
    case Pred(Head) of
        false -> filter(Pred, Tale, FilteredList);
        true -> filter(Pred, Tale, [Head|FilteredList])
    end.

filter_test() ->
    F = fun(Val) -> Val rem 5 =:= 0 end,
    ?assertEqual([], filter(F, [])),
    ?assertEqual([], filter(F, [1,2,3,4])),
    ?assertEqual([5,10], filter(F, [1,2,3,4,5,6,7,8,9,10])),
    ok.
