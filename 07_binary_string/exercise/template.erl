-module(template).

-export([parse/2]).


get_binary_value(Key, Map) ->
    Value = maps:get(Key, Map, <<"">>),

    if
        is_binary(Value) ->
            Value;

        is_list(Value) ->
            unicode:characters_to_binary(Value);

        is_integer(Value) ->
            unicode:characters_to_binary(integer_to_list(Value));

        true ->
            <<"">>
    end.

parse(Str, Data) when is_binary(Str) ->
    case binary:match(Str, <<"{{">>) of
        nomatch ->
            Str;

        _ ->
            [Begin, UnsplittedRest] = binary:split(Str, [<<"{{">>]),
            [Placeholder, End] = binary:split(UnsplittedRest, [<<"}}">>]),
            parse(
                iolist_to_binary(
                    [Begin, get_binary_value(Placeholder, Data), End]
                ),
                Data)
    end.

