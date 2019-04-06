-module(template).

-export([parse/2]).

parse(Str, Data) when is_binary(Str) ->
    maps:fold(
        fun (Key, Value, BinString) ->
            Placeholder = [<<"{{">>, Key, <<"}}">>],

            if
                is_binary(Value) ->
                    Substitute = Value;

                is_list(Value) ->
                    Substitute = unicode:characters_to_binary(Value);

                is_integer(Value) ->
                    Substitute = unicode:characters_to_binary(
                                                    integer_to_list(Value));

                true ->
                    Substitute = <<"">>
            end,

            binary:replace(BinString,
                           iolist_to_binary(Placeholder),
                           Substitute,
                           [global])
        end,
        Str,
        Data
    ).

