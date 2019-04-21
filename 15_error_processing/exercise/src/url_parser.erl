-module(url_parser).

-export([parse/1]).


-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(URL) ->
    Parsers = [
        fun get_protocol/1,
        fun get_domain/1,
        fun get_year/1,
        fun get_month/1,
        fun get_day/1,
        fun get_name/1,
        fun get_parameters/1
    ],

    {_, Elements, Errors} = lists:foldl(
        fun (Parser, {PartOfURL, Elements, Errors}) ->
            case Parser(PartOfURL) of
                {PartName, {ok, Result}, NewURL} ->
                    {
                        NewURL,
                        Elements#{PartName => Result},
                        Errors
                    };

                {PartName, {error, Reason}, _} ->
                    {
                        PartOfURL,
                        Elements,
                        Errors#{PartName => Reason}
                    }
            end
        end,
        {URL, #{}, #{}},
        Parsers
    ),

    process_results(Elements, Errors).


capture(String, Pattern) ->
    case re:run(String, Pattern) of
        {match, [Captured]} ->
            % io:format("~p~n~p~n", [String, Captured]),

            SubString = binary:list_to_bin(
                binary:bin_to_list(String, Captured)
            ),

            [_ | NewString] = re:replace(String, Pattern, <<>>),
            {SubString, NewString};

        nomatch ->
            {not_found, String}
    end.


get_element(Element, String, RawPattern, {error, ErrorReason}) ->
    {ok, Pattern} = re:compile(RawPattern, [caseless]),

    case capture(String, Pattern) of
        {not_found, NewString} ->
            {Element, {error, ErrorReason}, NewString};

        {Captured, NewString} ->
            {Element, {ok, Captured}, NewString}
    end;

get_element(Element, String, RawPattern, {default, DafaultValue}) ->
    {ok, Pattern} = re:compile(RawPattern, [caseless]),

    case capture(String, Pattern) of
        {not_found, NewString} ->
            {Element, {ok, DafaultValue}, NewString};

        {Captured, NewString} ->
            {Element, {ok, Captured}, NewString}
    end.


get_protocol(String) ->
    Pattern = <<"(?<protocol>^\w+)(?:\:\/\/)">>,
    % Pattern = <<"protocol">>,
    get_element(protocol, String, Pattern, {error, invalid_protocol}).


get_domain(String) ->
    Pattern = <<"domain">>,
    get_element(domain, String, Pattern, {error, invalid_domain}).


get_year(String) ->
    Pattern = <<"year">>,
    get_element(year, String, Pattern, {default, <<>>}).


get_month(String) ->
    Pattern = <<"month">>,
    get_element(month, String, Pattern, {default, <<>>}).


get_day(String) ->
    Pattern = <<"day">>,
    get_element(day, String, Pattern, {default, <<>>}).


get_name(String) ->
    Pattern = <<"name">>,
    get_element(name, String, Pattern, {default, <<>>}).


get_parameters(String) ->
    Pattern = <<"parameters">>,
    get_element(parameters, String, Pattern, {default, <<>>}).


process_results(Elements, Errors) ->
    case maps:size(Errors) of
        0 ->
            process_elements(Elements);

        _ ->
            {_Key, Value, _} = maps:next(maps:iterator(Errors)),
            {error, Value}
    end.


process_elements(Elements) ->
    #{
        protocol => maps:get(protocol, Elements),
        domain => maps:get(domain, Elements),
        path => get_path(Elements),
        query => maps:get(parameters, Elements),
        date => get_date(Elements)
    }.


get_path(Elements) ->
    lists:filter(
        fun
            (<<>>) -> false;
            (_) -> true
        end,
        [
            maps:get(year, Elements),
            maps:get(month, Elements),
            maps:get(day, Elements),
            maps:get(name, Elements)
        ]
    ).


get_date(Elements) ->
    Year = maps:get(year, Elements),
    Month = maps:get(month, Elements),
    Day = maps:get(day, Elements),
    DateUndefined = (Year == <<>> orelse Month == <<>> orelse Day == <<>>),

    if
        DateUndefined ->
            undefined;

        true ->
            {
                binary:bin_to_list(Year),
                binary:bin_to_list(Month),
                binary:bin_to_list(Day)
            }
    end.
