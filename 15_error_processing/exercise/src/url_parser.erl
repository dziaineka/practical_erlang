-module(url_parser).

-export([parse/1]).


-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(URL) ->
    Result = pipeline(URL, [
        fun get_protocol/1,
        fun get_domain/1,
        fun get_query/1,
        fun get_date/1,
        fun get_path/1
    ]),

    process_results(Result).


pipeline(URL, Parsers) ->
    lists:foldl(
        fun
            (Parser, {ok, PartOfURL, Result}) ->
                case Parser(PartOfURL) of
                    {PartName, {ok, Element}, NewURL} ->
                        {
                            ok,
                            NewURL,
                            Result#{PartName => Element}
                        };

                    {error, Reason} ->
                        {error, Reason}
                end;

            (_Parser, {error, Reason}) ->
                {
                    error,
                    Reason
                }
        end,
        {ok, URL, #{}},
        Parsers
    ).


get_protocol(String) ->
    case binary:split(String, <<"://">>) of
        [Protocol, Rest] ->
            {protocol, {ok, Protocol}, Rest};

        _ ->
            {error, invalid_protocol}
    end.


get_domain(String) ->
    case binary:split(String, <<"/">>) of
        [Domain, Rest] ->
            {domain, {ok, Domain}, Rest};

        _ ->
            {error, invalid_domain}
    end.


get_query(String) ->
    case binary:split(String, <<"?">>) of
        [Path, Query] ->
            {query, {ok, Query}, Path};

        [Path] ->
            {query, {ok, <<>>}, Path}
    end.


get_date(String) ->
    case binary:split(String, <<"/">>, [global]) of
        [Year, Month, Day | _Rest] ->
            case date_valid(Year, Month, Day) of
                {ok, Date} ->
                    {date, {ok, Date}, String};

                _ ->
                    {date, {ok, undefined}, String}
            end;

        _ ->
            {date, {ok, undefined}, String}
    end.


date_valid(BinYear, BinMonth, BinDay) ->
    try
        {Year, _} = string:to_integer(binary:bin_to_list(BinYear)),
        {Month, _} = string:to_integer(binary:bin_to_list(BinMonth)),
        {Day, _} = string:to_integer(binary:bin_to_list(BinDay)),

        if
            Month >= 1 andalso Month =< 12 andalso
            Day >= 1 andalso Day =< 31 ->
                {ok, {Year, Month, Day}};

            true ->
                error
        end
    catch
        _ ->
            error
    end.


get_path(String) ->
    Path = lists:filter(
        fun
            (<<>>) -> false;
            (_) -> true
        end,
        binary:split(String, <<"/">>, [global])
    ),

    {path, {ok, Path}, String}.


process_results({error, Reason}) ->
    {error, Reason};

process_results({ok, _, Result}) ->
    {ok, Result}.
