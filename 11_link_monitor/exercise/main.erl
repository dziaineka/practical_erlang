-module(main).

-export([parse/1, parse_file/2]).
-export([gather/2]).


parse(Files) ->
    Workers = lists:foldl(
        fun (File, Acc) ->
            Key = run_parse(File),
            Acc#{Key => File}
        end,
        #{},
        Files
    ),

    wait_for_result(Workers, {#{}, #{}}).


run_parse(File) ->
    Pid = spawn(?MODULE, parse_file, [File, self()]),
    Monitor = erlang:monitor(process, Pid),
    {Pid, Monitor}.


parse_file(File, ServerPid) ->
    {ok, Content} = file:read_file(File),
    Lines = binary:split(Content, [<<"\n">>], [global]),

    FilteredLines = lists:filter(
        fun
            (<<>>) -> false;
            (_) -> true
        end,
        Lines
    ),

    Result = lists:foldl(fun count_line/2, #{}, FilteredLines),
    ServerPid ! {result, Result},
    ok.


count_line(Line, Acc) ->
    Record = binary:split(Line, [<<",">>], [global]),

    case Record of
        [_Num, Name, Amount, _Price] ->
            IntAmount = list_to_integer(binary_to_list(Amount)),
            Acc#{Name => IntAmount + maps:get(Name, Acc, 0)}
    end.


wait_for_result(Workers, Storage) when map_size(Workers) == 0 ->
    Storage;

wait_for_result(Workers, {ResultStorage, ErrorStorage}) ->
    receive
        {result, Result} ->
            NewResultStorage = gather(ResultStorage, Result),
            wait_for_result(Workers, {NewResultStorage, ErrorStorage});

        {'DOWN', Monitor, process, Pid, Reason} ->
            NewWorkers = maps:remove({Pid, Monitor}, Workers),

            case Reason of
                normal ->
                    wait_for_result(NewWorkers, {ResultStorage, ErrorStorage});

                _ ->
                    {ok, File} = maps:find({Pid, Monitor}, Workers),
                    NewErrorStorage = ErrorStorage#{File => Reason},

                    wait_for_result(NewWorkers,
                                    {ResultStorage, NewErrorStorage})
            end
    end.


gather(Map1, Map2) ->
    maps:fold(
        fun
            (Name, Amount, AccMap) ->
                AccMap#{Name => Amount + maps:get(Name, AccMap, 0)}
        end,
        Map1,
        Map2
    ).


