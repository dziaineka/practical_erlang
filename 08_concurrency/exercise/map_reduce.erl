-module(map_reduce).

-export([start/1, collect_lists/3, count_words/1]).


get_word_amount(WordsList, WordsAmount) ->
    lists:foldl(
        fun (Word, Amount) ->
            Amount#{Word => maps:get(Word, Amount, 0) + 1}
        end,
        WordsAmount,
        WordsList
    ).


count_words_thread(File) ->
    spawn(?MODULE, count_words, [File]).


count_words(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            WordsList = binary:split(
                Content,
                [<<" ">>, <<"\n">>, <<"\r">>],
                [global]
            ),

            WordsAmount = get_word_amount(WordsList, #{}),
            collector ! {amount, WordsAmount};

        {error, Reason} ->
            collector ! {error, Reason}
    end.


start(Files) ->
    Pid = spawn(?MODULE, collect_lists, [length(Files), maps:new(), self()]),
    register(collector, Pid),

    lists:foreach(fun count_words_thread/1, Files),

    receive
        {total_amount, Amount} ->
            Amount;

        _ ->
            error
    end.


collect_lists(Length, StartAmount, ServerPid) ->
    if
        Length == 0 ->
            % io:format("TestAmount: ~p~n", [StartAmount]),
            ServerPid ! {total_amount, StartAmount};

        true ->
            receive
                {amount, WordsAmount} ->
                    NewAmount = maps:fold(
                        fun (Word, Amount, TotalAmount) ->
                            TotalAmount#{
                                Word => maps:get(Word, TotalAmount, 0) + Amount
                            }
                        end,
                        StartAmount,
                        WordsAmount
                    ),

                    collect_lists(Length - 1, NewAmount, ServerPid);

                {error, Reason} ->
                    io:format("Error: ~p~n", [Reason]),
                    collect_lists(Length - 1, StartAmount, ServerPid)
            end
    end.
