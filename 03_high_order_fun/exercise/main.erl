-module(main).

-export([sample_champ/0, get_stat/1, filter_sick_players/1, make_pairs/2]).
-include_lib("eunit/include/eunit.hrl").


sample_champ() ->
    [
     {team, "Crazy Bulls",
      [{player, "Big Bull",        22, 545, 99},
       {player, "Small Bull",      18, 324, 95},
       {player, "Bull Bob",        19,  32, 45},
       {player, "Bill The Bull",   23, 132, 85},
       {player, "Tall Ball Bull",  38,  50, 50},
       {player, "Bull Dog",        35, 201, 91},
       {player, "Bull Tool",       29,  77, 96},
       {player, "Mighty Bull",     22, 145, 98}
      ]},
     {team, "Cool Horses",
      [{player, "Lazy Horse",      21, 423, 80},
       {player, "Sleepy Horse",    23, 101, 35},
       {player, "Horse Doors",     19,  87, 23},
       {player, "Rainbow",         21, 200, 17},
       {player, "HoHoHorse",       20, 182, 44},
       {player, "Pony",            25,  96, 76},
       {player, "Hippo",           17, 111, 96},
       {player, "Hop-Hop",         31, 124, 49}
      ]},
     {team, "Fast Cows",
      [{player, "Flash Cow",       18,  56, 34},
       {player, "Cow Bow",         28,  89, 90},
       {player, "Boom! Cow",       20, 131, 99},
       {player, "Light Speed Cow", 21, 201, 98},
       {player, "Big Horn",        23,  38, 93},
       {player, "Milky",           25,  92, 95},
       {player, "Jumping Cow",     19, 400, 98},
       {player, "Cow Flow",        18, 328, 47}
      ]},
     {team, "Fury Hens",
      [{player, "Ben The Hen",     57, 403, 83},
       {player, "Hen Hen",         20, 301, 56},
       {player, "Son of Hen",      21, 499, 43},
       {player, "Beak",            22,  35, 96},
       {player, "Superhen",        27,  12, 26},
       {player, "Henman",          20,  76, 38},
       {player, "Farm Hen",        18, 131, 47},
       {player, "Henwood",         40, 198, 77}
      ]},
     {team, "Extinct Mosters",
      [{player, "T-Rex",           21, 999, 99},
       {player, "Velociraptor",    29, 656, 99},
       {player, "Giant Mammoth",   30, 382, 99},
       {player, "The Big Croc",    42, 632, 99},
       {player, "Huge Pig",        18, 125, 98},
       {player, "Saber-Tooth",     19, 767, 97},
       {player, "Beer Bear",       24, 241, 99},
       {player, "Pure Horror",     31,  90, 43}
      ]}
    ].


get_players_sums({player, _Name, Age, Rating, _Health},
                 {NumPlayers, SumAge, SumRating}) ->
    {NumPlayers + 1, SumAge + Age, SumRating + Rating}.


sum_champ_data({team, _Name, Players},
               {NumTeams, NumPlayers, SumAge, SumRating}) ->
    {TeamNumPlayers, TeamSumAge, TeamSumRating} = lists:foldl(
                                                    fun get_players_sums/2,
                                                    {0, 0, 0},
                                                    Players
                                                ),

    {
        NumTeams + 1,
        NumPlayers + TeamNumPlayers,
        SumAge + TeamSumAge,
        SumRating + TeamSumRating
    }.


get_stat(Champ) ->
    {NumTeams, NumPlayers, SumAge, SumRating} = lists:foldl(
                                                    fun sum_champ_data/2,
                                                    {0, 0, 0, 0},
                                                    Champ
                                                ),

    {NumTeams, NumPlayers, SumAge/NumPlayers, SumRating/NumPlayers}.


get_stat_test() ->
    ?assertEqual({5,40,24.85,242.8}, get_stat(sample_champ())),
    ok.


remove_sick_players({team, TeamName, Players}) ->
    FilteredPlayers = lists:filter(
        fun({player, _Name, _Age, _Rating, Health}) -> Health >= 50 end,
        Players
    ),

    case length(FilteredPlayers) > 5 of
        false -> false;
        true -> {true, {team, TeamName, FilteredPlayers}}
    end.


filter_sick_players(Champ) ->
    lists:filtermap(fun remove_sick_players/1, Champ).


filter_sick_players_test() ->
    Result = [{team, "Crazy Bulls",
               [{player, "Big Bull",        22, 545, 99},
                {player, "Small Bull",      18, 324, 95},
                {player, "Bill The Bull",   23, 132, 85},
                {player, "Tall Ball Bull",  38,  50, 50},
                {player, "Bull Dog",        35, 201, 91},
                {player, "Bull Tool",       29,  77, 96},
                {player, "Mighty Bull",     22, 145, 98}
               ]},
              {team, "Fast Cows",
               [{player, "Cow Bow",         28,  89, 90},
                {player, "Boom! Cow",       20, 131, 99},
                {player, "Light Speed Cow", 21, 201, 98},
                {player, "Big Horn",        23,  38, 93},
                {player, "Milky",           25,  92, 95},
                {player, "Jumping Cow",     19, 400, 98}
               ]},
              {team, "Extinct Mosters",
               [{player, "T-Rex",           21, 999, 99},
                {player, "Velociraptor",    29, 656, 99},
                {player, "Giant Mammoth",   30, 382, 99},
                {player, "The Big Croc",    42, 632, 99},
                {player, "Huge Pig",        18, 125, 98},
                {player, "Saber-Tooth",     19, 767, 97},
                {player, "Beer Bear",       24, 241, 99}
               ]}
             ],
    ?assertEqual(Result, filter_sick_players(sample_champ())),
    ok.


make_pairs({team, _TeamName1, Players1}, {team, _TeamName2, Players2}) ->
    [
        {Team1Player, Team2Player} ||
        {player, Team1Player, _Age1, Rating1, _Health1} <- Players1,
        {player, Team2Player, _Age2, Rating2, _Health2} <- Players2,
        Rating1 + Rating2 > 600
    ].


make_pairs_test() ->
    [T1, T2, T3, T4, _] = sample_champ(),
    ?assertEqual([{"Big Bull","Lazy Horse"},
                  {"Big Bull","Sleepy Horse"},
                  {"Big Bull","Horse Doors"},
                  {"Big Bull","Rainbow"},
                  {"Big Bull","HoHoHorse"},
                  {"Big Bull","Pony"},
                  {"Big Bull","Hippo"},
                  {"Big Bull","Hop-Hop"},
                  {"Small Bull","Lazy Horse"},
                  {"Bull Dog","Lazy Horse"}],
                 main:make_pairs(T1, T2)),
    ?assertEqual([{"Lazy Horse","Light Speed Cow"},
                  {"Lazy Horse","Jumping Cow"},
                  {"Lazy Horse","Cow Flow"}],
                 main:make_pairs(T2, T3)),
    ?assertEqual([{"Ben The Hen","Light Speed Cow"},
                  {"Ben The Hen","Jumping Cow"},
                  {"Ben The Hen","Cow Flow"},
                  {"Hen Hen","Jumping Cow"},
                  {"Hen Hen","Cow Flow"},
                  {"Son of Hen","Boom! Cow"},
                  {"Son of Hen","Light Speed Cow"},
                  {"Son of Hen","Jumping Cow"},
                  {"Son of Hen","Cow Flow"}],
                 main:make_pairs(T4, T3)),
    ok.
