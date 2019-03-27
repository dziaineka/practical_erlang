-module(task_extra).

-export([trim/1, trim/2]).

-record(result, {trimmed_string, another_letter_met, storage}).

% trim spaces
trim(String) ->
    trim(String, 32).

% trim specifyed char
trim(String, Char) ->
    trim(String,
         Char,
         #result{trimmed_string=[], another_letter_met=false, storage=[]}).

% string to trim is empty
trim([], Char, #result{trimmed_string=Trimmed})
when is_number(Char) ->
    lists:reverse(Trimmed);

% symbols matched and it's the beginning of the string
trim([Char|Tail],
     Char,
     #result{another_letter_met=false, storage=Storage})
when is_number(Char) ->
    trim(Tail,
         Char,
         #result{trimmed_string=[], another_letter_met=false, storage=Storage});

% first mismatched symbol
trim([Head|Tail],
     Char,
     #result{trimmed_string=Trimmed_string,
             another_letter_met=false,
             storage=Storage})
when is_number(Char) ->
    Another_letter_met = true,
    NewTrimmedStr = [Head|Trimmed_string],

    trim(Tail,
         Char,
         #result{trimmed_string=NewTrimmedStr,
                 another_letter_met=Another_letter_met,
                 storage=Storage});

% dont delete chars in the middle of the string
trim([Char|Tail],
     Char,
     #result{trimmed_string=Trimmed_string,
             another_letter_met=true,
             storage=Storage})
when is_number(Char) ->
    New_storage = [Char|Storage],

    trim(Tail,
         Char,
         #result{trimmed_string=Trimmed_string,
                 another_letter_met=true,
                 storage=New_storage});

% another mismatching symbol has been met
trim([Head|Tail],
     Char,
     #result{trimmed_string=Trimmed_string,
             another_letter_met=true,
             storage=Storage})
when is_number(Char) ->
    New_trimmed = [Head|Storage ++ Trimmed_string],
    New_storage = [],

    trim(Tail,
         Char,
         #result{trimmed_string=New_trimmed,
                 another_letter_met=true,
                 storage=New_storage});


trim(String, _, _) ->
    String.



