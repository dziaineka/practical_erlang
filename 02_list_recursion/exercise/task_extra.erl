-module(task_extra).

-export([trim/1, trim/2]).

-record(result, {trimmed_string, another_letter_met, storage}).

trim(String) ->
    trim(String, " ").

trim(String, Char) ->
    trim(String,
         Char,
         #result{trimmed_string=[], another_letter_met=false, storage=[]}).


trim([], [CharNumber], #result{trimmed_string=Trimmed})
when is_number(CharNumber) ->
    lists:reverse(Trimmed);

trim([CharNumber|Tail],
     [CharNumber],
     #result{another_letter_met=false, storage=Storage})
when is_number(CharNumber) ->
    trim(Tail,
         [CharNumber],
         #result{trimmed_string=[], another_letter_met=false, storage=Storage});

trim([Head|Tail],
     [CharNumber],
     #result{trimmed_string=Trimmed_string,
             another_letter_met=false,
             storage=Storage})
when is_number(CharNumber) ->
    Another_letter_met = true,
    NewTrimmedStr = [Head|Trimmed_string],

    trim(Tail,
         [CharNumber],
         #result{trimmed_string=NewTrimmedStr,
                 another_letter_met=Another_letter_met,
                 storage=Storage});

trim([CharNumber|Tail],
     [CharNumber],
     #result{trimmed_string=Trimmed_string,
             another_letter_met=true,
             storage=Storage})
when is_number(CharNumber) ->
    New_storage = [CharNumber|Storage],

    trim(Tail,
         [CharNumber],
         #result{trimmed_string=Trimmed_string,
                 another_letter_met=true,
                 storage=New_storage});

trim([Head|Tail],
     [CharNumber],
     #result{trimmed_string=Trimmed_string,
             another_letter_met=true,
             storage=[]})
when is_number(CharNumber) ->
    New_trimmed = [Head|Trimmed_string],
    New_storage = [],

    trim(Tail,
         [CharNumber],
         #result{trimmed_string=New_trimmed,
                 another_letter_met=true,
                 storage=New_storage});

trim([Head|Tail],
     [CharNumber],
     #result{trimmed_string=Trimmed_string,
             another_letter_met=true,
             storage=Storage})
when is_number(CharNumber) ->
    New_trimmed = [Head|[Storage|Trimmed_string]],
    New_storage = [],

    trim(Tail,
         [CharNumber],
         #result{trimmed_string=New_trimmed,
                 another_letter_met=true,
                 storage=New_storage});


trim(String, _, _) ->
    String.



