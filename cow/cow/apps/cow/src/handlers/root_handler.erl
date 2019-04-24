-module(root_handler).

-export([init/2]).

init(Req0, State) ->
    io:format("~p~n", [Req0]),

    case cowboy_req:has_body(Req0) of
        true ->
            {ok, ReqBody, Req2} = cowboy_req:read_body(Req0),
            io:format("~p~n", [ReqBody]),
            io:format("~p~n", [Req2]);

        false ->
            ok
    end,

    Headers = #{
        <<"content-type">> => <<"html">>
    },

    Body = <<
        "<html>"
        "<head>"
            "<title>Document</title>"
            "<link rel=\"stylesheet\" href=\"/static/main.css\">"
        "</head>"
        "<body>"
            "Hello Erlang from body!"
        "</body>"
        "</html>"
    >>,

    Req = cowboy_req:reply(200,
        Headers,
        Body,
        Req0),
    {ok, Req, State}.