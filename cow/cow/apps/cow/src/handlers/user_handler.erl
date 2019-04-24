-module(user_handler).

-export([init/2]).

init(Req0, State) ->
    io:format("wwwwwwww0~n"),
    Info = get_user_info(42),
    % io:format("~p~n", [Req0]),

    PrivDir = code:priv_dir(cow),
    {ok, Tpl} = file:read_file(PrivDir ++ "/tpl/user.html"),

    Headers = #{
        <<"content-type">> => <<"html">>
    },

    io:format("wwwwwwww1~n"),
    Body = bbmustache:render(Tpl, Info),
    % Body = "HELLO",
    io:format("~p~n", [Body]),

    Req = cowboy_req:reply(200,
        Headers,
        Body,
        Req0),
    {ok, Req, State}.

get_user_info(_UserId) ->
    #{
        "username" => <<"Bob">>,
        "attributes" => [
            #{
                "attrname" => "height",
                "attrvalue" => 200
            },
            #{
                "attrname" => "weight",
                "attrvalue" => 300
            }
        ]
    }.