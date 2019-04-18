-module(st_tcp_ip).

-export([on_connect/0]).

on_connect() ->
    Socket = make_ref(),
    supervisor:start_child(st_player_sup, [Socket]).

% auth()