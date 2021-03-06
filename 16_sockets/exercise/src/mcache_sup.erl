-module(mcache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    io:format("supervisor started~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => mcache_srv,
            start => {mcache_srv, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [mcache_srv]
        },
        #{
            id => mcache,
            start => {mcache, start_link, []},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [mcache]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.
