-module(sup_2).
-behaviour(supervisor).

-export([start_link/0, init/1, add_worker/1, remove_worker/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one
        intensity => 10,
        period => 60},

    ChildSpecifications = [
        #{
            id => worker_3,
            start => {worker, start_link, [worker_3]},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [worker]
        },

        #{
            id => worker_4,
            start => {worker, start_link, [worker_4]},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [worker]
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.


add_worker(Id) ->
    supervisor:start_child(
        ?MODULE,
        #{
            id => Id,
            start => {worker, start_link, [Id]},
            restart => permanent, % permanent | transient | temporary
            shutdown => 2000,
            type => worker, % worker | supervisor
            modules => [worker]
        }
    ).


remove_worker(Id) ->
    supervisor:terminate_child(?MODULE, Id),
    supervisor:delete_child(?MODULE, Id).


