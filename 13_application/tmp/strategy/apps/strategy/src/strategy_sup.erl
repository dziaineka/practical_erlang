%%%-------------------------------------------------------------------
%% @doc strategy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(strategy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications =
        [#{id => strategy_worker_1,
           start => {strategy_worker, start_link, [1, "Vasya"]},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [strategy_worker]},

         #{id => strategy_worker_2,
           start => {strategy_worker, start_link, [2, "Kolya"]},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [strategy_worker]}
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.

%%====================================================================
%% Internal functions
%%====================================================================
