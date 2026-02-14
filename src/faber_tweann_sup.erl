%% @doc Supervisor for faber_tweann application.
%%
%% Supervises the morphology registry.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(faber_tweann_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%==============================================================================
%% API Functions
%%==============================================================================

%% @doc Start the supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%==============================================================================
%% supervisor Callbacks
%%==============================================================================

%% @private
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    ChildSpecs = [
        #{
            id => morphology_registry,
            start => {morphology_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [morphology_registry]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
