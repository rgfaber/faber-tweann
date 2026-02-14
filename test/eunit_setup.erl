%% @doc EUnit setup - registers example morphologies before tests.
-module(eunit_setup).

-export([setup/0]).

%%==============================================================================
%% Setup
%%==============================================================================

%% @doc Called before running tests.
%%
%% Registers all example morphologies to make them available during testing.
setup() ->
    %% Ensure application is started (for registry)
    application:ensure_all_started(faber_tweann),

    %% Register all example morphologies
    test_helper:register_all_example_morphologies(),

    ok.
