%% @doc Test helper functions for registering example morphologies.
-module(test_helper).

-export([register_all_example_morphologies/0, unregister_all_example_morphologies/0]).

%% @doc Register all example morphologies for testing.
%%
%% Call this in test setup to make example morphologies available.
register_all_example_morphologies() ->
    ok = morphology_registry:register(xor_mimic, morphology_xor),
    ok = morphology_registry:register(pole_balancing, morphology_pole_balancing),
    ok = morphology_registry:register(forex_trader, morphology_forex),
    ok = morphology_registry:register(prey, morphology_flatland),
    ok = morphology_registry:register(predator, morphology_flatland),
    ok = morphology_registry:register(snake, morphology_snake),
    ok.

%% @doc Unregister all example morphologies.
unregister_all_example_morphologies() ->
    ok = morphology_registry:unregister(xor_mimic),
    ok = morphology_registry:unregister(pole_balancing),
    ok = morphology_registry:unregister(forex_trader),
    ok = morphology_registry:unregister(prey),
    ok = morphology_registry:unregister(predator),
    ok = morphology_registry:unregister(snake),
    ok.
