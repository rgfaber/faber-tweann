%% @doc XOR morphology - Classic neural network benchmark.
%%
%% The XOR (exclusive or) problem is a simple classification task used to test
%% neural networks. The goal is to learn the XOR function which outputs 1 when
%% inputs are different, and 0 when they are the same.
%%
%% == Truth Table ==
%%
%% | Input 1 | Input 2 | Output |
%% |---------|---------|--------|
%% | 0       | 0       | 0      |
%% | 0       | 1       | 1      |
%% | 1       | 0       | 1      |
%% | 1       | 1       | 0      |
%%
%% == Usage ==
%%
%% ```
%% % Register morphology
%% morphology_registry:register(xor_mimic, morphology_xor).
%%
%% % Create agent
%% Constraint = #constraint{morphology = xor_mimic},
%% {ok, AgentId} = genotype:construct_agent(Constraint).
%% ```
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(morphology_xor).

-behaviour(morphology_behaviour).

-include("records.hrl").

%% morphology_behaviour callbacks
-export([get_sensors/1, get_actuators/1]).

%%==============================================================================
%% Callbacks
%%==============================================================================

%% @doc Get sensors for XOR morphology.
%%
%% Returns a single sensor that reads 2 inputs (the XOR operands).
%%
%% @param xor_mimic The morphology name
%% @returns List containing one sensor record
-spec get_sensors(xor_mimic) -> [#sensor{}].
get_sensors(xor_mimic) ->
    [
        #sensor{
            name = xor_GetInput,
            type = standard,
            scape = {private, xor_sim},
            vl = 2
        }
    ];
get_sensors(_) ->
    error(invalid_morphology).

%% @doc Get actuators for XOR morphology.
%%
%% Returns a single actuator that outputs 1 value (the XOR result).
%%
%% @param xor_mimic The morphology name
%% @returns List containing one actuator record
-spec get_actuators(xor_mimic) -> [#actuator{}].
get_actuators(xor_mimic) ->
    [
        #actuator{
            name = xor_SendOutput,
            type = standard,
            scape = {private, xor_sim},
            vl = 1
        }
    ];
get_actuators(_) ->
    error(invalid_morphology).
