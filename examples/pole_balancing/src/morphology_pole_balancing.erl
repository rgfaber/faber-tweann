%% @doc Pole balancing morphology - Classic RL benchmark.
%%
%% The pole balancing task requires the agent to balance one or more poles
%% on a cart by applying forces left or right.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(morphology_pole_balancing).

-behaviour(morphology_behaviour).

-include("records.hrl").

%% morphology_behaviour callbacks
-export([get_sensors/1, get_actuators/1]).

-spec get_sensors(pole_balancing) -> [#sensor{}].
get_sensors(pole_balancing) ->
    [
        #sensor{
            name = pb_GetInput,
            type = standard,
            scape = {private, pb_sim},
            vl = 3,
            parameters = [3]
        }
    ];
get_sensors(_) ->
    error(invalid_morphology).

-spec get_actuators(pole_balancing) -> [#actuator{}].
get_actuators(pole_balancing) ->
    [
        #actuator{
            name = pb_SendOutput,
            type = standard,
            scape = {private, pb_sim},
            vl = 1,
            parameters = [with_damping, 1]
        }
    ];
get_actuators(_) ->
    error(invalid_morphology).
