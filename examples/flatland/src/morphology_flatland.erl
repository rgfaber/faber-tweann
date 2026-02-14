%% @doc Flatland morphology - Predator/prey simulation.
%%
%% Agents navigate a 2D flatland environment, sense distance, and control
%% two-wheeled locomotion.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(morphology_flatland).

-behaviour(morphology_behaviour).

-include("records.hrl").

%% morphology_behaviour callbacks
-export([get_sensors/1, get_actuators/1]).

-spec get_sensors(prey | predator) -> [#sensor{}].
get_sensors(Morphology) when Morphology =:= prey; Morphology =:= predator ->
    Pi = math:pi(),
    Spread = Pi / 2,
    Density = 5,
    ROffset = 0,
    [
        #sensor{
            name = distance_scanner,
            type = standard,
            scape = {public, flatland},
            format = no_geo,
            vl = Density,
            parameters = [Spread, Density, ROffset]
        }
    ];
get_sensors(_) ->
    error(invalid_morphology).

-spec get_actuators(prey | predator) -> [#actuator{}].
get_actuators(Morphology) when Morphology =:= prey; Morphology =:= predator ->
    [
        #actuator{
            name = two_wheels,
            type = standard,
            scape = {public, flatland},
            format = no_geo,
            vl = 2,
            parameters = [2]
        }
    ];
get_actuators(_) ->
    error(invalid_morphology).
