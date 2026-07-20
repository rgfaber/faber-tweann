%% @doc Cue-memory T-maze morphology.
%%
%% A clean memory benchmark (see tmaze_sim): the network senses a cue, then must
%% carry it across a corridor to a junction and decide which way to turn. Two
%% sensor inputs [Cue, JunctionFlag]; one actuator output (turn decision).
%%
%% The default parameter [2] is the corridor delay: the cue must be held for two
%% blank steps before the junction. A memoryless network caps at ~50%; a network
%% with memory (recurrent wiring or LTC internal_state) can reach 100%.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(morphology_tmaze).

-behaviour(morphology_behaviour).

-include("records.hrl").

-export([get_sensors/1, get_actuators/1]).

-spec get_sensors(tmaze) -> [#sensor{}].
get_sensors(tmaze) ->
    [
        #sensor{
            name = tmaze_GetInput,
            type = standard,
            scape = {private, tmaze_sim},
            vl = 2,
            parameters = [2]
        }
    ];
get_sensors(_) ->
    error(invalid_morphology).

-spec get_actuators(tmaze) -> [#actuator{}].
get_actuators(tmaze) ->
    [
        #actuator{
            name = tmaze_Decide,
            type = standard,
            scape = {private, tmaze_sim},
            vl = 1,
            parameters = [2]
        }
    ];
get_actuators(_) ->
    error(invalid_morphology).
