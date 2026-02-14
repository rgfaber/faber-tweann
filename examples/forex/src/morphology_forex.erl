%% @doc Forex trading morphology - Financial trading agent.
%%
%% Agent reads price indicators and makes trading decisions.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(morphology_forex).

-behaviour(morphology_behaviour).

-include("records.hrl").

%% morphology_behaviour callbacks
-export([get_sensors/1, get_actuators/1]).

-spec get_sensors(forex_trader) -> [#sensor{}].
get_sensors(forex_trader) ->
    HRes = 100,
    [
        #sensor{
            name = fx_PLI,
            type = standard,
            scape = {private, fx_sim},
            format = no_geo,
            vl = HRes,
            parameters = [HRes, close]
        }
    ];
get_sensors(_) ->
    error(invalid_morphology).

-spec get_actuators(forex_trader) -> [#actuator{}].
get_actuators(forex_trader) ->
    [
        #actuator{
            name = fx_Trade,
            type = standard,
            scape = {private, fx_sim},
            format = no_geo,
            vl = 1,
            parameters = []
        }
    ];
get_actuators(_) ->
    error(invalid_morphology).
