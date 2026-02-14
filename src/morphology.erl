%% @doc Morphology module for sensor/actuator specifications.
%%
%% This module provides the interface for accessing morphologies (problem domains).
%% Morphologies are now registered at runtime via morphology_registry.
%%
%% Using Morphologies:
%%
%% 1. Register a morphology (typically at application startup):
%%
%% morphology_registry:register(xor_mimic, morphology_xor).
%%
%% 2. Use the morphology in agent construction:
%%
%% Constraint = #constraint{morphology = xor_mimic},
%% {ok, AgentId} = genotype:construct_agent(Constraint).
%%
%% Custom Morphologies:
%%
%% To create a custom morphology:
%% 1. Implement morphology_behaviour in your module
%% 2. Register it at runtime
%% 3. Use it in constraints
%%
%% See guides/CUSTOM_MORPHOLOGIES.md for detailed instructions.
%% See examples/ directory for reference implementations.
%%
%% Based on DXNN2 by Gene Sher ("Handbook of Neuroevolution through Erlang").
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(morphology).

-include("records.hrl").

%% API
-export([
    get_InitSensors/1,
    get_InitActuators/1,
    get_Sensors/1,
    get_Actuators/1
]).

%%==============================================================================
%% API Functions
%%==============================================================================

%% @doc Get initial sensors for a morphology.
%%
%% Returns the first sensor from the morphology's sensor list.
%% Used when constructing a new agent to get the default sensor.
%%
%% @param Morphology The morphology name (atom) or {Module, Function} tuple
%% @returns List containing the first sensor record
-spec get_InitSensors(atom() | {module(), atom()}) -> [#sensor{}].
get_InitSensors({M, F}) ->
    % Direct module call (legacy support)
    Sensors = M:F(sensors),
    [hd(Sensors)];
get_InitSensors(Morphology) when is_atom(Morphology) ->
    % Delegate to registry
    Sensors = get_Sensors(Morphology),
    [hd(Sensors)].

%% @doc Get initial actuators for a morphology.
%%
%% Returns the first actuator from the morphology's actuator list.
%% Used when constructing a new agent to get the default actuator.
%%
%% @param Morphology The morphology name (atom) or {Module, Function} tuple
%% @returns List containing the first actuator record
-spec get_InitActuators(atom() | {module(), atom()}) -> [#actuator{}].
get_InitActuators({M, F}) ->
    % Direct module call (legacy support)
    Actuators = M:F(actuators),
    [hd(Actuators)];
get_InitActuators(Morphology) when is_atom(Morphology) ->
    % Delegate to registry
    Actuators = get_Actuators(Morphology),
    [hd(Actuators)].

%% @doc Get all sensors for a morphology.
%%
%% Looks up the morphology in the registry and calls its get_sensors/1 callback.
%%
%% @param Morphology The morphology name (atom) or {Module, Function} tuple
%% @returns List of all sensor records for this morphology
-spec get_Sensors(atom() | {module(), atom()}) -> [#sensor{}].
get_Sensors({M, F}) ->
    % Direct module call (legacy support)
    M:F(sensors);
get_Sensors(Morphology) when is_atom(Morphology) ->
    % Delegate to registered module
    case morphology_registry:get(Morphology) of
        {ok, Module} ->
            Module:get_sensors(Morphology);
        {error, not_found} ->
            error({morphology_not_registered, Morphology,
                   "Register morphology with: morphology_registry:register(" ++
                   atom_to_list(Morphology) ++ ", YourModule)"})
    end.

%% @doc Get all actuators for a morphology.
%%
%% Looks up the morphology in the registry and calls its get_actuators/1 callback.
%%
%% @param Morphology The morphology name (atom) or {Module, Function} tuple
%% @returns List of all actuator records for this morphology
-spec get_Actuators(atom() | {module(), atom()}) -> [#actuator{}].
get_Actuators({M, F}) ->
    % Direct module call (legacy support)
    M:F(actuators);
get_Actuators(Morphology) when is_atom(Morphology) ->
    % Delegate to registered module
    case morphology_registry:get(Morphology) of
        {ok, Module} ->
            Module:get_actuators(Morphology);
        {error, not_found} ->
            error({morphology_not_registered, Morphology,
                   "Register morphology with: morphology_registry:register(" ++
                   atom_to_list(Morphology) ++ ", YourModule)"})
    end.
