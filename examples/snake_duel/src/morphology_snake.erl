%% @doc Snake Duel morphology - LTC-enabled snake AI for competitive play.
%%
%% This morphology defines sensors and actuators for snake agents in the
%% Snake Duel arena. Designed to work with LTC (Liquid Time-Constant)
%% neurons for temporal awareness and adaptive behavior.
%%
%% Sensor Architecture:
%%   - Vision cone: 8 rays detecting food, enemies, self, walls
%%   - Self-awareness: length, energy, speed, health
%%   - Position: normalized x/y coordinates
%%   - Compass: facing direction (N/E/S/W one-hot)
%%
%% Actuator Architecture:
%%   - Direction: turn_left, turn_right, forward_bias
%%   - Speed: boost multiplier
%%   - Intent: confidence, aggression
%%
%% LTC Benefits for Snake AI:
%%   - Temporal memory: Remember recent moves and food positions
%%   - Adaptive response: Fast when hunting, cautious when threatened
%%   - Pattern learning: Recognize opponent behavior over time
%%
%% Usage:
%%   morphology_registry:register(snake, morphology_snake).
%%   morphology_registry:register(snake_ltc, morphology_snake).
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(morphology_snake).

-behaviour(morphology_behaviour).

-include("records.hrl").

%% morphology_behaviour callbacks
-export([get_sensors/1, get_actuators/1]).

%% Configuration
-define(VISION_RAYS, 8).          %% Number of vision rays in cone
-define(VISION_CHANNELS, 4).      %% food, enemy, self, wall per ray
-define(VISION_CONE_ANGLE, 180).  %% Degrees (forward-facing semicircle)

%%==============================================================================
%% Morphology Behaviour Callbacks
%%==============================================================================

%% @doc Get sensors for snake morphology.
%%
%% Returns a list of sensors that provide the snake with perception:
%% 1. Vision cone - spatial awareness
%% 2. Self state - internal state awareness
%% 3. Position - location awareness
%% 4. Compass - direction awareness
%%
%% @param Morphology snake | snake_ltc
%% @returns List of sensor records
-spec get_sensors(snake | snake_ltc) -> [#sensor{}].
get_sensors(Morphology) when Morphology =:= snake; Morphology =:= snake_ltc ->
    [
        vision_cone_sensor(),
        self_state_sensor(),
        position_sensor(),
        compass_sensor()
    ];
get_sensors(_) ->
    error(invalid_morphology).

%% @doc Get actuators for snake morphology.
%%
%% Returns a list of actuators that allow the snake to act:
%% 1. Direction control - steering
%% 2. Speed control - boost/throttle
%% 3. Intent signals - confidence/aggression (for behavioral expression)
%%
%% @param Morphology snake | snake_ltc
%% @returns List of actuator records
-spec get_actuators(snake | snake_ltc) -> [#actuator{}].
get_actuators(Morphology) when Morphology =:= snake; Morphology =:= snake_ltc ->
    [
        direction_actuator(),
        speed_actuator(),
        intent_actuator()
    ];
get_actuators(_) ->
    error(invalid_morphology).

%%==============================================================================
%% Sensor Definitions
%%==============================================================================

%% @doc Vision cone sensor - perceives objects in a forward-facing cone.
%%
%% Output vector length: VISION_RAYS * VISION_CHANNELS = 8 * 4 = 32
%%
%% Each ray outputs 4 values (normalized distances 0.0-1.0):
%%   [food_dist, enemy_dist, self_dist, wall_dist]
%%
%% Ray angles spread across VISION_CONE_ANGLE degrees centered forward.
vision_cone_sensor() ->
    VL = ?VISION_RAYS * ?VISION_CHANNELS,
    #sensor{
        name = snake_vision_cone,
        type = standard,
        scape = {public, snake_duel},
        format = no_geo,
        vl = VL,
        parameters = [
            ?VISION_RAYS,
            ?VISION_CHANNELS,
            ?VISION_CONE_ANGLE
        ]
    }.

%% @doc Self-state sensor - perceives internal state.
%%
%% Output vector length: 4
%%   [length, energy, speed, health]
%%
%% All values normalized to 0.0-1.0 range.
self_state_sensor() ->
    #sensor{
        name = snake_self_state,
        type = standard,
        scape = {public, snake_duel},
        format = no_geo,
        vl = 4,
        parameters = [length, energy, speed, health]
    }.

%% @doc Position sensor - perceives position on grid.
%%
%% Output vector length: 2
%%   [x, y] - normalized to 0.0-1.0 relative to grid bounds
position_sensor() ->
    #sensor{
        name = snake_position,
        type = standard,
        scape = {public, snake_duel},
        format = no_geo,
        vl = 2,
        parameters = [x, y]
    }.

%% @doc Compass sensor - perceives facing direction.
%%
%% Output vector length: 4
%%   [north, east, south, west] - one-hot encoding
compass_sensor() ->
    #sensor{
        name = snake_compass,
        type = standard,
        scape = {public, snake_duel},
        format = no_geo,
        vl = 4,
        parameters = [north, east, south, west]
    }.

%%==============================================================================
%% Actuator Definitions
%%==============================================================================

%% @doc Direction actuator - controls steering.
%%
%% Input vector length: 3
%%   [turn_left, turn_right, forward_bias]
%%
%% Values in range -1.0 to 1.0 (continuous control).
%% Final direction is computed from relative turn signals.
direction_actuator() ->
    #actuator{
        name = snake_direction,
        type = standard,
        scape = {public, snake_duel},
        format = no_geo,
        vl = 3,
        parameters = [turn_left, turn_right, forward_bias]
    }.

%% @doc Speed actuator - controls movement speed.
%%
%% Input vector length: 1
%%   [speed_boost] - 0.0 to 1.0 boost multiplier
%%
%% Base speed is determined by game rules (length, terrain).
%% Boost drains energy but increases speed up to 2x.
speed_actuator() ->
    #actuator{
        name = snake_speed,
        type = standard,
        scape = {public, snake_duel},
        format = no_geo,
        vl = 1,
        parameters = [speed_boost]
    }.

%% @doc Intent actuator - expresses behavioral signals.
%%
%% Input vector length: 2
%%   [confidence, aggression]
%%
%% These signals can influence:
%%   - Action commitment (high confidence = decisive moves)
%%   - Behavior mode (high aggression = hunting, low = fleeing)
%%
%% Primarily useful for behavioral analysis and visualization.
intent_actuator() ->
    #actuator{
        name = snake_intent,
        type = standard,
        scape = {public, snake_duel},
        format = no_geo,
        vl = 2,
        parameters = [confidence, aggression]
    }.

%%==============================================================================
%% Documentation: Using with LTC Neurons
%%==============================================================================
%%
%% To create an LTC-enabled snake agent:
%%
%%   1. Register the morphology:
%%      morphology_registry:register(snake_ltc, morphology_snake).
%%
%%   2. Create constraint with LTC neuron preference:
%%      Constraint = #constraint{
%%          morphology = snake_ltc,
%%          neural_afs = [tanh, sigmoid],
%%          neuron_types = [cfc, ltc, standard],  %% Prefer CfC
%%          ltc_time_constant_range = {0.1, 10.0},
%%          ltc_state_bound_range = {0.5, 2.0}
%%      }.
%%
%%   3. Construct agent:
%%      {ok, AgentId} = genotype:construct_agent(Constraint).
%%
%% LTC neurons are particularly beneficial for snake AI because:
%%
%%   - Temporal awareness: LTC internal state remembers recent inputs,
%%     helping the snake track moving food and predict enemy paths.
%%
%%   - Adaptive response: The liquid time constant adjusts based on
%%     input dynamics. High activity (danger) = fast response.
%%     Low activity (safe) = smooth, efficient movement.
%%
%%   - Pattern learning: Over evolution, LTC parameters (tau, A) adapt
%%     to recognize temporal patterns like opponent hunting strategies.
%%
%% Sensor Total: 32 + 4 + 2 + 4 = 42 inputs
%% Actuator Total: 3 + 1 + 2 = 6 outputs
%%
%% Recommended hidden layer: 16-32 LTC/CfC neurons for good performance.
