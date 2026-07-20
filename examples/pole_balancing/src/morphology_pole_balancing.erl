%% @doc Pole balancing morphology - Classic RL benchmark.
%%
%% The pole balancing task requires the agent to balance one or more poles
%% on a cart by applying forces left or right. All variants share the pb_sim
%% scape; they differ only in what the sensor observes and whether the second
%% pole's fall is checked.
%%
%% Named variants (each a distinct morphology, one shared scape):
%%
%%   pb_1_with_velocity    single pole, full state  [CPos,CVel,PAngle1,PVel1]   vl=4
%%   pb_1_without_velocity single pole, no velocity [CPos,PAngle1]              vl=2
%%   pb_2_with_velocity    double pole, full state  [CPos,CVel,PA1,PA2,PV1,PV2] vl=6
%%   pb_2_without_velocity double pole, no velocity [CPos,PAngle1,PAngle2]      vl=3
%%   pole_balancing        double pole, no velocity [CPos,PAngle1,PAngle2]      vl=3
%%
%% The without-velocity variants are non-Markov: the network must integrate
%% velocity itself, so they require working recurrent evaluation. Double pole
%% without velocity is the genuine memory benchmark (single pole without velocity
%% is only mildly non-Markov, insight 016). The with-velocity variants observe
%% full state and are feedforward-solvable.
%%
%% pb_2_without_velocity uses a BOUNDED goal (balance a few thousand steps rather
%% than the faithful 100000) so evaluation cost stays cheap and no run times out
%% (insight 016 harness lesson). pole_balancing keeps the faithful 100000-step
%% goal for a Table-14.1 comparison.
%%
%% Actuator parameters are [Damping_Flag, DPB_Flag | GoalSteps]: DPB_Flag=0 for
%% single pole (second pole's fall ignored), DPB_Flag=1 for double pole; the
%% optional third element caps the balanced-step goal (default 100000 in pb_sim).
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(morphology_pole_balancing).

-behaviour(morphology_behaviour).

-include("records.hrl").

%% morphology_behaviour callbacks
-export([get_sensors/1, get_actuators/1]).

-spec get_sensors(atom()) -> [#sensor{}].
get_sensors(pb_1_with_velocity) ->
    [sensor(4)];
get_sensors(pb_1_without_velocity) ->
    [sensor(2)];
get_sensors(pb_2_with_velocity) ->
    [sensor(6)];
get_sensors(pb_2_without_velocity) ->
    [sensor(3)];
get_sensors(pole_balancing) ->
    [sensor(3)];
get_sensors(_) ->
    error(invalid_morphology).

-spec get_actuators(atom()) -> [#actuator{}].
get_actuators(pb_1_with_velocity) ->
    [actuator(0)];
get_actuators(pb_1_without_velocity) ->
    [actuator(0)];
get_actuators(pb_2_with_velocity) ->
    [actuator(1)];
get_actuators(pb_2_without_velocity) ->
    [actuator(1, 5000)];
get_actuators(pole_balancing) ->
    [actuator(1)];
get_actuators(_) ->
    error(invalid_morphology).

%%%============================================================================
%%% Internal
%%%============================================================================

sensor(Variant) ->
    #sensor{
        name = pb_GetInput,
        type = standard,
        scape = {private, pb_sim},
        vl = Variant,
        parameters = [Variant]
    }.

actuator(DPBFlag) ->
    #actuator{
        name = pb_SendOutput,
        type = standard,
        scape = {private, pb_sim},
        vl = 1,
        parameters = [with_damping, DPBFlag]
    }.

%% As actuator/1 but caps the balanced-step goal (pb_sim reads the third param),
%% keeping evaluations bounded for tractable experiments.
actuator(DPBFlag, GoalSteps) ->
    #actuator{
        name = pb_SendOutput,
        type = standard,
        scape = {private, pb_sim},
        vl = 1,
        parameters = [with_damping, DPBFlag, GoalSteps]
    }.
