%% @doc Weight perturbation utilities for neural network evolution.
%%
%% Provides functions for perturbing weights during evolution:
%% - Single weight perturbation with momentum
%% - Batch weight perturbation
%% - Saturation to prevent numerical overflow
%%
%% == Weight Format ==
%%
%% Weights use the format: {Weight, DeltaWeight, LearningRate, ParameterList}
%% - Weight: Current synaptic weight value
%% - DeltaWeight: Previous change (for momentum)
%% - LearningRate: Plasticity learning rate
%% - ParameterList: Additional plasticity parameters
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(perturbation_utils).

-export([
    perturb_weight/2,
    perturb_weights/2,
    sat/2
]).

%% Maximum weight value (saturation limit).
%% Weights are clamped to [-SAT_LIMIT, SAT_LIMIT] to prevent
%% numerical overflow and maintain network stability.
-define(SAT_LIMIT, math:pi() * 10).

%% @doc Perturb a single weight with momentum.
%%
%% Uses momentum-based perturbation:
%% DW_new = random_noise * spread + DW_old * 0.5
%% W_new = saturate(W + DW_new)
%%
%% The 0.5 momentum factor provides smooth weight trajectories
%% during evolution.
%%
%% @param WeightSpec weight tuple {W, DW, LP, LPs}
%% @param Spread perturbation range
%% @returns new weight tuple
-spec perturb_weight({float(), float(), float(), list()}, float()) ->
    {float(), float(), float(), list()}.
perturb_weight({W, DW, LP, LPs}, Spread) ->
    %% Random noise in [-0.5*Spread, 0.5*Spread] plus momentum
    NewDW = (rand:uniform() - 0.5) * Spread + DW * 0.5,
    NewW = sat(W + NewDW, ?SAT_LIMIT),
    {NewW, NewDW, LP, LPs}.

%% @doc Perturb list of weight specs.
%%
%% Applies perturbation to each weight in the list.
%%
%% @param WeightSpecs list of weight tuples
%% @param Spread perturbation range
%% @returns list of perturbed weight tuples
-spec perturb_weights([{float(), float(), float(), list()}], float()) ->
    [{float(), float(), float(), list()}].
perturb_weights(WeightSpecs, Spread) ->
    [perturb_weight(WS, Spread) || WS <- WeightSpecs].

%% @doc Saturate a value to within +/- limit.
%%
%% Clamps the value to [-Limit, Limit] to prevent overflow.
%%
%% @param Value the value to saturate
%% @param Limit the saturation limit (positive)
%% @returns saturated value
-spec sat(float(), float()) -> float().
sat(Value, Limit) when Value > Limit ->
    Limit;
sat(Value, Limit) when Value < -Limit ->
    -Limit;
sat(Value, _Limit) ->
    Value.
