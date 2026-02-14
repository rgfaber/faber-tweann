%% @doc Liquid Time-Constant (LTC) neural dynamics.
%%
%% This module implements Liquid Time-Constant neurons and their Closed-form
%% Continuous-time (CfC) approximation for high-performance evaluation.
%%
%% LTC neurons have input-dependent time constants, enabling adaptive temporal
%% processing. This makes them particularly suitable for time-series data and
%% real-time control tasks.
%%
%% == Theory ==
%%
%% The LTC neuron is governed by the ODE:
%%
%%   dx(t)/dt = -[1/tau + f(x,I,theta)] * x(t) + f(x,I,theta) * A
%%
%% Where:
%%   - x(t) is the neuron's internal state at time t
%%   - tau is the base time constant (learnable)
%%   - f(...) is a nonlinear function producing the "liquid" time constant
%%   - A is the state bound (prevents explosion)
%%   - I(t) is the input at time t
%%
%% == CfC Approximation ==
%%
%% Instead of solving the ODE numerically, CfC uses a closed-form approximation:
%%
%%   x(t+dt) = sigma(-f) * x(t) + (1 - sigma(-f)) * h
%%
%% Where:
%%   - sigma is the sigmoid function
%%   - f is the backbone network output (time-constant modulator)
%%   - h is the head network output (target state)
%%
%% CfC is approximately 100x faster than ODE-based LTC while maintaining
%% similar expressivity.
%%
%% == References ==
%%
%% [1] Hasani, R., Lechner, M., et al. (2021). "Liquid Time-constant Networks."
%%     Proceedings of the AAAI Conference on Artificial Intelligence.
%%
%% [2] Hasani, R., Lechner, M., et al. (2022). "Closed-form Continuous-time
%%     Neural Networks." Nature Machine Intelligence.
%%
%% [3] Beer, R.D. (1995). "On the Dynamics of Small Continuous-Time Recurrent
%%     Neural Networks." Adaptive Behavior, 3(4).
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0

-module(ltc_dynamics).

-export([
    %% CfC evaluation (fast, closed-form)
    evaluate_cfc/4,
    evaluate_cfc/5,

    %% ODE-based evaluation (accurate)
    evaluate_ode/5,
    evaluate_ode/6,

    %% Backbone/head network functions
    compute_backbone/3,
    compute_head/2,

    %% Time constant computation
    compute_liquid_tau/4,

    %% State management
    clamp_state/2,
    reset_state/0,

    %% Utility
    sigmoid/1,
    tanh/1
]).

%%==============================================================================
%% Types
%%==============================================================================

-type state() :: float().
-type input() :: float() | [float()].
-type tau() :: float().          %% Base time constant
-type bound() :: float().        %% State bound A
-type dt() :: float().           %% Time step
-type backbone_weights() :: [float()].
-type head_weights() :: [float()].

%%==============================================================================
%% CfC Closed-Form Evaluation
%%==============================================================================

%% @doc Evaluate CfC neuron with closed-form approximation.
%%
%% This is the fast path for LTC evaluation, avoiding ODE solving.
%% Uses default backbone/head functions based on input only.
%%
%% @param Input the input signal (scalar or vector)
%% @param State the current internal state x(t)
%% @param Tau the base time constant
%% @param Bound the state bound A
%% @returns {NewState, Output} tuple
-spec evaluate_cfc(input(), state(), tau(), bound()) -> {state(), float()}.
evaluate_cfc(Input, State, Tau, Bound) ->
    %% Default backbone: simple nonlinearity on input
    F = compute_backbone_simple(Input, Tau),
    %% Default head: tanh of input
    H = compute_head_simple(Input),
    evaluate_cfc_internal(State, F, H, Bound).

%% @doc Evaluate CfC neuron with custom backbone/head weights.
%%
%% For networks with evolved backbone/head parameters.
%%
%% @param Input the input signal
%% @param State the current internal state
%% @param Tau the base time constant
%% @param Bound the state bound
%% @param Params map with backbone_weights and head_weights
%% @returns {NewState, Output} tuple
-spec evaluate_cfc(input(), state(), tau(), bound(), map()) -> {state(), float()}.
evaluate_cfc(Input, State, Tau, Bound, Params) ->
    BackboneWeights = maps:get(backbone_weights, Params, []),
    HeadWeights = maps:get(head_weights, Params, []),

    F = compute_backbone(Input, Tau, BackboneWeights),
    H = compute_head(Input, HeadWeights),
    evaluate_cfc_internal(State, F, H, Bound).

%% @private
%% @doc Internal CfC computation.
%%
%% Implements: x_new = sigma(-f) * x + (1 - sigma(-f)) * h
-spec evaluate_cfc_internal(state(), float(), float(), bound()) -> {state(), float()}.
evaluate_cfc_internal(State, F, H, Bound) ->
    %% Sigmoid gate on negative f
    SigNegF = sigmoid(-F),

    %% CfC update equation
    NewStateRaw = SigNegF * State + (1.0 - SigNegF) * H,

    %% Clamp to bounds for stability
    NewState = clamp_state(NewStateRaw, Bound),

    %% Output is the state (can be transformed by activation in neuron)
    Output = NewState,

    {NewState, Output}.

%%==============================================================================
%% ODE-Based Evaluation
%%==============================================================================

%% @doc Evaluate LTC neuron using ODE integration (Euler method).
%%
%% More accurate than CfC but slower. Use for training or when
%% temporal precision is critical.
%%
%% @param Input the input signal
%% @param State the current internal state
%% @param Tau the base time constant
%% @param Bound the state bound
%% @param Dt the time step for integration
%% @returns {NewState, Output} tuple
-spec evaluate_ode(input(), state(), tau(), bound(), dt()) -> {state(), float()}.
evaluate_ode(Input, State, Tau, Bound, Dt) ->
    evaluate_ode(Input, State, Tau, Bound, Dt, #{}).

%% @doc Evaluate LTC with ODE and custom parameters.
%%
%% @param Input the input signal
%% @param State the current internal state
%% @param Tau the base time constant
%% @param Bound the state bound
%% @param Dt the time step
%% @param Params map with liquid_weights
%% @returns {NewState, Output} tuple
-spec evaluate_ode(input(), state(), tau(), bound(), dt(), map()) -> {state(), float()}.
evaluate_ode(Input, State, Tau, Bound, Dt, Params) ->
    %% Compute liquid time constant
    LiquidTau = compute_liquid_tau(Input, State, Tau, Params),

    %% LTC ODE: dx/dt = -[1/tau + f] * x + f * A
    %% where f is approximated by LiquidTau adjustment

    %% Simplified: dx/dt = -(1/LiquidTau) * x + (1/LiquidTau) * target
    %% where target = A * tanh(input)
    Target = Bound * tanh(aggregate_input(Input)),

    %% Euler step: x_new = x + dt * dx/dt
    DxDt = (1.0 / LiquidTau) * (Target - State),
    NewStateRaw = State + Dt * DxDt,

    %% Clamp to bounds
    NewState = clamp_state(NewStateRaw, Bound),
    Output = NewState,

    {NewState, Output}.

%%==============================================================================
%% Backbone and Head Networks
%%==============================================================================

%% @doc Compute backbone network output (time-constant modulator).
%%
%% The backbone determines how the time constant varies with input.
%% This is the "f" in the CfC equation.
%%
%% @param Input the input signal
%% @param Tau base time constant
%% @param Weights backbone weights (empty list = simple mode)
%% @returns backbone output f
-spec compute_backbone(input(), tau(), backbone_weights()) -> float().
compute_backbone(Input, Tau, []) ->
    %% Simple mode: no learned weights
    compute_backbone_simple(Input, Tau);
compute_backbone(Input, Tau, Weights) ->
    %% Weighted combination with nonlinearity
    InputVal = aggregate_input(Input),
    %% Apply weights in layers
    Weighted = apply_weights(InputVal, Weights),
    %% Modulate by tau
    tanh(Weighted / Tau).

%% @private
%% @doc Simple backbone without learned weights.
-spec compute_backbone_simple(input(), tau()) -> float().
compute_backbone_simple(Input, Tau) ->
    InputVal = aggregate_input(Input),
    %% Simple nonlinearity: sigmoid of input scaled by tau
    sigmoid(InputVal / Tau).

%% @doc Compute head network output (target state).
%%
%% The head determines the asymptotic state the neuron moves toward.
%% This is the "h" in the CfC equation.
%%
%% @param Input the input signal
%% @param Weights head weights (empty list = simple mode)
%% @returns head output h
-spec compute_head(input(), head_weights()) -> float().
compute_head(Input, []) ->
    %% Simple mode: tanh of input
    compute_head_simple(Input);
compute_head(Input, Weights) ->
    InputVal = aggregate_input(Input),
    Weighted = apply_weights(InputVal, Weights),
    tanh(Weighted).

%% @private
%% @doc Simple head without learned weights.
-spec compute_head_simple(input()) -> float().
compute_head_simple(Input) ->
    InputVal = aggregate_input(Input),
    tanh(InputVal).

%%==============================================================================
%% Time Constant Computation
%%==============================================================================

%% @doc Compute the liquid (input-dependent) time constant.
%%
%% The "liquid" aspect of LTC comes from the time constant varying
%% based on input and state, allowing adaptive temporal dynamics.
%%
%% @param Input the input signal
%% @param State the current state
%% @param BaseTau the base time constant
%% @param Params optional parameters
%% @returns effective time constant (always > 0)
-spec compute_liquid_tau(input(), state(), tau(), map()) -> float().
compute_liquid_tau(Input, State, BaseTau, Params) ->
    LiquidWeights = maps:get(liquid_weights, Params, []),

    %% Base computation: tau varies with input magnitude
    InputVal = aggregate_input(Input),

    Modulation = case LiquidWeights of
        [] ->
            %% Simple mode: modulate by sigmoid of input
            1.0 + sigmoid(InputVal);
        Weights ->
            %% Learned modulation
            1.0 + sigmoid(apply_weights(InputVal + State, Weights))
    end,

    %% Ensure tau stays positive and reasonable
    EffectiveTau = BaseTau * Modulation,
    max(0.001, min(EffectiveTau, 100.0)).

%%==============================================================================
%% State Management
%%==============================================================================

%% @doc Clamp state to bounds [-Bound, Bound].
%%
%% Bounded dynamics are a key property of LTC that ensures stability.
%%
%% @param State the state value
%% @param Bound the bound magnitude
%% @returns clamped state
-spec clamp_state(state(), bound()) -> state().
clamp_state(State, Bound) when State > Bound -> Bound;
clamp_state(State, Bound) when State < -Bound -> -Bound;
clamp_state(State, _Bound) -> State.

%% @doc Reset state to zero.
%%
%% @returns initial state value
-spec reset_state() -> state().
reset_state() -> 0.0.

%%==============================================================================
%% Utility Functions
%%==============================================================================

%% @doc Sigmoid function.
%%
%% sigma(x) = 1 / (1 + e^-x)
%%
%% @param X input value
%% @returns sigmoid of X in range (0, 1)
-spec sigmoid(float()) -> float().
sigmoid(X) ->
    V = clamp(X, -10.0, 10.0),  %% Prevent overflow
    1.0 / (1.0 + math:exp(-V)).

%% @doc Hyperbolic tangent function.
%%
%% @param X input value
%% @returns tanh of X in range (-1, 1)
-spec tanh(float()) -> float().
tanh(X) ->
    math:tanh(X).

%%==============================================================================
%% Internal Helpers
%%==============================================================================

%% @private
%% @doc Aggregate input (handle scalar or vector).
-spec aggregate_input(input()) -> float().
aggregate_input(Input) when is_number(Input) ->
    float(Input);
aggregate_input([]) ->
    0.0;
aggregate_input(Input) when is_list(Input) ->
    %% Sum all inputs
    lists:sum(Input).

%% @private
%% @doc Apply weight vector to input value.
%% Simple single-layer transformation.
-spec apply_weights(float(), [float()]) -> float().
apply_weights(Input, []) ->
    Input;
apply_weights(Input, [W | Rest]) ->
    %% First weight is multiplicative, rest are polynomial terms
    Weighted = Input * W,
    apply_polynomial(Weighted, Input, Rest, 2).

%% @private
%% @doc Apply polynomial weights (x^2, x^3, etc.)
-spec apply_polynomial(float(), float(), [float()], pos_integer()) -> float().
apply_polynomial(Acc, _Input, [], _Power) ->
    Acc;
apply_polynomial(Acc, Input, [W | Rest], Power) ->
    Term = W * math:pow(Input, Power),
    apply_polynomial(Acc + Term, Input, Rest, Power + 1).

%% @private
%% @doc Clamp value to range.
-spec clamp(number(), number(), number()) -> number().
clamp(Val, Min, _Max) when Val < Min -> Min;
clamp(Val, _Min, Max) when Val > Max -> Max;
clamp(Val, _Min, _Max) -> Val.
