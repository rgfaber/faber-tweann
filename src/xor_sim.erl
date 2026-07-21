%% @doc XOR scape. The module `examples/xor/src/morphology_xor.erl' has
%% declared `scape = {private, xor_sim}' since the repository was created,
%% and until now no such module existed.
%%
%% Presents the four XOR cases once each per evaluation, in fixed order, and
%% accumulates squared error.
%%
%% Encoding is Sher's (Handbook Ch 7): -1.0/1.0 rather than 0.0/1.0, matching
%% the tanh output range of the default neuron.
%%
%%   [-1, -1] -> -1     [ 1, -1] ->  1
%%   [-1,  1] ->  1     [ 1,  1] -> -1
%%
%% == Fitness ==
%%
%% Lifetime-based, as in DXNN2: 0.0 on the first three cases, the real value
%% once at the end. Fitness is 1/(RMSE + epsilon), so a perfect network scores
%% very large but finite.
%%
%% == goal_reached ==
%%
%% Returned instead of a plain halt when all four cases land within tolerance
%% of their target. This is what lets the population monitor freeze
%% total_evaluations at the moment of solution; without it,
%% evaluations-to-solve is unmeasurable and no comparison against published
%% figures is possible.
%%
%% The tolerance-based test is deliberate rather than a fitness threshold:
%% fitness here is continuous and unbounded, so any threshold would be
%% arbitrary and would drift with the epsilon.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(xor_sim).
-behaviour(scape).

-export([init/1, sense/3, act/4]).

%% Exposed so tests and the neuroevolution-side xor_environment share one
%% definition of the problem instead of each restating it.
-export([cases/0, tolerance/0]).

-define(CASES, [
    {[-1.0, -1.0], -1.0},
    {[ 1.0, -1.0],  1.0},
    {[-1.0,  1.0],  1.0},
    {[ 1.0,  1.0], -1.0}
]).

-define(TOLERANCE, 0.5).
-define(EPSILON, 1.0e-5).

-record(state, {
    remaining = ?CASES :: [{[float()], float()}],
    current            :: {[float()], float()} | undefined,
    sse       = 0.0    :: float(),
    correct   = 0      :: non_neg_integer()
}).

%% @doc The four XOR cases as {Inputs, Target}.
cases() -> ?CASES.

%% @doc Distance from target within which an output counts as correct.
tolerance() -> ?TOLERANCE.

%% @doc Fresh state: all four cases pending.
init(_Params) ->
    #state{}.

%% @doc Serve the current case's inputs, advancing to it on first request.
%%
%% The scape advances on sense rather than on act because the sensor always
%% fires before the actuator within one sense-think-act cycle.
sense(_SensorName, _Params, #state{current = undefined, remaining = [Case | Rest]} = S) ->
    {Inputs, _Target} = Case,
    {Inputs, S#state{current = Case, remaining = Rest}};
sense(_SensorName, _Params, #state{current = {Inputs, _Target}} = S) ->
    {Inputs, S}.

%% @doc Score the output against the current case.
act(_ActuatorName, _Params, [Output], #state{current = {_Inputs, Target}} = S) ->
    Error = Target - Output,
    Sse = S#state.sse + Error * Error,
    Correct = case abs(Error) < ?TOLERANCE of
                  true  -> S#state.correct + 1;
                  false -> S#state.correct
              end,
    case S#state.remaining of
        [] ->
            %% Last case: report fitness and halt.
            Rmse = math:sqrt(Sse / length(?CASES)),
            Fitness = 1.0 / (Rmse + ?EPSILON),
            HaltFlag = halt_flag(Correct),
            {Fitness, HaltFlag, #state{}};
        _ ->
            %% More cases pending: no fitness yet, clear current so the next
            %% sense advances.
            {0.0, 0, S#state{sse = Sse, correct = Correct, current = undefined}}
    end;
act(_ActuatorName, _Params, Output, S) ->
    %% Wrong output arity is a genotype/morphology mismatch, not a runtime
    %% condition to absorb. Fail loudly.
    erlang:error({xor_sim_expected_one_output, Output, S}).

%% @private goal_reached once every case is correct, otherwise keep evaluating.
halt_flag(Correct) ->
    case Correct =:= length(?CASES) of
        true  -> goal_reached;
        false -> 1
    end.
