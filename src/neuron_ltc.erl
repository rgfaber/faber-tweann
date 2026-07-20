%% @doc Liquid Time-Constant (LTC) neural processing unit for TWEANN networks.
%%
%% This module implements LTC neurons that have input-dependent time constants,
%% enabling adaptive temporal processing for time-series data and real-time
%% control tasks.
%%
%% == LTC vs Standard Neurons ==
%%
%% Standard neurons: output = f(sum(w_i * x_i) + bias)
%% LTC neurons: maintain internal state x(t) that evolves according to:
%%   dx/dt = -[1/tau + f(x,I,theta)] * x + f(x,I,theta) * A
%%
%% CfC neurons use a fast closed-form approximation:
%%   x_new = sigma(-f) * x_old + (1 - sigma(-f)) * h
%%
%% == Neuron Lifecycle ==
%%
%% 1. Spawned by cortex with initial state (including LTC parameters)
%% 2. Waits for signals from input connections
%% 3. Aggregates all inputs when complete
%% 4. Evaluates LTC dynamics (CfC or ODE)
%% 5. Forwards output to all output connections
%% 6. Maintains internal_state between evaluations
%% 7. Repeats from step 2
%%
%% == State Persistence ==
%%
%% Unlike standard neurons which are stateless between evaluations,
%% LTC neurons maintain persistent internal_state that carries information
%% across timesteps. This enables temporal memory and adaptive dynamics.
%%
%% == References ==
%%
%% [1] Hasani, R., Lechner, M., et al. (2021). "Liquid Time-constant Networks."
%%     Proceedings of the AAAI Conference on Artificial Intelligence.
%%
%% [2] Hasani, R., Lechner, M., et al. (2022). "Closed-form Continuous-time
%%     Neural Networks." Nature Machine Intelligence.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(neuron_ltc).

-export([
    start_link/1,
    init/1,
    forward/3,
    backup/1,
    reset_state/1,
    get_state/1
]).

-record(state, {
    id :: term(),
    cortex_pid :: pid(),
    neuron_type :: ltc | cfc,                    %% LTC mode (ODE or CfC)
    time_constant :: float(),                     %% tau
    state_bound :: float(),                       %% A
    ltc_backbone_weights :: [float()],            %% f() backbone weights
    ltc_head_weights :: [float()],                %% h() head weights
    internal_state :: float(),                    %% x(t)
    dt :: float(),                                %% time step for ODE mode
    input_pids :: [pid()],
    output_pids :: [pid()],
    ro_pids :: [pid()],                           %% recurrent output PIDs
    input_weights :: #{pid() => [{float(), float(), float(), list()}]},
    bias :: float(),
    acc_input :: #{pid() => [float()]},
    expected_inputs :: non_neg_integer(),
    input_timeout :: pos_integer(),
    %% Memetic tuning snapshots (weights only; LTC params evolve at the
    %% generational level via ltc_mutations, not the per-agent tuner).
    saved_weights = undefined :: undefined | map(),
    saved_bias = undefined :: undefined | float()
}).

%% Default timeout for waiting on neuron inputs (10 seconds)
-define(DEFAULT_INPUT_TIMEOUT, 10000).

%% Default time step for ODE integration
-define(DEFAULT_DT, 0.1).

%% @doc Start an LTC neuron process.
%%
%% Options (in addition to standard neuron options):
%% - `neuron_type' - ltc (ODE) or cfc (closed-form), default: cfc
%% - `time_constant' - Base time constant tau, default: 1.0
%% - `state_bound' - State bound A, default: 1.0
%% - `ltc_backbone_weights' - Backbone network weights, default: []
%% - `ltc_head_weights' - Head network weights, default: []
%% - `internal_state' - Initial internal state, default: 0.0
%% - `dt' - Time step for ODE mode, default: 0.1
-spec start_link(map()) -> {ok, pid()}.
start_link(Opts) ->
    Pid = spawn_link(?MODULE, init, [Opts]),
    {ok, Pid}.

%% @doc Initialize the LTC neuron and enter the main loop.
-spec init(map()) -> no_return().
init(Opts) ->
    Id = maps:get(id, Opts),
    CortexPid = maps:get(cortex_pid, Opts),
    NeuronType = maps:get(neuron_type, Opts, cfc),
    TimeConstant = maps:get(time_constant, Opts, 1.0),
    StateBound = maps:get(state_bound, Opts, 1.0),
    BackboneWeights = maps:get(ltc_backbone_weights, Opts, []),
    HeadWeights = maps:get(ltc_head_weights, Opts, []),
    InternalState = maps:get(internal_state, Opts, 0.0),
    Dt = maps:get(dt, Opts, ?DEFAULT_DT),
    InputPids = maps:get(input_pids, Opts, []),
    OutputPids = maps:get(output_pids, Opts, []),
    RoPids = maps:get(ro_pids, Opts, []),
    InputWeights = maps:get(input_weights, Opts, #{}),
    Bias = maps:get(bias, Opts, 0.0),
    InputTimeout = maps:get(input_timeout, Opts, ?DEFAULT_INPUT_TIMEOUT),

    State = #state{
        id = Id,
        cortex_pid = CortexPid,
        neuron_type = NeuronType,
        time_constant = TimeConstant,
        state_bound = StateBound,
        ltc_backbone_weights = BackboneWeights,
        ltc_head_weights = HeadWeights,
        internal_state = InternalState,
        dt = Dt,
        input_pids = InputPids,
        output_pids = OutputPids,
        ro_pids = RoPids,
        input_weights = InputWeights,
        bias = Bias,
        acc_input = #{},
        expected_inputs = length(InputPids),
        input_timeout = InputTimeout
    },

    loop(State).

%% @doc Send a signal to an LTC neuron.
%%
%% Called by sensors or other neurons to forward their output.
-spec forward(pid(), pid(), [float()]) -> ok.
forward(NeuronPid, FromPid, Signal) ->
    NeuronPid ! {forward, FromPid, Signal},
    ok.

%% @doc Request the neuron to backup its current weights and state.
%%
%% The neuron will send its weights and LTC parameters to the cortex.
-spec backup(pid()) -> ok.
backup(NeuronPid) ->
    NeuronPid ! backup,
    ok.

%% @doc Reset the internal state of the LTC neuron to zero.
%%
%% Useful when starting a new episode or sequence.
-spec reset_state(pid()) -> ok.
reset_state(NeuronPid) ->
    NeuronPid ! reset_state,
    ok.

%% @doc Get the current internal state of the LTC neuron.
%%
%% Returns the internal state asynchronously via message.
-spec get_state(pid()) -> ok.
get_state(NeuronPid) ->
    NeuronPid ! {get_state, self()},
    ok.

%% Internal functions

loop(State) ->
    Timeout = State#state.input_timeout,
    receive
        {forward, FromPid, Signal} ->
            NewState = handle_forward(FromPid, Signal, State),
            loop(NewState);

        backup ->
            _ = handle_backup(State),
            loop(State#state{saved_weights = State#state.input_weights,
                             saved_bias = State#state.bias});

        %% Memetic tuning: jitter input weights and bias. The LTC-specific
        %% parameters (tau, bound, backbone/head weights) evolve at the
        %% generational level via ltc_mutations, not the per-agent tuner, so
        %% this mirrors the standard neuron and keeps the two arms comparable.
        {perturb, Range} ->
            PerturbedWeights = maps:map(
                fun(_Pid, WSpecs) ->
                    perturbation_utils:perturb_weights(WSpecs, Range)
                end,
                State#state.input_weights),
            PerturbedBias = perturbation_utils:sat(
                State#state.bias + (rand:uniform() - 0.5) * Range, 3.1415926),
            loop(State#state{input_weights = PerturbedWeights,
                             bias = PerturbedBias});

        %% Revert to the last backup (the perturbation was worse).
        restore ->
            case State#state.saved_weights of
                undefined ->
                    loop(State);
                Saved ->
                    loop(State#state{input_weights = Saved,
                                     bias = State#state.saved_bias})
            end;

        %% Reset temporal memory at the start of each evaluation episode, so the
        %% network's memory of one episode does not leak into the next.
        reset_state ->
            loop(State#state{internal_state = 0.0, acc_input = #{}});

        {get_state, FromPid} ->
            FromPid ! {ltc_state, State#state.id, State#state.internal_state},
            loop(State);

        {cortex, terminate} ->
            ok;

        {update_weights, NewWeights, NewBias} ->
            NewState = State#state{
                input_weights = NewWeights,
                bias = NewBias
            },
            loop(NewState);

        {update_ltc_params, Params} ->
            NewState = update_ltc_params(State, Params),
            loop(NewState);

        %% Dynamic linking from constructor
        {link, input_pids, InputPids} ->
            NewState = State#state{
                input_pids = InputPids,
                expected_inputs = length(InputPids)
            },
            loop(NewState);

        {link, output_pids, OutputPids} ->
            loop(State#state{output_pids = OutputPids});

        %% Seed recurrent targets with [0.0] so they do not deadlock on cycle 0
        %% (see neuron.erl). LTC neurons carry memory in internal_state, so they
        %% rarely need recurrent wiring, but this keeps the protocol uniform.
        {link, ro_pids, RoPids} ->
            _ = [RoPid ! {forward, self(), [0.0]} || RoPid <- RoPids],
            loop(State#state{ro_pids = RoPids});

        %% Bias is a constant input, extracted from input_idps by the exoself.
        {link, bias, BiasWeight} ->
            loop(State#state{bias = BiasWeight});

        {link, input_weights, InputWeights} ->
            loop(State#state{input_weights = InputWeights});

        %% Two-phase evaluation reset (see neuron.erl). Phase 1 flushes stale
        %% signals, clears the accumulator, acks. Phase 2 re-seeds recurrent
        %% targets AND zeroes the temporal memory, so each evaluation starts from
        %% a clean internal_state.
        {reset_prep, ExoSelfPid} ->
            flush_forwards(),
            ExoSelfPid ! {neuron_reset_ready, self()},
            receive
                reset ->
                    _ = [RoPid ! {forward, self(), [0.0]}
                         || RoPid <- State#state.ro_pids],
                    loop(State#state{acc_input = #{}, internal_state = 0.0});
                {cortex, terminate} ->
                    ok;
                terminate ->
                    ok
            end
    after Timeout ->
        handle_input_timeout(State)
    end.

%% @private Update LTC-specific parameters
update_ltc_params(State, Params) ->
    State#state{
        time_constant = maps:get(time_constant, Params, State#state.time_constant),
        state_bound = maps:get(state_bound, Params, State#state.state_bound),
        ltc_backbone_weights = maps:get(ltc_backbone_weights, Params,
                                        State#state.ltc_backbone_weights),
        ltc_head_weights = maps:get(ltc_head_weights, Params,
                                    State#state.ltc_head_weights),
        dt = maps:get(dt, Params, State#state.dt)
    }.

%% @private Drain stale forward signals before a fresh evaluation.
flush_forwards() ->
    receive
        {forward, _From, _Signal} -> flush_forwards()
    after 0 -> ok
    end.

%% @private Handle input timeout
handle_input_timeout(State) ->
    AccInput = State#state.acc_input,
    MissingInputs = length([Pid || Pid <- State#state.input_pids,
                                   maps:get(Pid, AccInput, []) =:= []]),
    tweann_logger:warning("LTC Neuron ~p input timeout after ~pms, missing ~p inputs",
                         [State#state.id, State#state.input_timeout, MissingInputs]),
    %% Continue waiting - neuron stays alive but logs the issue
    loop(State).

handle_forward(FromPid, Signal, State) ->
    #state{
        acc_input = AccInput,
        input_pids = InputPids
    } = State,

    %% FIFO-queue accumulation per source (see neuron.erl): the neuron fires
    %% exactly once per cortex cycle even when the cortex over-triggers a
    %% recurrent network, so a recurrent target never starves.
    Queue = maps:get(FromPid, AccInput, []),
    NewAccInput = maps:put(FromPid, Queue ++ [Signal], AccInput),

    case is_ready(InputPids, NewAccInput) of
        true ->
            process_and_forward(State#state{acc_input = NewAccInput});
        false ->
            State#state{acc_input = NewAccInput}
    end.

is_ready(InputPids, AccInput) ->
    lists:all(fun(Pid) -> maps:get(Pid, AccInput, []) =/= [] end, InputPids).

split_heads(InputPids, AccInput) ->
    lists:foldl(
        fun(Pid, {Heads, Rem}) ->
            case maps:get(Pid, AccInput, []) of
                [H | T] -> {Heads#{Pid => H}, Rem#{Pid => T}};
                [] -> {Heads, Rem}
            end
        end,
        {#{}, #{}},
        InputPids).

process_and_forward(State) ->
    #state{
        neuron_type = NeuronType,
        time_constant = TimeConstant,
        state_bound = StateBound,
        ltc_backbone_weights = BackboneWeights,
        ltc_head_weights = HeadWeights,
        internal_state = InternalState,
        dt = Dt,
        output_pids = OutputPids,
        ro_pids = RoPids,
        input_weights = InputWeights,
        bias = Bias,
        acc_input = AccInput,
        input_pids = InputPids
    } = State,

    %% Consume one signal per source for this cycle; buffer any extras.
    {CurrentSignals, RemainingAcc} = split_heads(InputPids, AccInput),

    %% Build input list and aggregate weighted signals
    Inputs = build_inputs(InputPids, CurrentSignals),
    Weights = build_weights(InputPids, InputWeights),

    %% Compute weighted input sum (standard aggregation)
    Aggregated = aggregate_weighted_inputs(Inputs, Weights) + Bias,

    %% Apply LTC dynamics
    {NewInternalState, Output} = evaluate_ltc(
        NeuronType,
        Aggregated,
        InternalState,
        TimeConstant,
        StateBound,
        BackboneWeights,
        HeadWeights,
        Dt
    ),

    %% Forward to all output connections
    lists:foreach(
        fun(OutputPid) ->
            OutputPid ! {forward, self(), [Output]}
        end,
        OutputPids
    ),

    %% Forward to recurrent outputs
    lists:foreach(
        fun(RoPid) ->
            RoPid ! {forward, self(), [Output]}
        end,
        RoPids
    ),

    %% Reset accumulated inputs but preserve internal state
    State#state{
        acc_input = RemainingAcc,
        internal_state = NewInternalState
    }.

%% @private Evaluate LTC dynamics based on neuron type
evaluate_ltc(cfc, Input, InternalState, Tau, Bound, BackboneW, HeadW, _Dt) ->
    Params = #{
        backbone_weights => BackboneW,
        head_weights => HeadW
    },
    ltc_dynamics:evaluate_cfc(Input, InternalState, Tau, Bound, Params);

evaluate_ltc(ltc, Input, InternalState, Tau, Bound, _BackboneW, _HeadW, Dt) ->
    %% ODE mode - uses liquid_weights from backbone if present
    ltc_dynamics:evaluate_ode(Input, InternalState, Tau, Bound, Dt).

build_inputs(InputPids, AccInput) ->
    [{Pid, maps:get(Pid, AccInput, [0.0])} || Pid <- InputPids].

build_weights(InputPids, InputWeights) ->
    [{Pid, maps:get(Pid, InputWeights, [{1.0, 0.0, 0.1, []}])} || Pid <- InputPids].

%% @private Aggregate weighted inputs using dot product
aggregate_weighted_inputs(Inputs, Weights) ->
    signal_aggregator:dot_product(Inputs, Weights).

handle_backup(State) ->
    #state{
        id = Id,
        cortex_pid = CortexPid,
        input_weights = InputWeights,
        bias = Bias,
        time_constant = TimeConstant,
        state_bound = StateBound,
        ltc_backbone_weights = BackboneWeights,
        ltc_head_weights = HeadWeights,
        internal_state = InternalState
    } = State,

    %% Include LTC parameters in backup
    LtcParams = #{
        time_constant => TimeConstant,
        state_bound => StateBound,
        ltc_backbone_weights => BackboneWeights,
        ltc_head_weights => HeadWeights,
        internal_state => InternalState
    },

    CortexPid ! {backup, Id, InputWeights, Bias, LtcParams}.
