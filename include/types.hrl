%% @doc Core type specifications for Faber TWEANN
%%
%% This file defines all custom types used throughout the TWEANN system.
%% Types are organized by category: basic types, identifiers, weights,
%% network components, and evolutionary operators.
%%
%% == Type Naming Conventions ==
%% - All types use descriptive, full names (no abbreviations)
%% - Compound types use underscores for readability
%% - Function types end with _function suffix
%%
%% == Weight Tuple Format ==
%% The weight_spec() type documents the critical 4-tuple format used
%% throughout the system for synaptic weights with plasticity support:
%%   {Weight, DeltaWeight, LearningRate, ParameterList}
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0

-ifndef(FABER_TWEANN_TYPES_HRL).
-define(FABER_TWEANN_TYPES_HRL, true).

%%==============================================================================
%% Basic Numeric Types
%%==============================================================================

%% @doc Standard weight value for synaptic connections
-type weight() :: float().

%% @doc Delta weight - momentum term for weight updates
%% Used in gradient-based learning and plasticity rules
-type delta_weight() :: float().

%% @doc Learning rate parameter for plasticity rules
-type learning_rate() :: float().

%% @doc Additional parameters for plasticity rules
%% Different rules require different parameter sets
-type parameter_list() :: [float()].

%% @doc Signal value from sensors or neurons
-type signal() :: float().

%% @doc Vector of signal values
-type signal_vector() :: [signal()].

%% @doc Fitness score for evolutionary evaluation
-type fitness() :: float() | undefined.

%% @doc Generation counter for evolutionary tracking
-type generation() :: non_neg_integer().

%%==============================================================================
%% Weight Specifications
%%==============================================================================

%% @doc Complete weight specification with plasticity support
%%
%% This is the core data structure for synaptic weights in the system.
%% Each weight carries:
%%   - Current weight value
%%   - Momentum term (delta_weight) for smoother updates
%%   - Learning rate for plasticity rule
%%   - Parameter list specific to the plasticity rule
%%
%% Example:
%%   {0.5, 0.0, 0.1, [0.1, 0.2]} means:
%%   - Weight = 0.5
%%   - DeltaWeight = 0.0 (no momentum yet)
%%   - LearningRate = 0.1
%%   - Parameters = [0.1, 0.2] for the plasticity rule
-type weight_spec() :: {weight(), delta_weight(), learning_rate(), parameter_list()}.

%% @doc List of weight specifications for a connection
-type weight_list() :: [weight_spec()].

%%==============================================================================
%% Entity Identifiers
%%==============================================================================

%% @doc Unique identifier format: {LayerCoordinate, UniqueId}
%% LayerCoordinate indicates the topological position (-1 for sensors,
%% 0-1 for neurons, 1 for actuators)
%% UniqueId is a random float for uniqueness
-type unique_id() :: {float(), float()}.

%% @doc Neuron identifier with type tag
%% Format: {{LayerCoord, UniqueId}, neuron}
-type neuron_id() :: {unique_id(), neuron}.

%% @doc Sensor identifier with type tag
%% Format: {{-1.0, UniqueId}, sensor}
-type sensor_id() :: {unique_id(), sensor}.

%% @doc Actuator identifier with type tag
%% Format: {{1.0, UniqueId}, actuator}
-type actuator_id() :: {unique_id(), actuator}.

%% @doc Cortex identifier with type tag
%% Format: {{0.0, UniqueId}, cortex}
-type cortex_id() :: {unique_id(), cortex}.

%% @doc Agent identifier with type tag
-type agent_id() :: {float(), agent}.

%% @doc Specie identifier
-type specie_id() :: atom() | {float(), specie}.

%% @doc Population identifier
-type population_id() :: atom() | {float(), population}.

%% @doc Any network element identifier
-type element_id() :: neuron_id() | sensor_id() | actuator_id() | cortex_id().

%%==============================================================================
%% Input/Output Specifications
%%==============================================================================

%% @doc Weighted input specification
%% Associates a source ID with its weight list
%% Old name: idps (input_idps)
-type weighted_input() :: {element_id(), weight_list()}.

%% @doc List of weighted inputs for a neuron
%% Old name: input_idps
-type weighted_inputs() :: [weighted_input()].

%% @doc Output target specification
-type output_id() :: neuron_id() | actuator_id().

%% @doc Recurrent output specification (feedback connections)
-type recurrent_output_id() :: neuron_id().

%%==============================================================================
%% Activation Functions
%%==============================================================================

%% @doc Available activation functions
%% These transform the aggregated input signal into output
-type activation_function() ::
    tanh |
    cos |
    sin |
    gaussian |
    absolute |
    sqrt |
    sigmoid |
    relu |
    linear.

%%==============================================================================
%% Plasticity Functions
%%==============================================================================

%% @doc Available plasticity rules for learning
%% These modify weights based on activity patterns
-type plasticity_function() ::
    none |
    hebbian |
    hebbian_w |
    ojas |
    ojas_w |
    self_modulation_v1 |
    self_modulation_v2 |
    self_modulation_v3 |
    self_modulation_v4 |
    self_modulation_v5 |
    self_modulation_v6 |
    neuromodulation.

%% @doc Plasticity function with parameters
-type plasticity_spec() :: {plasticity_function(), parameter_list()}.

%%==============================================================================
%% Aggregation Functions
%%==============================================================================

%% @doc Signal aggregation functions
%% These combine multiple input signals into a single value
-type aggregation_function() ::
    dot_product |
    mult_product |
    diff.

%%==============================================================================
%% Network Topology
%%==============================================================================

%% @doc Network connection architecture
-type connection_architecture() :: recurrent | feedforward.

%% @doc Layer pattern entry
%% Associates a layer coordinate with the number of neurons
-type layer_pattern() :: {float(), [neuron_id()]}.

%% @doc Complete network pattern
-type network_pattern() :: [layer_pattern()].

%% @doc Agent encoding type
-type encoding_type() :: neural | substrate.

%% @doc Heredity type for evolution
-type heredity_type() :: darwinian | lamarckian.

%%==============================================================================
%% Evolutionary Operators
%%==============================================================================

%% @doc Mutation operator with probability weight
-type mutation_operator() :: {atom(), float()}.

%% @doc List of available mutation operators
-type mutation_operators() :: [mutation_operator()].

%% @doc Tuning selection function
-type tuning_selection_function() ::
    all |
    all_random |
    recent |
    recent_random |
    lastgen |
    lastgen_random |
    dynamic_random.

%% @doc Tuning duration specification
-type tuning_duration_spec() :: {atom(), float()}.

%% @doc Topological mutations specification
-type topological_mutations_spec() :: {atom(), float()}.

%%==============================================================================
%% Population Management
%%==============================================================================

%% @doc Evolution algorithm type
-type evolution_algorithm() :: generational | steady_state.

%% @doc Fitness postprocessor type
-type fitness_postprocessor() :: none | size_proportional.

%% @doc Selection function type
-type selection_function() :: competition | top3 | hof_competition.

%%==============================================================================
%% LTC (Liquid Time-Constant) Types
%%==============================================================================

%% @doc Neuron dynamics type
%%
%% Determines how the neuron processes signals:
%%   - standard: Traditional activation-based processing (instantaneous)
%%   - ltc: Liquid Time-Constant with ODE integration (accurate, slower)
%%   - cfc: Closed-form Continuous-time approximation (fast, ~100x faster than ODE)
%%
%% LTC neurons have input-dependent time constants, enabling adaptive temporal
%% processing for time-series data and real-time control tasks.
%%
%% References:
%%   [1] Hasani, R., Lechner, M., et al. (2021). "Liquid Time-constant Networks."
%%   [2] Hasani, R., Lechner, M., et al. (2022). "Closed-form Continuous-time
%%       Neural Networks." Nature Machine Intelligence.
-type neuron_type() :: standard | ltc | cfc.

%% @doc Time constant (tau) for LTC neurons
%% Controls how quickly the neuron responds to input changes.
%% Must be positive. Typical range: 0.1 to 10.0
%% Lower values = faster response, higher values = smoother integration.
-type time_constant() :: float().

%% @doc State bound (A) for LTC neurons
%% The internal state x(t) is clamped to [-A, A] for numerical stability.
%% Prevents state explosion during long-running simulations.
%% Must be positive. Typical range: 0.5 to 2.0
-type state_bound() :: float().

%% @doc Internal state for LTC neurons
%% Represents x(t) in the LTC ODE. Persistent across evaluations.
%% Range: [-state_bound, state_bound]
-type internal_state() :: float().

%% @doc Time step for ODE integration (dt)
%% Only used with neuron_type = ltc (ODE mode).
%% Smaller values = more accurate, larger = faster.
%% Typical range: 0.01 to 0.1
-type time_step() :: float().

%% @doc LTC backbone weights
%% Weights for the backbone network f() that modulates the time constant.
%% Can be empty for simple mode (input-based modulation without learning).
-type ltc_backbone_weights() :: [float()].

%% @doc LTC head weights
%% Weights for the head network h() that computes the target state.
%% Can be empty for simple mode (tanh of input as target).
-type ltc_head_weights() :: [float()].

%% @doc LTC parameters map for evaluate functions
%% Optional configuration passed to ltc_dynamics:evaluate_* functions
-type ltc_params() :: #{
    backbone_weights => ltc_backbone_weights(),
    head_weights => ltc_head_weights(),
    liquid_weights => [float()]  %% For ODE-based liquid tau computation
}.

%%==============================================================================
%% Substrate Types (for HyperNEAT)
%%==============================================================================

%% @doc Substrate link form
-type substrate_linkform() ::
    l2l_feedforward |
    jordan_recurrent |
    fully_connected.

%% @doc Substrate plasticity type
-type substrate_plasticity() :: none | hebbian | ojas.

%%==============================================================================
%% Scape Types (Environment)
%%==============================================================================

%% @doc Scape identifier
-type scape_id() :: atom() | {float(), scape}.

%% @doc Scape visibility
-type scape_visibility() :: private | public.

%% @doc Scape specification
-type scape_spec() :: {scape_visibility(), atom()}.

%%==============================================================================
%% Format Types
%%==============================================================================

%% @doc Sensor/Actuator format specification
-type format_spec() :: {no_geo | geo, [non_neg_integer()]}.

%% @doc Vector length
-type vector_length() :: non_neg_integer().

-endif. %% FABER_TWEANN_TYPES_HRL
