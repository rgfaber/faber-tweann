%% @doc Neuron Introspection API.
%%
%% This module provides introspection capabilities for neurons in faber-tweann.
%% It can extract information from both neuron records and running neuron processes.
%%
%% == Usage ==
%%
%% From a neuron record:
%%
%%   Info = neuron_info:get_neuron_info(NeuronRecord),
%%   #{neuron_type := Type, time_constant := Tau} = Info.
%%
%% From a running neuron process:
%%
%%   Info = neuron_info:get_neuron_info(NeuronPid),
%%   #{neuron_type := cfc, internal_state := State} = Info.
%%
%% == Returned Information ==
%%
%% The returned map contains:
%%
%%   neuron_type - standard | ltc | cfc
%%   time_constant - tau value (for LTC/CfC neurons)
%%   state_bound - A value (for LTC/CfC neurons)
%%   internal_state - current x(t) state (for LTC/CfC neurons)
%%   activation_function - the activation function atom
%%   plasticity_function - the plasticity function (if any)
%%   input_count - number of input connections
%%   output_count - number of output connections
%%   capabilities - list of neuron capabilities
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever
-module(neuron_info).

-include("records.hrl").

-export([
    get_neuron_info/1,
    get_neuron_type/1,
    get_capabilities/1,
    is_temporal/1,
    describe/1
]).

-type neuron_info() :: #{
    neuron_type := standard | ltc | cfc,
    time_constant := float(),
    state_bound := float(),
    internal_state := float(),
    activation_function := atom(),
    plasticity_function := atom() | undefined,
    input_count := non_neg_integer(),
    output_count := non_neg_integer(),
    capabilities := [atom()]
}.

-export_type([neuron_info/0]).

%% @doc Get comprehensive information about a neuron.
%%
%% Accepts either a neuron record or a running neuron process pid.
%% Returns a map with all available neuron information.
-spec get_neuron_info(Neuron) -> neuron_info() when
    Neuron :: #neuron{} | pid().
get_neuron_info(#neuron{} = N) ->
    #{
        neuron_type => N#neuron.neuron_type,
        time_constant => N#neuron.time_constant,
        state_bound => N#neuron.state_bound,
        internal_state => N#neuron.internal_state,
        activation_function => N#neuron.af,
        plasticity_function => N#neuron.pf,
        input_count => length(N#neuron.input_idps),
        output_count => length(N#neuron.output_ids),
        capabilities => get_capabilities(N#neuron.neuron_type)
    };

get_neuron_info(Pid) when is_pid(Pid) ->
    %% Query the running neuron process for its state
    case catch gen_server:call(Pid, get_state, 5000) of
        {ok, State} when is_map(State) ->
            maps:merge(
                #{capabilities => get_capabilities(maps:get(neuron_type, State, standard))},
                State
            );
        #neuron{} = N ->
            get_neuron_info(N);
        {'EXIT', _} ->
            #{
                neuron_type => unknown,
                time_constant => 0.0,
                state_bound => 0.0,
                internal_state => 0.0,
                activation_function => undefined,
                plasticity_function => undefined,
                input_count => 0,
                output_count => 0,
                capabilities => [],
                error => process_not_responding
            }
    end.

%% @doc Get just the neuron type.
-spec get_neuron_type(Neuron) -> standard | ltc | cfc | unknown when
    Neuron :: #neuron{} | pid() | neuron_info().
get_neuron_type(#neuron{neuron_type = Type}) ->
    Type;
get_neuron_type(#{neuron_type := Type}) ->
    Type;
get_neuron_type(Pid) when is_pid(Pid) ->
    Info = get_neuron_info(Pid),
    maps:get(neuron_type, Info, unknown).

%% @doc Get capabilities for a neuron type.
%%
%% Returns a list of atoms describing what the neuron type can do:
%%
%%   temporal_memory - Can remember past inputs
%%   adaptive_dynamics - Time constant varies with input
%%   fast_inference - Optimized for production speed
%%   ode_accurate - Uses accurate ODE integration
%%   hebbian_plasticity - Supports Hebbian learning
%%   modulated_plasticity - Supports neuromodulation
-spec get_capabilities(NeuronType) -> [atom()] when
    NeuronType :: standard | ltc | cfc | atom().
get_capabilities(standard) ->
    [fast_inference, hebbian_plasticity, modulated_plasticity];
get_capabilities(ltc) ->
    [temporal_memory, adaptive_dynamics, ode_accurate, hebbian_plasticity];
get_capabilities(cfc) ->
    [temporal_memory, adaptive_dynamics, fast_inference, hebbian_plasticity];
get_capabilities(_) ->
    [].

%% @doc Check if a neuron type has temporal memory.
-spec is_temporal(Neuron) -> boolean() when
    Neuron :: #neuron{} | standard | ltc | cfc | neuron_info().
is_temporal(#neuron{neuron_type = Type}) ->
    is_temporal(Type);
is_temporal(#{neuron_type := Type}) ->
    is_temporal(Type);
is_temporal(ltc) -> true;
is_temporal(cfc) -> true;
is_temporal(_) -> false.

%% @doc Get a human-readable description of a neuron type.
-spec describe(NeuronType) -> binary() when
    NeuronType :: standard | ltc | cfc | atom().
describe(standard) ->
    <<"Standard neuron: instant response, no temporal memory. "
      "Best for pattern recognition and non-temporal tasks.">>;
describe(ltc) ->
    <<"LTC (Liquid Time-Constant) neuron: ODE-based temporal dynamics. "
      "Accurate but slower. Use for training and research.">>;
describe(cfc) ->
    <<"CfC (Closed-form Continuous-time) neuron: fast temporal dynamics. "
      "~100x faster than LTC ODE with equivalent expressivity. "
      "Use for production inference.">>;
describe(_) ->
    <<"Unknown neuron type.">>.
