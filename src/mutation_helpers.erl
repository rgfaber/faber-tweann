%% @doc Helper functions for genome mutation operations.
%%
%% This module provides utility functions used by mutation operators
%% for linking network elements and weight management.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0
-module(mutation_helpers).

-include("records.hrl").

-dialyzer({nowarn_function, [
    update_source_output/3,
    update_target_input/4,
    link_neuron_to_target/3,
    link_source_to_neuron/3
]}).

-export([
    %% Neuron selection
    select_random_neuron/1,
    select_ltc_neuron/1,

    %% Link operations
    link_neuron_to_target/3,
    link_source_to_neuron/3,
    link_sensor_to_neuron/3,
    link_neuron_to_actuator/3,
    update_source_output/3,
    update_target_input/4,

    %% Weight utilities
    create_random_weight/0,
    get_link_weight/2,
    find_splittable_link/1,
    get_layer_coord/1,

    %% LTC utilities
    perturb_ltc_weight_list/2
]).

%% Delta multiplier for weight perturbation.
-define(DELTA_MULTIPLIER, math:pi() * 2).

%%==============================================================================
%% Neuron Selection
%%==============================================================================

%% @doc Select a random neuron from the agent's network.
%%
%% @param AgentId the agent
%% @returns NeuronId or {error, no_neurons}
-spec select_random_neuron(term()) -> term() | {error, no_neurons}.
select_random_neuron(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    select_from_neuron_ids(Cortex#cortex.neuron_ids).

select_from_neuron_ids([]) -> {error, no_neurons};
select_from_neuron_ids(NeuronIds) -> selection_utils:random_select(NeuronIds).

%% @doc Select a random LTC or CfC neuron from the agent's network.
-spec select_ltc_neuron(term()) -> term() | {error, no_ltc_neurons | no_neurons}.
select_ltc_neuron(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    filter_and_select_ltc(Cortex#cortex.neuron_ids).

filter_and_select_ltc([]) ->
    {error, no_neurons};
filter_and_select_ltc(NeuronIds) ->
    LtcNeurons = [NId || NId <- NeuronIds, is_ltc_neuron(NId)],
    select_from_ltc_list(LtcNeurons).

select_from_ltc_list([]) -> {error, no_ltc_neurons};
select_from_ltc_list(LtcNeurons) -> selection_utils:random_select(LtcNeurons).

is_ltc_neuron(NeuronId) ->
    N = genotype:dirty_read({neuron, NeuronId}),
    N#neuron.neuron_type =:= ltc orelse N#neuron.neuron_type =:= cfc.

%%==============================================================================
%% Link Operations
%%==============================================================================

%% @doc Link a neuron to a target (neuron or actuator).
-spec link_neuron_to_target(term(), #neuron{}, term()) -> ok.
link_neuron_to_target(NeuronId, Neuron, TargetId) ->
    %% Update source neuron's outputs
    NewOutputIds = [TargetId | Neuron#neuron.output_ids],
    UpdatedNeuron = Neuron#neuron{output_ids = NewOutputIds},
    genotype:write(UpdatedNeuron),
    %% Update target's inputs
    update_target_with_new_input(NeuronId, TargetId),
    ok.

update_target_with_new_input(NeuronId, TargetId) ->
    case genotype:dirty_read({neuron, TargetId}) of
        undefined ->
            add_neuron_to_actuator_fanin(NeuronId, TargetId);
        TargetNeuron ->
            add_input_to_neuron(NeuronId, TargetNeuron)
    end.

add_neuron_to_actuator_fanin(NeuronId, ActuatorId) ->
    Actuator = genotype:dirty_read({actuator, ActuatorId}),
    NewFaninIds = [NeuronId | Actuator#actuator.fanin_ids],
    UpdatedActuator = Actuator#actuator{fanin_ids = NewFaninIds},
    genotype:write(UpdatedActuator).

add_input_to_neuron(SourceId, TargetNeuron) ->
    NewWeight = create_random_weight(),
    NewInputIdps = [{SourceId, [NewWeight]} | TargetNeuron#neuron.input_idps],
    UpdatedTarget = TargetNeuron#neuron{input_idps = NewInputIdps},
    genotype:write(UpdatedTarget).

%% @doc Link a source (sensor or neuron) to a neuron.
-spec link_source_to_neuron(term(), term(), #neuron{}) -> ok.
link_source_to_neuron(SourceId, NeuronId, Neuron) ->
    %% Update neuron's inputs
    NewWeight = create_random_weight(),
    NewInputIdps = [{SourceId, [NewWeight]} | Neuron#neuron.input_idps],
    UpdatedNeuron = Neuron#neuron{input_idps = NewInputIdps},
    genotype:write(UpdatedNeuron),
    %% Update source's outputs
    update_source_with_new_output(SourceId, NeuronId),
    ok.

update_source_with_new_output(SourceId, NeuronId) ->
    case genotype:dirty_read({neuron, SourceId}) of
        undefined ->
            add_neuron_to_sensor_fanout(SourceId, NeuronId);
        SourceNeuron ->
            add_output_to_neuron(SourceNeuron, NeuronId)
    end.

add_neuron_to_sensor_fanout(SensorId, NeuronId) ->
    Sensor = genotype:dirty_read({sensor, SensorId}),
    NewFanoutIds = [NeuronId | Sensor#sensor.fanout_ids],
    UpdatedSensor = Sensor#sensor{fanout_ids = NewFanoutIds},
    genotype:write(UpdatedSensor).

add_output_to_neuron(SourceNeuron, NeuronId) ->
    NewOutputIds = [NeuronId | SourceNeuron#neuron.output_ids],
    UpdatedSource = SourceNeuron#neuron{output_ids = NewOutputIds},
    genotype:write(UpdatedSource).

%% @doc Link a sensor to a neuron.
-spec link_sensor_to_neuron(term(), #sensor{}, term()) -> ok.
link_sensor_to_neuron(SensorId, Sensor, NeuronId) ->
    %% Update sensor's fanout
    NewFanoutIds = [NeuronId | Sensor#sensor.fanout_ids],
    UpdatedSensor = Sensor#sensor{fanout_ids = NewFanoutIds},
    genotype:write(UpdatedSensor),
    %% Update neuron's inputs
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    NewWeight = create_random_weight(),
    NewInputIdps = [{SensorId, [NewWeight]} | Neuron#neuron.input_idps],
    UpdatedNeuron = Neuron#neuron{input_idps = NewInputIdps},
    genotype:write(UpdatedNeuron),
    ok.

%% @doc Link a neuron to an actuator.
-spec link_neuron_to_actuator(term(), term(), #actuator{}) -> ok.
link_neuron_to_actuator(NeuronId, ActuatorId, Actuator) ->
    %% Update actuator's fanin
    NewFaninIds = [NeuronId | Actuator#actuator.fanin_ids],
    UpdatedActuator = Actuator#actuator{fanin_ids = NewFaninIds},
    genotype:write(UpdatedActuator),
    %% Update neuron's outputs
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    NewOutputIds = [ActuatorId | Neuron#neuron.output_ids],
    UpdatedNeuron = Neuron#neuron{output_ids = NewOutputIds},
    genotype:write(UpdatedNeuron),
    ok.

%% @doc Update source element to output to new target.
-spec update_source_output(term(), term(), term()) -> ok.
update_source_output(FromId, OldToId, NewToId) ->
    case genotype:dirty_read({neuron, FromId}) of
        undefined ->
            update_sensor_output(FromId, OldToId, NewToId);
        Neuron ->
            update_neuron_output(Neuron, OldToId, NewToId)
    end,
    ok.

update_sensor_output(SensorId, OldToId, NewToId) ->
    Sensor = genotype:dirty_read({sensor, SensorId}),
    NewFanoutIds = [NewToId | (Sensor#sensor.fanout_ids -- [OldToId])],
    UpdatedSensor = Sensor#sensor{fanout_ids = NewFanoutIds},
    genotype:write(UpdatedSensor).

update_neuron_output(Neuron, OldToId, NewToId) ->
    NewOutputIds = [NewToId | (Neuron#neuron.output_ids -- [OldToId])],
    UpdatedNeuron = Neuron#neuron{output_ids = NewOutputIds},
    genotype:write(UpdatedNeuron).

%% @doc Update target element to receive from new source.
-spec update_target_input(term(), term(), term(), {float(), float(), float(), list()}) -> ok.
update_target_input(ToId, OldFromId, NewFromId, Weight) ->
    case genotype:dirty_read({neuron, ToId}) of
        undefined ->
            update_actuator_input(ToId, OldFromId, NewFromId);
        Neuron ->
            update_neuron_input(Neuron, OldFromId, NewFromId, Weight)
    end,
    ok.

update_actuator_input(ActuatorId, OldFromId, NewFromId) ->
    Actuator = genotype:dirty_read({actuator, ActuatorId}),
    NewFaninIds = [NewFromId | (Actuator#actuator.fanin_ids -- [OldFromId])],
    UpdatedActuator = Actuator#actuator{fanin_ids = NewFaninIds},
    genotype:write(UpdatedActuator).

update_neuron_input(Neuron, OldFromId, NewFromId, Weight) ->
    FilteredInputs = [{Id, W} || {Id, W} <- Neuron#neuron.input_idps, Id /= OldFromId],
    NewInputIdps = [{NewFromId, [Weight]} | FilteredInputs],
    UpdatedNeuron = Neuron#neuron{input_idps = NewInputIdps},
    genotype:write(UpdatedNeuron).

%%==============================================================================
%% Weight Utilities
%%==============================================================================

%% @doc Create a random weight tuple.
-spec create_random_weight() -> {float(), float(), float(), []}.
create_random_weight() ->
    {rand:uniform() - 0.5, 0.0, 0.1, []}.

%% @doc Get weight of a link between two elements.
-spec get_link_weight(term(), term()) -> {float(), float(), float(), list()}.
get_link_weight(FromId, ToId) ->
    case genotype:dirty_read({neuron, ToId}) of
        undefined ->
            create_random_weight();
        Neuron ->
            find_weight_in_inputs(FromId, Neuron#neuron.input_idps)
    end.

find_weight_in_inputs(FromId, InputIdps) ->
    case lists:keyfind(FromId, 1, InputIdps) of
        {FromId, [Weight | _]} -> Weight;
        _ -> create_random_weight()
    end.

%% @doc Find a link that can be split to insert a neuron.
-spec find_splittable_link(term()) -> {term(), term(), {float(), float(), float(), list()}} | {error, no_links}.
find_splittable_link(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cortex = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    Links = collect_all_links(Cortex#cortex.neuron_ids),
    select_random_link(Links).

collect_all_links(NeuronIds) ->
    lists:flatmap(fun collect_neuron_links/1, NeuronIds).

collect_neuron_links(NeuronId) ->
    Neuron = genotype:dirty_read({neuron, NeuronId}),
    [{NeuronId, OutputId} || OutputId <- Neuron#neuron.output_ids].

select_random_link([]) ->
    {error, no_links};
select_random_link(Links) ->
    {FromId, ToId} = selection_utils:random_select(Links),
    Weight = get_link_weight(FromId, ToId),
    {FromId, ToId, Weight}.

%% @doc Get layer coordinate from element ID.
-spec get_layer_coord(term()) -> float().
get_layer_coord({{Layer, _}, _Type}) -> Layer;
get_layer_coord(_) -> 0.5.

%%==============================================================================
%% LTC Utilities
%%==============================================================================

%% @doc Perturb a list of LTC weights.
-spec perturb_ltc_weight_list([float()], float()) -> [float()].
perturb_ltc_weight_list([], _PerturbRange) ->
    %% Initialize with small random weights if empty
    [rand:uniform() - 0.5, rand:uniform() - 0.5];
perturb_ltc_weight_list(Weights, PerturbRange) ->
    [W + (rand:uniform() - 0.5) * PerturbRange || W <- Weights].
