%%% @doc Tests for the add_delay mutation operator.
%%%
%%% Two things are being defended. That evolution can introduce a memory
%%% organelle at all, which is what makes the delay a TWEANN capability rather
%%% than something a person has to author. And that a delay CANNOT be run by the
%%% process-per-neuron phenotype, loudly, because that phenotype has no delay
%%% process and its dispatch would otherwise fall through to a standard neuron
%%% and evaluate a memory organelle as ordinary arithmetic.
%%%
%%% The second is the one worth having. exoself's own comment on that dispatch
%%% says it exists "so LTC genotypes are evaluated as LTC, not as standard
%%% neurons that silently ignore their temporal parameters". A delay would have
%%% walked straight into the failure that comment describes.
-module(add_delay_tests).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

setup() ->
    application:ensure_all_started(faber_tweann),
    test_helper:register_all_example_morphologies(),
    genotype:init_db(),
    innovation:init().

teardown(_) ->
    genotype:reset_db().

add_delay_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        fun splices_a_delay_into_an_existing_connection/0,
        fun the_spliced_delay_preserves_the_path_gain/0,
        fun a_spliced_delay_converts_and_delays_by_one_tick/0,
        fun it_is_reachable_through_genome_mutator/0,
        fun it_is_not_in_the_default_operator_list/0,
        fun the_process_phenotype_refuses_a_delay_rather_than_faking_it/0,
        fun the_dag_path_runs_what_the_process_path_refuses/0,
        fun splices_a_leaky_integrator_with_a_drawn_time_constant/0,
        fun the_time_constant_is_reachable_by_mutate_time_constant/0,
        fun it_is_also_absent_from_the_default_operator_list/0,
        fun the_process_phenotype_refuses_a_leaky_too/0
    ]}.

an_agent() ->
    Id = {{origin, genotype:generate_UniqueId()}, agent},
    genotype:construct_Agent(test_specie, Id, #constraint{morphology = xor_mimic}),
    Id.

neurons_of(AgentId) ->
    Agent = genotype:dirty_read({agent, AgentId}),
    Cx = genotype:dirty_read({cortex, Agent#agent.cx_id}),
    [genotype:dirty_read({neuron, I}) || I <- Cx#cortex.neuron_ids].

delays_of(AgentId) ->
    [N || N <- neurons_of(AgentId), N#neuron.neuron_type =:= delay].

%%==============================================================================
%% The operator
%%==============================================================================

splices_a_delay_into_an_existing_connection() ->
    AgentId = an_agent(),
    Before = length(neurons_of(AgentId)),
    ?assertEqual([], delays_of(AgentId)),
    ?assertEqual(ok, topological_mutations:add_delay(AgentId)),
    ?assertEqual(Before + 1, length(neurons_of(AgentId))),
    [D] = delays_of(AgentId),
    %% Exactly one input, which is the delay-line shape, and no activation to
    %% suggest the evaluator applies one.
    ?assertEqual(1, length(D#neuron.input_idps)),
    ?assertEqual(linear, D#neuron.af),
    ?assertEqual([], D#neuron.ro_ids).

%% add_neuron/1 splices with the original weight on BOTH the new input and the
%% link out, which squares the path gain. A delay must not do that, or it would
%% be a gain change as well as a delay and the two would be inseparable after.
the_spliced_delay_preserves_the_path_gain() ->
    AgentId = an_agent(),
    ok = topological_mutations:add_delay(AgentId),
    [D] = delays_of(AgentId),
    [{_Src, [{W, _, _, _}]}] = D#neuron.input_idps,
    ?assertEqual(1.0, W).

%% End to end: the mutated genotype converts on the DAG path and the spliced
%% organelle costs the signal exactly one tick.
a_spliced_delay_converts_and_delays_by_one_tick() ->
    AgentId = an_agent(),
    {ok, Before} = genotype_to_dag:compile(AgentId),
    Steady = tweann_nif:evaluate(Before, [1.0, 1.0]),
    ok = topological_mutations:add_delay(AgentId),
    {ok, After} = genotype_to_dag:compile(AgentId),
    %% Tick one runs on an empty organelle, so it cannot equal the undelayed
    %% answer; by the time the delay has been fed, it does.
    {Tick1, S1} = tweann_nif:evaluate_with_state(After, [1.0, 1.0], []),
    {_Tick2, S2} = tweann_nif:evaluate_with_state(After, [1.0, 1.0], S1),
    {Tick3, _} = tweann_nif:evaluate_with_state(After, [1.0, 1.0], S2),
    ?assertEqual(1, length(S1)),
    ?assertNotEqual(Tick1, Tick3),
    ?assertEqual(Steady, Tick3).

%% mutate/2 takes a COUNT and draws the operator from the agent's own
%% mutation_operators, so reaching add_delay means constructing an agent whose
%% constraint asks for it. That is exactly how a caller opts in.
it_is_reachable_through_genome_mutator() ->
    Id = {{origin, genotype:generate_UniqueId()}, agent},
    genotype:construct_Agent(test_specie, Id,
                             #constraint{morphology = xor_mimic,
                                         mutation_operators = [{add_delay, 100}]}),
    ?assertEqual([], delays_of(Id)),
    ok = genome_mutator:mutate(Id, 1),
    ?assertEqual(1, length(delays_of(Id))).

%% ⚠ Deliberate. A population driven by population_monitor uses the process
%% phenotype, which raises on a delay, so shipping this in the default list
%% would crash every existing run. It is opt-in per constraint.
it_is_not_in_the_default_operator_list() ->
    Default = #constraint{},
    Operators = [Op || {Op, _Weight} <- Default#constraint.mutation_operators],
    ?assertNot(lists:member(add_delay, Operators)),
    %% and the ones that ARE there are still there
    ?assert(lists:member(add_neuron, Operators)).

%%==============================================================================
%% The guard
%%==============================================================================

%% Both phenotype builders dispatch on neuron_type and both used to end in a
%% catch-all spawning a standard neuron. A delay reaching that clause would
%% compute an activation of its inputs instead of emitting last tick's value,
%% and nothing would say so.
%% Driven through the public builder rather than the private dispatch, so it is
%% the behaviour that is pinned and not the shape of an internal function.
the_process_phenotype_refuses_a_delay_rather_than_faking_it() ->
    AgentId = an_agent(),
    ok = topological_mutations:add_delay(AgentId),
    ?assertError({organelle_has_no_process_phenotype, _, _},
                 constructor:construct(AgentId)).

%% And the same genotype the process path refuses is one the DAG path runs, so
%% the refusal is about that phenotype rather than about the genotype being bad.
the_dag_path_runs_what_the_process_path_refuses() ->
    AgentId = an_agent(),
    ok = topological_mutations:add_delay(AgentId),
    ?assertError({organelle_has_no_process_phenotype, _, _},
                 constructor:construct(AgentId)),
    {ok, Net} = genotype_to_dag:compile(AgentId),
    {Out, State} = tweann_nif:evaluate_with_state(Net, [1.0, 1.0], []),
    ?assertEqual(1, length(State)),
    ?assert(is_list(Out)).

%%==============================================================================
%% add_leaky, the second organelle's operator
%%==============================================================================

splices_a_leaky_integrator_with_a_drawn_time_constant() ->
    AgentId = an_agent(),
    ?assertEqual(ok, topological_mutations:add_leaky(AgentId)),
    [L] = [N || N <- neurons_of(AgentId), N#neuron.neuron_type =:= leaky],
    ?assertEqual(1, length(L#neuron.input_idps)),
    %% Same range create_cfc_feedforward draws tau from.
    ?assert(L#neuron.time_constant >= 0.1),
    ?assert(L#neuron.time_constant =< 2.0),
    [{_Src, [{W, _, _, _}]}] = L#neuron.input_idps,
    ?assertEqual(1.0, W).

%% Placement comes from this operator; the constant is then tuned by the
%% machinery that already exists, which is what makes it evolvable rather than
%% fixed at the moment it was spliced.
the_time_constant_is_reachable_by_mutate_time_constant() ->
    AgentId = an_agent(),
    ok = topological_mutations:add_leaky(AgentId),
    [Before] = [N || N <- neurons_of(AgentId), N#neuron.neuron_type =:= leaky],
    Moved = lists:any(
        fun(_) ->
            _ = ltc_mutations:mutate_time_constant(AgentId),
            [After] = [N || N <- neurons_of(AgentId), N#neuron.neuron_type =:= leaky],
            After#neuron.time_constant =/= Before#neuron.time_constant
        end,
        lists:seq(1, 25)),
    ?assert(Moved).

it_is_also_absent_from_the_default_operator_list() ->
    Default = #constraint{},
    Operators = [Op || {Op, _W} <- Default#constraint.mutation_operators],
    ?assertNot(lists:member(add_leaky, Operators)).

the_process_phenotype_refuses_a_leaky_too() ->
    AgentId = an_agent(),
    ok = topological_mutations:add_leaky(AgentId),
    ?assertError({organelle_has_no_process_phenotype, leaky, _},
                 constructor:construct(AgentId)).
