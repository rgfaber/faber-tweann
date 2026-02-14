# Quick Start

This guide walks you through creating and evolving a simple neural network.

## Initialize Database

First, initialize the Mnesia database:

```erlang
genotype:init_db().
```

## Create an Agent

Create a simple XOR agent:

```erlang
%% Create a constraint specifying the morphology
Constraint = #constraint{
    morphology = xor_mimic,
    connection_architecture = recurrent
}.

%% Construct the agent genotype
{ok, AgentId} = genotype:construct_agent(Constraint).
```

This creates a minimal neural network topology stored in Mnesia.

## Construct Phenotype

Build the running process network:

```erlang
Phenotype = constructor:construct_phenotype(AgentId).
```

This spawns all network processes (cortex, sensors, neurons, actuators).

## Evaluate

Trigger an evaluation cycle:

```erlang
cortex:sync(Phenotype#phenotype.cortex_pid).
```

The network will:
1. Read inputs from sensors
2. Process through neurons
3. Generate outputs via actuators
4. Compute fitness

## Evolve

Apply random mutations to improve the network:

```erlang
%% Mutate the agent
genome_mutator:mutate(AgentId).

%% Construct new phenotype to test
NewPhenotype = constructor:construct_phenotype(AgentId).
cortex:sync(NewPhenotype#phenotype.cortex_pid).
```

## Population Evolution

For evolving a population:

```erlang
%% Create population parameters
PMP = #pmp{
    population_id = xor_population,
    init_specie_size = 20,
    generation_limit = 100,
    fitness_goal = 0.9
}.

%% Start evolution
population_monitor:start(PMP).
```

The population monitor will:
- Evaluate all agents
- Select survivors based on fitness
- Create offspring through mutation/crossover
- Continue until generation limit or fitness goal reached

## Read Results

After evolution:

```erlang
%% Get the best agent
{ok, BestAgent} = genotype:read({agent, BestAgentId}).
Fitness = BestAgent#agent.fitness.

%% Inspect topology
Cortex = genotype:read({cortex, BestAgent#agent.cx_id}).
NeuronCount = length(Cortex#cortex.neuron_ids).
```

## Cleanup

When done:

```erlang
%% Reset database (WARNING: deletes all data)
genotype:reset_db().
```

## Next Steps

- [Architecture](architecture.md) - Understand the system design
- See module documentation for detailed API reference
