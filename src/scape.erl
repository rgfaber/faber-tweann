%% @doc The scape: the environment a neural network is evaluated in.
%%
%% A scape is an independent process, spawned by the exoself before the
%% sensors, neurons and actuators, and terminated before the cortex. It owns
%% the problem state and is the ONLY source of fitness.
%%
%% == Why this module exists ==
%%
%% faber-tweann shipped without any scape. `#sensor.scape' and
%% `#actuator.scape' were declared in the morphologies and read by nobody,
%% sensors returned zeros for unrecognised names, actuators passed their input
%% through, and `exoself' computed fitness as the sum of the network's own
%% outputs. No end-to-end evolutionary run had ever completed. See
%% faber-ecosystem/docs/CONFORMANCE.md and insight 001.
%%
%% == Protocol ==
%%
%% Verbatim from DXNN2 (see faber-ecosystem/docs/PROTOCOL.md). The wire format
%% matters: the Ch 13 benchmarker and the Ch 16 substrate encoding both assume
%% it.
%%
%%   sensor   -> scape   {SensorPid, sense, Name, Params}
%%   scape    -> sensor  {ScapePid, percept, SensoryVector}
%%   actuator -> scape   {ActuatorPid, action, Name, Params, Output}
%%   scape    -> actuator {ScapePid, Fitness, HaltFlag}
%%   exoself  -> scape   {ExoSelfPid, terminate}
%%
%% HaltFlag is 0 to continue, 1 to end the evaluation, or the atom
%% `goal_reached' when the scape has decided the task is solved. goal_reached
%% propagates through actuator and cortex to the exoself so that
%% evaluations-to-solve can be recorded; without it that metric is
%% unmeasurable, which is the whole reason Table 14.1 comparison was blocked.
%%
%% == Departure from DXNN2 ==
%%
%% Sher implements every scape as a function inside one `scape' module
%% (`scape:xor_sim/1'). Here `scape' is a generic process that dispatches to a
%% callback module implementing the `scape' behaviour. The morphologies already
%% declare scapes as atoms (`scape = {private, xor_sim}'), so dispatching to a
%% module of that name is both more faithful to the declaration and extensible
%% without editing a shared file. The wire protocol is unchanged.
%%
%% @copyright 2024-2026 R.G. Lefever
%% @license Apache-2.0
-module(scape).

-export([gen/2, gen/3, prep/1]).

%%%============================================================================
%%% Behaviour
%%%============================================================================

%% @doc Initialise scape state. Called once when the scape process starts.
-callback init(Params :: term()) -> State :: term().

%% @doc Produce the sensory vector for the current state.
%%
%% SensorName lets one scape serve several sensors (DXNN2's pole balancing
%% uses this to offer full or partial state information).
-callback sense(SensorName :: atom(), Params :: term(), State :: term()) ->
    {SensoryVector :: [float()], State :: term()}.

%% @doc Apply the network's output and return fitness for this step.
%%
%% HaltFlag: 0 continue, 1 end the evaluation, `goal_reached' when solved.
%%
%% Fitness is normally 0.0 on intermediate steps with the real value returned
%% once at halt (DXNN2 calls this lifetime-based fitness), but a scape may
%% award fitness per step instead.
-callback act(ActuatorName :: atom(), Params :: term(), Output :: [float()],
              State :: term()) ->
    {Fitness :: float(), HaltFlag :: 0 | 1 | goal_reached, State :: term()}.

-optional_callbacks([]).

%%%============================================================================
%%% Lifecycle
%%%============================================================================

%% @doc Spawn a scape process on the local node.
gen(ExoSelfPid, Node) ->
    spawn(Node, ?MODULE, prep, [ExoSelfPid]).

%% @doc Spawn and immediately name it, for callers that have the name to hand.
gen(ExoSelfPid, Node, {Module, Params}) ->
    Pid = spawn(Node, ?MODULE, prep, [ExoSelfPid]),
    Pid ! {ExoSelfPid, Module, Params},
    Pid.

%% @private Wait to be told which scape to become, then run it.
%%
%% Matches DXNN2's two-step spawn-then-name handshake, which exists so the
%% exoself can spawn scapes before it has finished reading the genotype.
prep(ExoSelfPid) ->
    receive
        {ExoSelfPid, Module, Params} ->
            State = Module:init(Params),
            loop(ExoSelfPid, Module, State);
        {ExoSelfPid, Module} ->
            State = Module:init(undefined),
            loop(ExoSelfPid, Module, State);
        {ExoSelfPid, terminate} ->
            ok
    end.

%%%============================================================================
%%% Main loop
%%%============================================================================

loop(ExoSelfPid, Module, State) ->
    receive
        {From, sense, SensorName, Params} ->
            {SensoryVector, State1} = Module:sense(SensorName, Params, State),
            From ! {self(), percept, SensoryVector},
            loop(ExoSelfPid, Module, State1);

        {From, action, ActuatorName, Params, Output} ->
            {Fitness, HaltFlag, State1} = Module:act(ActuatorName, Params, Output, State),
            From ! {self(), Fitness, HaltFlag},
            loop(ExoSelfPid, Module, State1);

        {ExoSelfPid, reset} ->
            %% Fresh state for the next evaluation of the same phenotype,
            %% without paying to respawn the process.
            loop(ExoSelfPid, Module, Module:init(undefined));

        {ExoSelfPid, terminate} ->
            ok
    end.
