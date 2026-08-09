#!/usr/bin/env escript
%%! -pa _build/default/lib/faber_tweann/ebin
%%
%% Produce both sides of the ONNX boundary: an exported model, and the
%% evaluator's own output for the same input. check_onnx_export.py decides
%% whether onnxruntime agrees.
%%
%% This exists because every eunit test for network_onnx asserted only that
%% bytes came out and that there were more than zero of them. Nothing loaded a
%% model, so nothing could see that the exported graph computed the wrong
%% function. Two defects were sitting under that, and this script found both.
-include_lib("faber_tweann/include/records.hrl").

main([OutDir]) ->
    ok = filelib:ensure_path(OutDir),
    genotype:init_db(),
    Cases = shapes() ++ [evolved()],
    lists:foreach(fun({Name, Net, In}) -> emit(OutDir, Name, Net, In) end, Cases),
    io:format("wrote ~p cases to ~s~n", [length(Cases), OutDir]).

%% Activation pairs matter more than sizes here: the output-activation defect
%% was invisible whenever the hidden and output activations were equal.
shapes() ->
    [{"one_layer", network_evaluator:create_feedforward(3, [], 2, tanh, tanh), [0.5, -0.25, 1.0]},
     {"two_hidden", network_evaluator:create_feedforward(4, [5, 3], 2, tanh, tanh), [0.1, 0.2, -0.3, 0.4]},
     {"relu_linear", network_evaluator:create_feedforward(3, [4], 1, relu, linear), [1.0, -2.0, 0.5]},
     {"tanh_linear", network_evaluator:create_feedforward(3, [4], 2, tanh, linear), [0.3, 0.6, -0.9]},
     {"sigmoid_tanh", network_evaluator:create_feedforward(2, [3], 2, sigmoid, tanh), [0.7, -0.7]},
     {"sigmoid", network_evaluator:create_feedforward(2, [3], 2, sigmoid, sigmoid), [0.7, -0.7]}].

%% The charter's framing, end to end: a genotype that only ever existed in ETS
%% becomes a network, becomes ONNX, and runs somewhere that is not the BEAM. If
%% this one agrees, an evolved controller can leave the simulator.
evolved() ->
    Ag = a_genotype(),
    {ok, Net} = network_evaluator:from_genotype(Ag),
    {"evolved_genotype", Net, [1.0, 2.0]}.

a_genotype() ->
    U = fun() -> genotype:generate_UniqueId() end,
    W = fun(X) -> {X, 0.0, 0.0, []} end,
    S = {{-1.0, U()}, sensor},
    H1 = {{0.0, U()}, neuron},
    H2 = {{0.0, U()}, neuron},
    O = {{0.5, U()}, neuron},
    A = {{1.0, U()}, actuator},
    Cx = {{origin, U()}, cortex},
    Ag = {{origin, U()}, agent},
    genotype:write(#sensor{id = S, cx_id = Cx, name = test_in, vl = 2, fanout_ids = [H1, H2]}),
    genotype:write(#neuron{id = H1, cx_id = Cx, af = tanh, aggr_f = dot_product,
                           input_idps = [{S, [W(0.3), W(-0.2)]}, {bias, [W(0.1)]}],
                           output_ids = [O]}),
    genotype:write(#neuron{id = H2, cx_id = Cx, af = tanh, aggr_f = dot_product,
                           input_idps = [{S, [W(0.5), W(0.4)]}], output_ids = [O]}),
    genotype:write(#neuron{id = O, cx_id = Cx, af = linear, aggr_f = dot_product,
                           input_idps = [{H1, [W(0.9)]}, {H2, [W(-0.7)]}, {bias, [W(0.05)]}],
                           output_ids = [A]}),
    genotype:write(#actuator{id = A, cx_id = Cx, name = test_out, vl = 1, fanin_ids = [O]}),
    genotype:write(#cortex{id = Cx, agent_id = Ag, neuron_ids = [H1, H2, O],
                           sensor_ids = [S], actuator_ids = [A]}),
    genotype:write(#agent{id = Ag, cx_id = Cx, generation = 0}),
    Ag.

emit(Dir, Name, Net, In) ->
    Out = network_evaluator:evaluate(Net, In),
    case network_onnx:to_onnx(Net) of
        {ok, Bin} ->
            ok = file:write_file(filename:join(Dir, Name ++ ".onnx"), Bin),
            ok = file:write_file(filename:join(Dir, Name ++ ".json"),
                                 io_lib:format("{\"input\": ~s, \"expected\": ~s}",
                                               [nums(In), nums(Out)])),
            io:format("~-18s exported ~p bytes~n", [Name, byte_size(Bin)]);
        {error, Reason} ->
            io:format("~-18s EXPORT FAILED: ~p~n", [Name, Reason])
    end.

nums(L) -> ["[", lists:join(",", [io_lib:format("~.10f", [F]) || F <- L]), "]"].
