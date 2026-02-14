%% @doc Benchmark tests for network forward propagation.
%%
%% Tests forward pass latency for different network sizes and compares
%% pure Erlang vs NIF-accelerated evaluation.
%%
%% Run with: rebar3 eunit --module=bench_forward_pass
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever
-module(bench_forward_pass).

-include_lib("eunit/include/eunit.hrl").

-define(TRIALS, 100).
-define(WARMUP, 10).

%% @doc Benchmark small network (10 inputs, 8 hidden, 4 outputs).
small_network_test() ->
    Topology = [10, 8, 4],
    Inputs = [rand:uniform() * 2.0 - 1.0 || _ <- lists:seq(1, 10)],
    run_network_benchmark("Small (10-8-4)", Topology, Inputs).

%% @doc Benchmark medium network (50 inputs, 32-16 hidden, 8 outputs).
medium_network_test() ->
    Topology = [50, 32, 16, 8],
    Inputs = [rand:uniform() * 2.0 - 1.0 || _ <- lists:seq(1, 50)],
    run_network_benchmark("Medium (50-32-16-8)", Topology, Inputs).

%% @doc Benchmark large network (100 inputs, 64-32-16 hidden, 10 outputs).
large_network_test() ->
    Topology = [100, 64, 32, 16, 10],
    Inputs = [rand:uniform() * 2.0 - 1.0 || _ <- lists:seq(1, 100)],
    run_network_benchmark("Large (100-64-32-16-10)", Topology, Inputs).

%% @doc Benchmark XOR-like network (3 inputs, 4 hidden, 1 output).
xor_network_test() ->
    Topology = [3, 4, 1],
    Inputs = [1.0, 0.0, 1.0],
    run_network_benchmark("XOR (3-4-1)", Topology, Inputs).

%% @private Run benchmark for a specific network configuration.
run_network_benchmark(Name, Topology, Inputs) ->
    %% Create network
    Network = bench_common:create_test_network(Topology),

    %% Run trials
    Stats = bench_common:run_trials(
        fun() -> network_evaluator:evaluate(Network, Inputs) end,
        ?TRIALS,
        ?WARMUP
    ),

    %% Output results
    io:format("~n=== Forward Pass: ~s ===~n", [Name]),
    io:format("  Avg: ~s~n", [bench_common:format_time(round(maps:get(avg, Stats)))]),
    io:format("  Min: ~s~n", [bench_common:format_time(round(maps:get(min, Stats)))]),
    io:format("  Max: ~s~n", [bench_common:format_time(round(maps:get(max, Stats)))]),
    io:format("  Std: ~.2f us~n", [maps:get(std, Stats)]),

    %% Verify network works
    Outputs = network_evaluator:evaluate(Network, Inputs),
    ?assert(is_list(Outputs)),
    ?assertEqual(lists:last(Topology), length(Outputs)),

    ok.

%% @doc Benchmark batch evaluation.
batch_evaluation_test() ->
    Topology = [10, 8, 4],
    Network = bench_common:create_test_network(Topology),
    BatchSizes = [1, 10, 50, 100],

    io:format("~n=== Batch Evaluation Benchmark ===~n"),

    lists:foreach(
        fun(BatchSize) ->
            InputBatch = bench_common:create_test_signals(BatchSize, 10),

            Stats = bench_common:run_trials(
                fun() ->
                    [network_evaluator:evaluate(Network, I) || I <- InputBatch]
                end,
                ?TRIALS,
                ?WARMUP
            ),

            AvgPerSample = maps:get(avg, Stats) / BatchSize,
            io:format("  Batch ~3B: ~s total, ~s/sample~n",
                     [BatchSize,
                      bench_common:format_time(round(maps:get(avg, Stats))),
                      bench_common:format_time(round(AvgPerSample))])
        end,
        BatchSizes
    ),

    ok.

%% @doc Memory usage during forward pass.
memory_usage_test() ->
    Topology = [50, 32, 16, 8],
    Network = bench_common:create_test_network(Topology),
    Inputs = [rand:uniform() * 2.0 - 1.0 || _ <- lists:seq(1, 50)],

    %% Measure memory for single evaluation
    {MemDelta, _Result} = bench_common:measure_memory(
        fun() ->
            [network_evaluator:evaluate(Network, Inputs) || _ <- lists:seq(1, 100)]
        end
    ),

    io:format("~n=== Memory Usage: Medium Network (100 evals) ===~n"),
    io:format("  Total delta: ~s~n", [bench_common:format_bytes(MemDelta)]),
    io:format("  Per eval: ~s~n", [bench_common:format_bytes(MemDelta div 100)]),

    ok.
