%% @doc Benchmark tests comparing NIF vs pure Erlang implementations.
%%
%% Tests the performance difference between the Rust NIF and pure Erlang
%% implementations of key operations.
%%
%% Run with: rebar3 eunit --module=bench_nif_vs_erlang
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever
-module(bench_nif_vs_erlang).

-include_lib("eunit/include/eunit.hrl").

-define(TRIALS, 100).
-define(WARMUP, 10).

%% @doc Benchmark dot_product: NIF vs Erlang.
dot_product_comparison_test() ->
    InputSizes = [5, 10, 20, 50, 100],

    io:format("~n=== Dot Product: NIF vs Erlang ===~n"),
    io:format("  Size    Erlang       NIF        Speedup~n"),
    io:format("  ----    ------       ---        -------~n"),

    lists:foreach(
        fun(Size) ->
            %% Create test data in signal_aggregator format
            Signals = [{make_ref(), [rand:uniform() * 2.0 - 1.0]}
                       || _ <- lists:seq(1, Size)],
            Weights = [{element(1, S), [{rand:uniform() * 2.0 - 1.0, 0.0, 0.1, []}]}
                       || S <- Signals],

            %% Pure Erlang timing
            ErlangStats = bench_common:run_trials(
                fun() -> signal_aggregator:dot_product(Signals, Weights) end,
                ?TRIALS,
                ?WARMUP
            ),

            %% NIF timing (may fall back to Erlang if not loaded)
            NifStats = bench_common:run_trials(
                fun() -> signal_aggregator:dot_product_nif(Signals, Weights) end,
                ?TRIALS,
                ?WARMUP
            ),

            ErlangAvg = maps:get(avg, ErlangStats),
            NifAvg = maps:get(avg, NifStats),
            Speedup = ErlangAvg / max(1.0, NifAvg),

            io:format("  ~4B    ~8.1f us  ~8.1f us  ~6.1fx~n",
                     [Size, ErlangAvg, NifAvg, Speedup])
        end,
        InputSizes
    ),

    ok.

%% @doc Benchmark flat dot product (NIF primitive).
flat_dot_product_test() ->
    NifLoaded = tweann_nif:is_loaded(),

    io:format("~n=== Flat Dot Product NIF ===~n"),
    io:format("  NIF Loaded: ~p~n", [NifLoaded]),

    case NifLoaded of
        true ->
            Sizes = [10, 50, 100, 500, 1000],

            io:format("  Size    Time (avg)~n"),
            io:format("  ----    ----------~n"),

            lists:foreach(
                fun(Size) ->
                    Signals = [rand:uniform() * 2.0 - 1.0 || _ <- lists:seq(1, Size)],
                    Weights = [rand:uniform() * 2.0 - 1.0 || _ <- lists:seq(1, Size)],
                    Bias = rand:uniform() * 0.1,

                    Stats = bench_common:run_trials(
                        fun() ->
                            tweann_nif:dot_product_flat(Signals, Weights, Bias)
                        end,
                        ?TRIALS,
                        ?WARMUP
                    ),

                    io:format("  ~5B   ~s~n",
                             [Size, bench_common:format_time(round(maps:get(avg, Stats)))])
                end,
                Sizes
            );
        false ->
            io:format("  (skipping - NIF not loaded)~n")
    end,

    ok.

%% @doc Benchmark batch dot product NIF.
batch_dot_product_test() ->
    NifLoaded = tweann_nif:is_loaded(),

    io:format("~n=== Batch Dot Product NIF ===~n"),
    io:format("  NIF Loaded: ~p~n", [NifLoaded]),

    case NifLoaded of
        true ->
            BatchSizes = [1, 10, 50, 100],
            InputSize = 50,

            io:format("  Batch   Individual    Batch       Speedup~n"),
            io:format("  -----   ----------    -----       -------~n"),

            lists:foreach(
                fun(BatchSize) ->
                    %% Create batch data
                    Batch = [{
                        [rand:uniform() * 2.0 - 1.0 || _ <- lists:seq(1, InputSize)],
                        [rand:uniform() * 2.0 - 1.0 || _ <- lists:seq(1, InputSize)],
                        rand:uniform() * 0.1
                    } || _ <- lists:seq(1, BatchSize)],

                    %% Individual calls timing
                    IndividualStats = bench_common:run_trials(
                        fun() ->
                            [tweann_nif:dot_product_flat(S, W, B)
                             || {S, W, B} <- Batch]
                        end,
                        ?TRIALS,
                        ?WARMUP
                    ),

                    %% Batch call timing
                    BatchStats = bench_common:run_trials(
                        fun() -> tweann_nif:dot_product_batch(Batch) end,
                        ?TRIALS,
                        ?WARMUP
                    ),

                    IndAvg = maps:get(avg, IndividualStats),
                    BatchAvg = maps:get(avg, BatchStats),
                    Speedup = IndAvg / max(1.0, BatchAvg),

                    io:format("  ~5B   ~8.1f us   ~8.1f us  ~6.2fx~n",
                             [BatchSize, IndAvg, BatchAvg, Speedup])
                end,
                BatchSizes
            );
        false ->
            io:format("  (skipping - NIF not loaded)~n")
    end,

    ok.

%% @doc Check NIF status.
nif_status_test() ->
    io:format("~n=== NIF Status ===~n"),
    io:format("  tweann_nif:is_loaded() = ~p~n", [tweann_nif:is_loaded()]),
    ok.
