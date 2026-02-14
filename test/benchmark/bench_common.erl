%% @doc Common utilities for benchmark tests.
%%
%% Provides timing, memory measurement, and statistical functions
%% used across all benchmark modules.
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever
-module(bench_common).

-export([
    measure_time/1,
    measure_time/2,
    measure_memory/1,
    run_trials/3,
    run_trials_gc/3,
    format_bytes/1,
    format_time/1,
    calc_stats/1,
    create_test_network/1,
    create_test_signals/2
]).

%% @doc Measure execution time of a function in microseconds.
-spec measure_time(fun(() -> term())) -> {non_neg_integer(), term()}.
measure_time(Fun) ->
    measure_time(Fun, microsecond).

%% @doc Measure execution time in specified unit.
-spec measure_time(fun(() -> term()), erlang:time_unit()) ->
    {non_neg_integer(), term()}.
measure_time(Fun, Unit) ->
    Start = erlang:monotonic_time(Unit),
    Result = Fun(),
    End = erlang:monotonic_time(Unit),
    {End - Start, Result}.

%% @doc Measure memory delta of a function in bytes.
-spec measure_memory(fun(() -> term())) -> {integer(), term()}.
measure_memory(Fun) ->
    erlang:garbage_collect(),
    {memory, StartMem} = erlang:process_info(self(), memory),
    Result = Fun(),
    {memory, EndMem} = erlang:process_info(self(), memory),
    {EndMem - StartMem, Result}.

%% @doc Run multiple trials and collect timing statistics.
-spec run_trials(fun(() -> term()), pos_integer(), pos_integer()) ->
    #{min := float(), max := float(), avg := float(), median := float(),
      std := float(), times := [non_neg_integer()]}.
run_trials(Fun, Trials, WarmupRuns) ->
    %% Warmup runs
    _ = [Fun() || _ <- lists:seq(1, WarmupRuns)],

    %% Actual trials
    Times = [begin {T, _} = measure_time(Fun), T end
             || _ <- lists:seq(1, Trials)],

    calc_stats(Times).

%% @doc Run trials with GC between each run.
-spec run_trials_gc(fun(() -> term()), pos_integer(), pos_integer()) ->
    #{min := float(), max := float(), avg := float(), median := float(),
      std := float(), times := [non_neg_integer()]}.
run_trials_gc(Fun, Trials, WarmupRuns) ->
    %% Warmup runs with GC
    _ = [begin erlang:garbage_collect(), Fun() end
         || _ <- lists:seq(1, WarmupRuns)],

    %% Actual trials with GC
    Times = [begin
                 erlang:garbage_collect(),
                 {T, _} = measure_time(Fun),
                 T
             end
             || _ <- lists:seq(1, Trials)],

    calc_stats(Times).

%% @doc Calculate statistics from a list of values.
-spec calc_stats([number()]) ->
    #{min := float(), max := float(), avg := float(), median := float(),
      std := float(), times := [number()]}.
calc_stats([]) ->
    #{min => 0.0, max => 0.0, avg => 0.0, median => 0.0, std => 0.0, times => []};
calc_stats(Values) ->
    Sorted = lists:sort(Values),
    N = length(Values),
    Sum = lists:sum(Values),
    Avg = Sum / N,

    Min = hd(Sorted),
    Max = lists:last(Sorted),

    Median = case N rem 2 of
        1 -> float(lists:nth((N + 1) div 2, Sorted));
        0 ->
            Mid1 = lists:nth(N div 2, Sorted),
            Mid2 = lists:nth(N div 2 + 1, Sorted),
            (Mid1 + Mid2) / 2.0
    end,

    %% Standard deviation
    SumSquaredDiffs = lists:sum([(V - Avg) * (V - Avg) || V <- Values]),
    Std = math:sqrt(SumSquaredDiffs / N),

    #{
        min => float(Min),
        max => float(Max),
        avg => Avg,
        median => Median,
        std => Std,
        times => Values
    }.

%% @doc Format bytes to human readable string.
-spec format_bytes(non_neg_integer()) -> string().
format_bytes(Bytes) when Bytes < 1024 ->
    io_lib:format("~B B", [Bytes]);
format_bytes(Bytes) when Bytes < 1024 * 1024 ->
    io_lib:format("~.2f KB", [Bytes / 1024.0]);
format_bytes(Bytes) when Bytes < 1024 * 1024 * 1024 ->
    io_lib:format("~.2f MB", [Bytes / (1024.0 * 1024.0)]);
format_bytes(Bytes) ->
    io_lib:format("~.2f GB", [Bytes / (1024.0 * 1024.0 * 1024.0)]).

%% @doc Format time in microseconds to human readable string.
-spec format_time(non_neg_integer()) -> string().
format_time(Us) when Us < 1000 ->
    io_lib:format("~B us", [Us]);
format_time(Us) when Us < 1000000 ->
    io_lib:format("~.2f ms", [Us / 1000.0]);
format_time(Us) ->
    io_lib:format("~.2f s", [Us / 1000000.0]).

%% @doc Create a test feedforward network.
%%
%% Topology format: [InputSize, Hidden1, Hidden2, ..., OutputSize]
-spec create_test_network([pos_integer()]) -> network_evaluator:network().
create_test_network([InputSize | Rest]) ->
    %% Split into hidden and output
    {HiddenSizes, [OutputSize]} = lists:split(length(Rest) - 1, Rest),
    network_evaluator:create_feedforward(InputSize, HiddenSizes, OutputSize, tanh).

%% @doc Create random test signals.
-spec create_test_signals(pos_integer(), pos_integer()) -> [[float()]].
create_test_signals(NumSamples, InputSize) ->
    [[rand:uniform() * 2.0 - 1.0 || _ <- lists:seq(1, InputSize)]
     || _ <- lists:seq(1, NumSamples)].
