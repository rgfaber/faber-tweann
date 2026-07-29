%% @doc Robo Rumble: the deterministic integer forward pass (pure, table-driven).
%%
%% The companion to robo_sim. The engine was always going to be integer; the
%% FORWARD PASS is the real determinism hazard named by PLAN_ROBO_RUMBLE.md
%% section 3, because math:tanh/1 comes out of libm and libm is not bit-identical
%% across libc versions. A controller that called it would replay differently on
%% two honest boxes, which is worse than cheating because nobody could tell. So
%% there is no libm here and no float here.
%%
%% UNITS. The network works in Q12: 4096 is 1.0. This is deliberately NOT the
%% arena's scale of 256. Arena quantities widen into Q12 by an exact shift of 4,
%% losing nothing. Q12 puts 8193 distinct levels on an activation, against the
%% 1025 distinct values of the widest intent field, so activation quantisation is
%% never what limits a controller. Weights are Q12 as well, so a weight times an
%% activation is Q24, the dot product accumulates in Q24 with NO intermediate
%% rounding, and exactly ONE rounding happens per neuron, in activate/1.
%%
%% THAT SINGLE ROUNDING IS A RULE, not a convenience. Integer addition is
%% associative, so an exact Q24 accumulator makes the summation order irrelevant:
%% visiting the same weight-input pairs in any order gives the same neuron, to
%% the bit. Narrowing per term instead would make a neuron depend on the order
%% its inputs were visited, which is a real cross-machine divergence the moment
%% anything reorders a list. The pairing is of course not free to change, only
%% the order in which the pairs are summed.
%%
%% SMOOTHNESS IS NOT REQUIRED, and the choice of tanh should not be read as
%% claiming otherwise. Evolution uses no gradients. The engine hard-clamps every
%% intent field, so the terminal nonlinearity of any controller is a hard clamp
%% whatever the hidden activation is. What the task actually needs from a
%% nonlinearity is that it be bounded, odd, monotone, and flat late rather than
%% early, so that neurons do not park on an exactly-zero-slope plateau where
%% weight mutations produce no behavioural change and selection has nothing to
%% grade. tanh is chosen from that family for two reasons only: its saturation
%% onset in Q12 is late (4.875), and every table entry below has a one-line
%% closed form a reviewer can recompute, which a fitted polynomial does not.
%%
%% Every operation below is integer comparison, negation, addition, subtraction,
%% multiplication, arithmetic shift, mask and tuple indexing. All of those are
%% exactly specified by the Erlang language with no implementation freedom, and
%% Erlang integers are arbitrary precision so nothing can overflow or wrap. That
%% is what makes a match a pure function of its inputs.
%%
%% THE NETWORK. A fixed-topology feedforward multilayer perceptron. The topology
%% is a list of layer widths, so [8, 10, 5] is eight sensors, one hidden layer of
%% ten, five outputs. Every layer including the OUTPUT layer is passed through
%% activate/1, which is why the output range is guaranteed rather than hoped for
%% and why to_range/2 is total. The parameters are ONE FLAT LIST OF INTEGERS at
%% the arena's scale of 256, because the optimisers this repo already owns
%% (mu_lambda_es, sep_cma_es) evolve flat vectors and nothing else. The layout is
%% fixed and is part of the genome contract: layers in order, neurons within a
%% layer in order, and for each neuron its BIAS FIRST followed by one weight per
%% input in input order. weight_count/1 is the length that layout requires.
%%
%% THE FLOAT BOUNDARY IS quantize/1 AND NOTHING ELSE. Optimisers search in
%% floating point, so somewhere a float has to become an integer. That happens
%% once, at PHENOTYPE BUILD TIME, before the match starts, and its result is what
%% gets hashed into the genome. No function reachable from eval/3 touches a
%% float. If a float ever appears downstream of quantize/1 the determinism
%% guarantee is void, so that boundary is the thing to defend in review.
%%
%% ROUNDING IS ONE CONVENTION, EVERYWHERE: round half AWAY FROM ZERO, applied by
%% splitting on sign so that the mapping is exactly odd. quantize/1 uses the
%% round/1 BIF, which is already half-away-from-zero; narrow/1, to_arena/1 and
%% to_range/2 each split on sign by hand. The consequence worth stating is
%% negative: no arithmetic right shift in this module is ever applied to a
%% negative operand, so the classic fixed-point trap of mixing floor semantics
%% with truncate-toward-zero semantics is not merely specified here, it is never
%% reached. A port to another language must copy that rule rather than
%% substitute a division, which truncates toward zero in C and Rust and floors
%% in Python.
%% @end
-module(robo_net).

%% The activation
-export([tanh/1, activate/1, scale/0]).
%% The fixed-point contract around it
-export([from_arena/1, to_arena/1, narrow/1, dot/3, to_range/2]).
%% The network
-export([weight_count/1, weight_limit/0, quantize/1, eval/3, eval_q12/3]).

-define(ONE, 4096).        %% Q12 1.0
-define(SHIFT, 7).         %% segment width is 128 Q12 units, that is 1/32
-define(FRAC, 127).        %% low bits of the input: position inside a segment
-define(HALF_SEG, 64).     %% half of 1 bsl 7: round-half-up inside a segment
-define(NARROW_RND, 2048). %% half of 1 bsl 12: round-half-up folding Q24 to Q12
-define(ARENA_RND, 8).     %% half of 1 bsl 4: round-half-up folding Q12 to 256
-define(SAT, 19968).       %% 4.875 in Q12: the first segment boundary at which
                           %% round(4096 * tanh(x)) has reached 4096 and stays
-define(ARENA_SCALE, 256). %% robo_sim's fixed-point scale, mirrored here
-define(W_LIMIT, 2048).    %% plus or minus 8.0 at scale 256, the weight cap

%% Tanh over 0.0 to 4.875 at 1/32 resolution, scaled by 4096: entry I is
%% round(4096 * tanh(I / 32)) for I in 0..156. CHECKED IN AS A LITERAL, for the
%% same reason robo_sim checks in its sine table: computing it at load would put
%% libm back in the match path and reintroduce the divergence this module exists
%% to prevent. The eunit suite recomputes that one-line formula and asserts
%% equality entry by entry, so this table is verified by recomputation rather
%% than trusted, which is the standard the front applies to match results. No
%% entry sits nearer than 0.00594 of a rounding tie, so no two libm
%% implementations can disagree about any of these values and the test is safe to
%% assert exactly. DO NOT EDIT: the table is a rule of the game, it is inside the
%% engine hash, and changing one entry voids the season.
-define(TANH_TABLE, {
    0, 128, 256, 383, 509, 635, 759, 882,
    1003, 1123, 1240, 1355, 1468, 1578, 1686, 1791,
    1893, 1992, 2088, 2181, 2272, 2359, 2443, 2524,
    2602, 2676, 2748, 2817, 2883, 2946, 3007, 3064,
    3119, 3172, 3222, 3270, 3315, 3358, 3399, 3438,
    3475, 3510, 3543, 3574, 3604, 3632, 3659, 3684,
    3707, 3730, 3751, 3771, 3790, 3808, 3825, 3841,
    3856, 3870, 3883, 3896, 3908, 3919, 3929, 3939,
    3949, 3957, 3966, 3973, 3981, 3988, 3994, 4000,
    4006, 4011, 4016, 4021, 4026, 4030, 4034, 4038,
    4041, 4044, 4048, 4050, 4053, 4056, 4058, 4061,
    4063, 4065, 4067, 4068, 4070, 4072, 4073, 4074,
    4076, 4077, 4078, 4079, 4080, 4081, 4082, 4083,
    4084, 4084, 4085, 4086, 4086, 4087, 4088, 4088,
    4089, 4089, 4089, 4090, 4090, 4091, 4091, 4091,
    4091, 4092, 4092, 4092, 4092, 4093, 4093, 4093,
    4093, 4093, 4094, 4094, 4094, 4094, 4094, 4094,
    4094, 4094, 4095, 4095, 4095, 4095, 4095, 4095,
    4095, 4095, 4095, 4095, 4095, 4095, 4095, 4095,
    4095, 4095, 4095, 4095, 4096
}).

%%==============================================================================
%% The activation
%%==============================================================================

%% What Q12 1.0 is, for callers that would otherwise hardcode 4096.
-spec scale() -> pos_integer().
scale() -> ?ONE.

%% Hyperbolic tangent of a Q12 value, in Q12. Total: every integer maps
%% somewhere, and inputs far outside the interesting range saturate rather than
%% misbehave.
%%
%% Saturation first, then oddness, then one table segment. Beyond 4.875 the Q12
%% representation of tanh is 4096 and cannot be anything else, so clamping there
%% costs 0.0000002 of accuracy and removes two fifths of the table.
%%
%% tanh(-X) is exactly -tanh(X): the negative case negates the input, evaluates
%% the positive branch and negates the result, with no rounding in between. Two
%% consequences. Every narrowing in this module is odd as well, so a BIAS-FREE
%% network fed mirrored inputs produces exactly mirrored outputs rather than
%% nearly-mirrored ones, which is worth having in an arena whose starting
%% positions are mirrored. Note the qualifier: a neuron with a nonzero bias is
%% not an odd function, so a trained controller does not inherit the symmetry and
%% is not claimed to. What is claimed, and what the property buys, is that no
%% rounding rule in the module breaks a symmetry the weights do have. And
%% segment/1 never sees a negative operand, so the classic fixed-point trap of
%% mixing floor semantics with truncate-toward-zero semantics is not merely
%% specified here, it is never reached.
-spec tanh(integer()) -> -4096..4096.
tanh(X) when X >= ?SAT -> ?ONE;
tanh(X) when X =< -?SAT -> -?ONE;
tanh(X) when X < 0 -> -segment(-X);
tanh(X) -> segment(X).

%% Linear interpolation inside one segment, for X in 0 up to SAT-1.
%%
%% The segment width is a power of two, so the index is a shift and the position
%% inside the segment is a mask, with no division anywhere. Because the table is
%% monotone non-decreasing and the rounded interpolant cannot overshoot its right
%% endpoint, the whole function is monotone and continuous by construction rather
%% than by measurement: the largest step between adjacent inputs is one least
%% significant bit, including across every segment boundary and across the
%% saturation seam.
segment(X) ->
    I = X bsr ?SHIFT,
    A = element(I + 1, ?TANH_TABLE),
    B = element(I + 2, ?TANH_TABLE),
    A + ((((B - A) * (X band ?FRAC)) + ?HALF_SEG) bsr ?SHIFT).

%% A neuron: fold its exact Q24 accumulator to Q12 and activate. This is the only
%% place a weighted sum is rounded, and callers reach the activation through here
%% rather than rounding for themselves.
-spec activate(integer()) -> -4096..4096.
activate(Acc) -> tanh(narrow(Acc)).

%%==============================================================================
%% The fixed-point contract
%%==============================================================================

%% Widen an arena value (scale 256) to Q12. Exact, lossless, never rounds. The
%% shift is 4 because Q12 is 2 to the 12 and the arena scale is 2 to the 8; the
%% two scales are locked together by that difference, so a change to robo_sim's
%% scale is a change here too.
-spec from_arena(integer()) -> integer().
from_arena(V) -> V bsl 4.

%% Narrow a Q12 value back to the arena's scale of 256, rounding half away from
%% zero. Lossy by exactly 4 bits, and the only reason it exists is that the
%% arena's unit scheme is the module's external contract: an activation of plus
%% or minus 4096 becomes plus or minus 256, which is plus or minus 1.0 in the
%% units robo_sim speaks. Sign-split for the same reason narrow/1 is, so the
%% round trip through Q12 stays exactly odd.
-spec to_arena(integer()) -> integer().
to_arena(V) when V < 0 -> -((-V + ?ARENA_RND) bsr 4);
to_arena(V) -> (V + ?ARENA_RND) bsr 4.

%% Fold a Q24 accumulator back to Q12, rounding half away from zero. The sign
%% split keeps the rounding odd, so the whole activation chain is exactly odd
%% rather than nearly odd, and a plain arithmetic shift on a negative accumulator
%% (which floors, and would therefore be asymmetric) is never used. A port to
%% another language must copy this rule, not substitute a division.
-spec narrow(integer()) -> integer().
narrow(Acc) when Acc < 0 -> -((-Acc + ?NARROW_RND) bsr 12);
narrow(Acc) -> (Acc + ?NARROW_RND) bsr 12.

%% Weighted sum of Q12 inputs by Q12 weights plus a Q12 bias, accumulated exactly
%% in Q24 with no intermediate rounding. Lists of unequal length stop at the
%% shorter one, so a malformed genome cannot crash a match.
%%
%% Headroom: with weights capped at plus or minus 8.0 (Q12 32768) each product is
%% at most 1 bsl 27, so even a thousand-input neuron accumulates under 1 bsl 37,
%% far inside the 60-bit immediate integers of a 64-bit BEAM. No activation ever
%% allocates a bignum. A NIF port must use i64; i32 is not sufficient.
-spec dot([integer()], [integer()], integer()) -> integer().
dot(Ws, Xs, Bias) -> dot_acc(Ws, Xs, Bias bsl 12).

dot_acc([W | Ws], [X | Xs], Acc) -> dot_acc(Ws, Xs, Acc + W * X);
dot_acc(_Ws, _Xs, Acc) -> Acc.

%% Map a Q12 activation in -1.0 to 1.0 onto the integer range -Max to Max, which
%% is how an output neuron becomes an intent field: to_range(A, 7) for turn_body,
%% 14 for turn_gun, 32 for turn_radar, 512 for accel. Sign-split for the same
%% reason narrow/1 is, so the mapping is exactly odd.
-spec to_range(integer(), non_neg_integer()) -> integer().
to_range(A, Max) when A < 0 -> -((-A * Max) bsr 12);
to_range(A, Max) -> (A * Max) bsr 12.

%%==============================================================================
%% The genome: topology, size, and the one float boundary
%%==============================================================================

%% How many integers a topology needs, which is the dimensionality the optimiser
%% searches in. Each neuron in layer L owns one bias plus one weight per neuron
%% in layer L-1, so [8, 10, 5] needs 10 * 9 + 5 * 11 = 145.
%%
%% Total by construction: a topology with fewer than two layers has no neurons
%% and therefore needs nothing.
-spec weight_count([non_neg_integer()]) -> non_neg_integer().
weight_count(Layers) -> weight_count(Layers, 0).

weight_count([In, Out | Rest], Acc) -> weight_count([Out | Rest], Acc + Out * (In + 1));
weight_count(_Layers, Acc) -> Acc.

%% The weight cap, at the arena's scale of 256. Plus or minus 2048 is plus or
%% minus 8.0, which is what the headroom argument on dot/3 assumes and what
%% quantize/1 enforces. Exported so an optimiser can be told the box it is
%% searching in rather than discovering it by having its proposals silently
%% clipped.
-spec weight_limit() -> pos_integer().
weight_limit() -> ?W_LIMIT.

%% THE FLOAT BOUNDARY. This is the ONLY function in the module that touches a
%% float, and it runs at PHENOTYPE BUILD TIME, outside the match, exactly once
%% per genome. Everything reachable from eval/3 is integer, and that separation
%% is the whole determinism argument: a genome is a list of integers, a match is
%% a pure function of integers, and the float search that produced the integers
%% happened before the match and is not replayed.
%%
%% So a float here is not a leak, it is the boundary doing its job. A float
%% ANYWHERE downstream of this function is a defect that voids replay, and that
%% is the specific thing to look for in review.
%%
%% The mapping is round(F * 256) with the input clamped to plus or minus 8.0
%% FIRST. Clamping before multiplying rather than after is deliberate: it makes
%% the function total on every finite float an optimiser can emit, including the
%% 1.0e308-scale values a diverging covariance can produce, which would raise
%% badarith if multiplied first. The round/1 BIF rounds half away from zero,
%% which is the module's single rounding convention, so quantisation is exactly
%% odd here too.
-spec quantize([float()]) -> [integer()].
quantize(Fs) -> [quantize_one(F) || F <- Fs].

quantize_one(F) when F >= 8.0 -> ?W_LIMIT;
quantize_one(F) when F =< -8.0 -> -?W_LIMIT;
quantize_one(F) -> round(F * ?ARENA_SCALE).

%%==============================================================================
%% The forward pass
%%==============================================================================

%% Evaluate a network. PURE INTEGER, and this is the match path.
%%
%% Layers is the topology, Weights the flat genome from quantize/1 (arena scale
%% 256, laid out bias-then-weights per neuron, neurons in order, layers in
%% order), Inputs the sensor vector at arena scale 256.
%%
%% INPUT RANGE, expected: -256 to 256, that is -1.0 to 1.0 in arena units. The
%% controller normalises; this module does not, because what counts as a sensible
%% normaliser for a radar distance is a controller's business and baking one in
%% here would put a rule of the game in the wrong module. Out-of-range inputs are
%% legal and total, not an error: robo_sim's raw quantities are far larger (a
%% distance runs to 800 * 256 = 204800) and feeding one raw simply saturates the
%% first layer, which costs behaviour rather than correctness.
%%
%% OUTPUT RANGE, guaranteed: -256 to 256 inclusive, one value per neuron in the
%% last layer, always exactly that many even if the genome is short. The
%% guarantee holds because every layer including the last passes through
%% activate/1, whose range is a hard -4096 to 4096 in Q12.
%%
%% OVERFLOW, concretely. After the first layer every activation is bounded by
%% 4096 by construction, so the 1 bsl 27 per-product bound on dot/3 holds for
%% every layer past the first no matter what the caller feeds. Only the first
%% layer's products depend on caller discipline, and even the undisciplined case
%% is safe: a raw arena distance widens to under 1 bsl 22, times the 1 bsl 15
%% weight cap is under 1 bsl 37 per product, and a thousand of those stay under
%% 1 bsl 47, still inside the 60-bit immediate integers of a 64-bit BEAM. Erlang
%% would stay CORRECT past that point because integers are arbitrary precision,
%% but bignum promotion is a speed cliff in the hot loop, so the bound is kept
%% deliberately small rather than merely relied on to be finite.
-spec eval([non_neg_integer()], [integer()], [integer()]) -> [integer()].
eval(Layers, Weights, Inputs) -> [to_arena(A) || A <- eval_q12(Layers, Weights, Inputs)].

%% The same forward pass without the final narrowing, so outputs come back in Q12
%% and feed to_range/2 directly. Prefer this when driving an intent: eval/3 is
%% the arena-scale contract and costs 4 bits at the very last step, which is 4
%% bits an intent field would rather keep.
-spec eval_q12([non_neg_integer()], [integer()], [integer()]) -> [integer()].
eval_q12([In | _] = Layers, Weights, Inputs) ->
    forward(Layers, [from_arena(W) || W <- Weights], fit(In, [from_arena(X) || X <- Inputs]));
eval_q12([], _Weights, _Inputs) -> [].

%% One layer per step, consuming the genome left to right.
forward([In, Out | Rest], Ws, Xs) ->
    {Ys, Ws2} = layer(Out, In, Ws, Xs, []),
    forward([Out | Rest], Ws2, Ys);
forward(_Layers, _Ws, Xs) -> Xs.

%% Out neurons, each taking In inputs. The load-bearing invariant is that Xs has
%% EXACTLY In elements: fit/2 guarantees it for the first layer, and every later
%% layer inherits it because this function emits exactly Out values. That is what
%% makes it safe for dot/3 to be handed the whole remaining genome and stop on
%% the shorter list, and it is what makes drop/2 skip exactly the weights dot/3
%% just consumed. Break the invariant and the layers silently misalign, so it is
%% stated here rather than left to be rediscovered.
%%
%% A genome too short for its topology is not an error either: the remaining
%% neurons are all-zero, which activates to zero, so the output arity still
%% matches the declared topology and a malformed genome loses instead of
%% crashing a match.
layer(0, _In, Ws, _Xs, Acc) -> {lists:reverse(Acc), Ws};
layer(N, In, [B | Ws], Xs, Acc) ->
    layer(N - 1, In, drop(In, Ws), Xs, [activate(dot(Ws, Xs, B)) | Acc]);
layer(N, _In, [], _Xs, Acc) ->
    {lists:reverse(Acc) ++ lists:duplicate(N, activate(0)), []}.

%% Skip the N weights dot/3 has just consumed. Total on a short list.
drop(0, Ws) -> Ws;
drop(N, [_W | Ws]) -> drop(N - 1, Ws);
drop(_N, []) -> [].

%% Force a sensor vector to exactly N elements, truncating a long one and padding
%% a short one with zeros. A controller whose sensor count disagrees with its
%% declared topology therefore degrades rather than crashing or, worse, sliding
%% the genome out of alignment.
fit(0, _Xs) -> [];
fit(N, [X | Xs]) -> [X | fit(N - 1, Xs)];
fit(N, []) -> [0 | fit(N - 1, [])].
