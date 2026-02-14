%% @doc Activation and utility functions for neural computation.
%%
%% This module provides activation functions used by neurons to transform
%% aggregated input signals into output signals, plus utility functions
%% for saturation and scaling.
%%
%% Based on DXNN2 by Gene Sher ("Handbook of Neuroevolution through Erlang").
%%
%% == Activation Functions ==
%%
%% Monotonic:
%%
%% - `tanh' - Hyperbolic tangent, smooth, range [-1, 1]
%%
%% - `sigmoid' - Logistic function, range [0, 1]
%%
%% - `sigmoid1' - Alternative sigmoid, range [-1, 1]
%%
%% - `linear' - Identity function (no transformation)
%%
%% Periodic:
%%
%% - `sin' - Sine function
%%
%% - `cos' - Cosine function
%%
%% Radial Basis:
%%
%% - `gaussian' - Bell curve, peaks at 0
%%
%% - `multiquadric' - sqrt(x^2 + c)
%%
%% Threshold:
%%
%% - `sgn' - Sign function {-1, 0, 1}
%%
%% - `bin' - Binary threshold {0, 1}
%%
%% - `trinary' - Three-level output {-1, 0, 1}
%%
%% Other:
%%
%% - `absolute' - Absolute value
%%
%% - `quadratic' - Signed square
%%
%% - `sqrt' - Signed square root
%%
%% - `log' - Signed logarithm
%%
%% == Extensions (not in original DXNN2) ==
%%
%% - `cubic' - Cube function x^3
%%
%% - `relu' - Rectified Linear Unit max(0, x)
%%
%% == Utility Functions ==
%%
%% - `saturation/1,2' - Clamp values to prevent overflow
%%
%% - `sat/3' - Clamp to [min, max] range
%%
%% - `sat_dzone/5' - Saturation with dead zone
%%
%% - `scale/3,5' - Scale values between ranges
%%
%% @author R.G. Lefever
%% @copyright 2024-2026 R.G. Lefever, Apache-2.0

-module(functions).

-export([
    %% Activation functions
    tanh/1,
    sin/1,
    cos/1,
    gaussian/1,
    gaussian/2,
    sigmoid/1,
    sigmoid1/1,
    sgn/1,
    bin/1,
    trinary/1,
    multiquadric/1,
    absolute/1,
    linear/1,
    quadratic/1,
    cubic/1,
    sqrt/1,
    log/1,
    relu/1,

    %% Utility functions
    saturation/1,
    saturation/2,
    sat/3,
    sat_dzone/5,
    scale/3,
    scale/5,

    %% Statistics
    avg/1,
    std/1
]).

%%==============================================================================
%% Activation Functions
%%==============================================================================

%% @doc Hyperbolic tangent activation function
%%
%% Maps input to range [-1, 1] with smooth gradient.
%% Mathematical definition: tanh(x) = (e^x - e^-x) / (e^x + e^-x)
%%
%% Properties:
%% - Output range: [-1, 1]
%% - tanh(0) = 0
%% - Smooth derivative (good for learning)
%% - Most commonly used activation in neuroevolution
%%
%% @param Val the input signal value
%% @returns output in range [-1, 1]
-spec tanh(float()) -> float().
tanh(Val) ->
    math:tanh(Val).

%% @doc Sine activation function
%%
%% Periodic function with range [-1, 1].
%% Useful for oscillatory patterns and fourier-like representations.
%%
%% @param Val the input signal value
%% @returns output in range [-1, 1]
-spec sin(float()) -> float().
sin(Val) ->
    math:sin(Val).

%% @doc Cosine activation function
%%
%% Periodic function with range [-1, 1].
%% cos(0) = 1, phase-shifted from sine.
%%
%% @param Val the input signal value
%% @returns output in range [-1, 1]
-spec cos(float()) -> float().
cos(Val) ->
    math:cos(Val).

%% @doc Gaussian (radial basis) activation function
%%
%% Bell curve centered at 0, peaks at 1.0, decays towards 0.
%% Mathematical definition: e^(-x^2)
%%
%% Input is clamped to [-10, 10] to prevent underflow.
%%
%% Properties:
%% - Output range: (0, 1]
%% - gaussian(0) = 1
%% - Radially symmetric
%%
%% @param Val the input signal value
%% @returns output in range (0, 1]
-spec gaussian(float()) -> float().
gaussian(Val) ->
    gaussian(2.71828183, Val).

%% @doc Gaussian with custom base
%%
%% @param Base the exponential base (typically e = 2.71828183)
%% @param Val the input signal value
%% @returns output value
-spec gaussian(float(), float()) -> float().
gaussian(Base, Val) ->
    V = clamp(Val, -10.0, 10.0),
    math:pow(Base, -V * V).

%% @doc Sigmoid (logistic) activation function
%%
%% S-shaped curve mapping to range [0, 1].
%% Mathematical definition: 1 / (1 + e^-x)
%%
%% Input is clamped to [-10, 10] to prevent overflow.
%%
%% Properties:
%% - Output range: (0, 1)
%% - sigmoid(0) = 0.5
%% - Derivative: y * (1 - y)
%%
%% @param Val the input signal value
%% @returns output in range (0, 1)
-spec sigmoid(float()) -> float().
sigmoid(Val) ->
    V = clamp(Val, -10.0, 10.0),
    1.0 / (1.0 + math:exp(-V)).

%% @doc Alternative sigmoid activation function
%%
%% Maps to range [-1, 1] using: x / (1 + |x|)
%%
%% Properties:
%% - Output range: (-1, 1)
%% - sigmoid1(0) = 0
%% - Faster to compute than standard sigmoid
%% - Derivative: 1 / (1 + |x|)^2
%%
%% @param Val the input signal value
%% @returns output in range (-1, 1)
-spec sigmoid1(float()) -> float().
sigmoid1(Val) ->
    Val / (1.0 + abs(Val)).

%% @doc Sign function
%%
%% Returns the sign of the input value.
%%
%% @param Val the input signal value
%% @returns -1, 0, or 1
-spec sgn(number()) -> -1 | 0 | 1.
sgn(0) -> 0;
sgn(+0.0) -> 0;
sgn(Val) when Val > 0 -> 1;
sgn(_Val) -> -1.

%% @doc Binary threshold function
%%
%% Returns 1 for positive input, 0 otherwise.
%%
%% @param Val the input signal value
%% @returns 0 or 1
-spec bin(number()) -> 0 | 1.
bin(Val) when Val > 0 -> 1;
bin(_Val) -> 0.

%% @doc Trinary threshold function
%%
%% Returns -1, 0, or 1 based on threshold of 0.33.
%%
%% @param Val the input signal value
%% @returns -1, 0, or 1
-spec trinary(float()) -> -1 | 0 | 1.
trinary(Val) when Val < 0.33, Val > -0.33 -> 0;
trinary(Val) when Val >= 0.33 -> 1;
trinary(_Val) -> -1.

%% @doc Multiquadric activation function
%%
%% Mathematical definition: sqrt(x^2 + 0.01)
%% Always positive, smooth at origin.
%%
%% @param Val the input signal value
%% @returns output value >= 0.1
-spec multiquadric(float()) -> float().
multiquadric(Val) ->
    math:pow(Val * Val + 0.01, 0.5).

%% @doc Absolute value activation function
%%
%% @param Val the input signal value
%% @returns |Val|
-spec absolute(number()) -> number().
absolute(Val) ->
    abs(Val).

%% @doc Linear (identity) activation function
%%
%% No transformation - output equals input.
%% Used for output neurons when raw values are needed.
%%
%% @param Val the input signal value
%% @returns Val unchanged
-spec linear(float()) -> float().
linear(Val) ->
    Val.

%% @doc Quadratic activation function
%%
%% Signed square: sgn(x) * x^2
%% Preserves sign while amplifying magnitude.
%%
%% @param Val the input signal value
%% @returns signed square of input
-spec quadratic(float()) -> float().
quadratic(Val) ->
    sgn(Val) * Val * Val.

%% @doc Cubic activation function
%%
%% Signed cube: x^3
%% Preserves sign while strongly amplifying magnitude.
%%
%% @param Val the input signal value
%% @returns cube of input
-spec cubic(float()) -> float().
cubic(Val) ->
    Val * Val * Val.

%% @doc Square root activation function
%%
%% Signed square root: sgn(x) * sqrt(|x|)
%% Compresses magnitude while preserving sign.
%%
%% @param Val the input signal value
%% @returns signed square root
-spec sqrt(float()) -> float().
sqrt(Val) ->
    sgn(Val) * math:sqrt(abs(Val)).

%% @doc Logarithm activation function
%%
%% Signed logarithm: sgn(x) * ln(|x|)
%% Handles zero input specially.
%%
%% @param Val the input signal value
%% @returns signed natural logarithm
-spec log(number()) -> float().
log(0) -> 0.0;
log(+0.0) -> 0.0;
log(Val) -> sgn(Val) * math:log(abs(Val)).

%% @doc Rectified Linear Unit (ReLU) activation function
%%
%% Returns max(0, x).
%% Popular in deep learning for its simplicity and effectiveness.
%%
%% @param Val the input signal value
%% @returns max(0, Val)
-spec relu(float()) -> float().
relu(Val) when Val > 0 -> Val;
relu(_Val) -> 0.0.

%%==============================================================================
%% Utility Functions
%%==============================================================================

%% @doc Clamp value to default range [-1000, 1000]
%%
%% Prevents numerical overflow in subsequent calculations.
%%
%% @param Val the input value
%% @returns clamped value
-spec saturation(number()) -> number().
saturation(Val) when Val > 1000 -> 1000;
saturation(Val) when Val < -1000 -> -1000;
saturation(Val) -> Val.

%% @doc Clamp value to symmetric range [-Spread, Spread]
%%
%% @param Val the input value
%% @param Spread the maximum absolute value
%% @returns clamped value
-spec saturation(number(), number()) -> number().
saturation(Val, Spread) when Val > Spread -> Spread;
saturation(Val, Spread) when Val < -Spread -> -Spread;
saturation(Val, _Spread) -> Val.

%% @doc Clamp value to range [Min, Max]
%%
%% @param Val the input value
%% @param Max the maximum allowed value
%% @param Min the minimum allowed value
%% @returns clamped value
-spec sat(number(), number(), number()) -> number().
sat(Val, Max, _Min) when Val > Max -> Max;
sat(Val, _Max, Min) when Val < Min -> Min;
sat(Val, _Max, _Min) -> Val.

%% @doc Clamp value with dead zone
%%
%% Values within the dead zone [DZMin, DZMax] are set to 0.
%% Values outside are clamped to [Min, Max].
%%
%% @param Val the input value
%% @param Max the maximum allowed value
%% @param Min the minimum allowed value
%% @param DZMax the dead zone upper bound
%% @param DZMin the dead zone lower bound
%% @returns processed value
-spec sat_dzone(number(), number(), number(), number(), number()) -> number().
sat_dzone(Val, _Max, _Min, DZMax, DZMin) when Val < DZMax, Val > DZMin -> 0;
sat_dzone(Val, Max, Min, _DZMax, _DZMin) -> sat(Val, Max, Min).

%% @doc Scale list or value from one range to [-1, 1]
%%
%% Normalizes values using: (Val*2 - (Max + Min)) / (Max - Min)
%%
%% @param ValOrList the input value or list of values
%% @param Max the maximum of the input range
%% @param Min the minimum of the input range
%% @returns scaled value(s) in range [-1, 1]
-spec scale([number()] | number(), number(), number()) -> [float()] | float().
scale([H | T], Max, Min) ->
    [scale(Val, Max, Min) || Val <- [H | T]];
scale(_Val, Max, Min) when Max == Min ->
    0.0;
scale(Val, Max, Min) ->
    (Val * 2 - (Max + Min)) / (Max - Min).

%% @doc Scale value from one range to another
%%
%% Linear interpolation from [FromMin, FromMax] to [ToMin, ToMax].
%%
%% @param Val the input value
%% @param FromMin minimum of input range
%% @param FromMax maximum of input range
%% @param ToMin minimum of output range
%% @param ToMax maximum of output range
%% @returns scaled value
-spec scale(number(), number(), number(), number(), number()) -> float().
scale(Val, FromMin, FromMax, ToMin, ToMax) ->
    Normalized = (Val - FromMin) / (FromMax - FromMin),
    ToMin + Normalized * (ToMax - ToMin).

%%==============================================================================
%% Statistics Functions
%%==============================================================================

%% @doc Calculate average of a list
%%
%% @param List list of numbers
%% @returns arithmetic mean
-spec avg([number()]) -> float().
avg(List) ->
    lists:sum(List) / length(List).

%% @doc Calculate standard deviation of a list
%%
%% @param List list of numbers
%% @returns standard deviation
-spec std([number()]) -> float().
std(List) ->
    Avg = avg(List),
    Variance = lists:sum([math:pow(Avg - Val, 2) || Val <- List]) / length(List),
    math:sqrt(Variance).

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @private
%% @doc Clamp value to range
-spec clamp(number(), number(), number()) -> number().
clamp(Val, Min, _Max) when Val < Min -> Min;
clamp(Val, _Min, Max) when Val > Max -> Max;
clamp(Val, _Min, _Max) -> Val.
