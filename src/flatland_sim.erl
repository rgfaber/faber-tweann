%% @doc Flatland: a minimal 2D artificial-life world engine (pure, process-free).
%%
%% The instrument for the Flatland / ALife convergence front (faber-ecosystem
%% PLAN_FLATLAND.md, exp058). A toroidal WxW grid holding plants (food cells) and
%% avatars (evolved networks). This module is PURE physics -- sensing, movement,
%% distance, nearest-object -- with no episode loop, no energy accounting and no
%% randomness; the caller (the exp058 runner, later a `scape' adapter) owns those.
%%
%% Held IDENTICAL to the 057 pursuit-evasion grid on purpose (DESIGN gate, 2026-07-23):
%% same 9x9 torus, same 4-way argmax movement, same shortest-wrap RELATIVE-POSITION
%% sensing modality (NOT DXNN2 range/ray sensors -- deferred as a confound). The ONLY
%% thing Flatland adds over 057 is the energy economy (plants + eating + death), which
%% lives in the caller. So a later coupling difference is attributable to the ecology,
%% not the sensing.
%%
%% Sensor vector (5 floats), same modality for every avatar:
%%   [OppDX, OppDY, PlantDX, PlantDY, EnergyFrac]
%% where Opp* is the shortest-wrap signed delta to the opponent (0 if none),
%% Plant* the delta to the nearest plant (0 if none), each normalised by W div 2,
%% and EnergyFrac = Energy / E0.
%%
%% Move codes: 0=North (y-1), 1=East (x+1), 2=South (y+1), 3=West (x-1).
%% @end
-module(flatland_sim).

-export([wrap/2, tdelta/3, cheb/3, nearest/3, move/3, sense/6, toward/3, away/3]).

-type pos() :: {integer(), integer()}.

%% Wrap a coordinate onto the torus [0, W).
-spec wrap(integer(), pos_integer()) -> integer().
wrap(X, W) -> ((X rem W) + W) rem W.

%% Shortest-wrap signed delta from A to B along one axis, in [-(W div 2), W div 2].
-spec tdelta(integer(), integer(), pos_integer()) -> integer().
tdelta(A, B, W) ->
    D = (((B - A) rem W) + W) rem W,
    shortest(D, W).

shortest(D, W) when D > W div 2 -> D - W;
shortest(D, _W) -> D.

%% Torus Chebyshev distance between two positions.
-spec cheb(pos(), pos(), pos_integer()) -> non_neg_integer().
cheb({X1, Y1}, {X2, Y2}, W) ->
    max(abs(tdelta(X1, X2, W)), abs(tdelta(Y1, Y2, W))).

%% The cell in Cells nearest (torus Chebyshev) to Pos; `none' if Cells is empty.
-spec nearest(pos(), [pos()], pos_integer()) -> pos() | none.
nearest(_Pos, [], _W) -> none;
nearest(Pos, Cells, W) ->
    {_D, Best} = lists:min([{cheb(Pos, C, W), C} || C <- Cells]),
    Best.

%% Apply a move code to a position (wrapped).
-spec move(pos(), 0..3, pos_integer()) -> pos().
move({X, Y}, 0, W) -> {X, wrap(Y - 1, W)};
move({X, Y}, 1, W) -> {wrap(X + 1, W), Y};
move({X, Y}, 2, W) -> {X, wrap(Y + 1, W)};
move({X, Y}, 3, W) -> {wrap(X - 1, W), Y}.

%% Build the 5-float sensor vector for an avatar. Opp = none for a solo forager.
-spec sense(pos_integer(), pos(), pos() | none, [pos()], number(), number()) -> [float()].
sense(W, {SX, SY} = Self, Opp, Plants, Energy, E0) ->
    N = max(1, W div 2),
    {ODX, ODY} = case Opp of
                     none -> {0, 0};
                     {OX, OY} -> {tdelta(SX, OX, W), tdelta(SY, OY, W)}
                 end,
    {PDX, PDY} = case nearest(Self, Plants, W) of
                     none -> {0, 0};
                     {PX, PY} -> {tdelta(SX, PX, W), tdelta(SY, PY, W)}
                 end,
    [ODX / N, ODY / N, PDX / N, PDY / N, Energy / E0].

%% Hand-coded policy helper: the move that most reduces torus distance to Target.
-spec toward(pos(), pos(), pos_integer()) -> 0..3.
toward({SX, SY}, {TX, TY}, W) ->
    DX = tdelta(SX, TX, W), DY = tdelta(SY, TY, W),
    axis_move(DX, DY).

%% The move that most increases torus distance to Threat (flee).
-spec away(pos(), pos(), pos_integer()) -> 0..3.
away({SX, SY}, {TX, TY}, W) ->
    DX = tdelta(SX, TX, W), DY = tdelta(SY, TY, W),
    opposite(axis_move(DX, DY)).

%% Step along the axis with the larger absolute delta (ties -> x).
axis_move(DX, DY) when abs(DX) >= abs(DY), DX > 0 -> 1;   %% East
axis_move(DX, DY) when abs(DX) >= abs(DY), DX < 0 -> 3;   %% West
axis_move(_DX, DY) when DY > 0 -> 2;                      %% South
axis_move(_DX, DY) when DY < 0 -> 0;                      %% North
axis_move(_DX, _DY) -> 0.                                 %% coincident -> arbitrary

opposite(0) -> 2;
opposite(2) -> 0;
opposite(1) -> 3;
opposite(3) -> 1.
