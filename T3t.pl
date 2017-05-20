

:- dynamic ttc/0.
:- dynamic longform.

ttimeLimit(5). % in seconds, per test

longform. % keep this for verbose test output.

% Test using:
% ttestAll -- runs all tests
% ttestOne(+TestID) -- runs 1 test
% ttestOneFree(+TestID) -- runs 1 test, with no time limit

tt :- ttestAll.

% test general
ttpoints(T, 2.5) :- member(T, [11,12,13,14]).
ttpoints('21a'/_/_/_/_, 0.25).
ttpoints('21b'/_/_/_/_, 0.25).
ttpoints('21c'/_/_/_/_, 0.25).
ttpoints(22, 15.0).
ttpoints(31/_/_/_/_, 0.2).
ttpoints(32/_/_/_/_, 0.2).
ttpoints(41/_, 15).
ttpoints(X, 2) :- member(X, [50, 51, 52, 53, 54, 55, 56, 57, 58, 59]).

tte(M, Test, Vars) :- \+ longform -> fail ;
    format("~15+ Test ~w error: ", [Test]),
    format(M, Vars), writeln("."), !, fail.
tte(M, Test) :- tte(M, Test, []).

% test elements

% tests 21a/*
tt_moves_1([
    (1, 1, '#7', 'R0'), (1, 1, '#7', 'R2'), (1, 1, '#8', 'R0'),
    (2, 1, '#2', 'R0'), (2, 1, '#2', 'R1'), (3, 1, '#5', 'R0'),
    (4, 1, '#5', 'R2'), (8, 1, '#7', 'R1'), (8, 1, '#7', 'R3'),
    (8, 1, '#8', 'R1'), (1, 8, '#7', 'R1'), (1, 8, '#9', 'R0'),
    (1, 6, '#6', 'R0'), (1, 6, '#9', 'R0'), (1, 6, '#10', 'R0')
    ]). % 15

% tests 21b/*
tt_moves_2([
    (1, 1, '#1', 'R0'), (1, 1, '#1', 'R1'), (1, 1, '#1', 'R2'), (1, 1, '#1', 'R3'),
    (1, 1, '#2', 'R0'), (1, 1, '#2', 'R1'), (1, 1, '#2', 'R2'), (1, 1, '#2', 'R3'),
    (1, 1, '#3', 'R0'), (1, 1, '#3', 'R1'), (1, 1, '#3', 'R2'), (1, 1, '#3', 'R3'),
    (1, 1, '#4', 'R0'), (1, 1, '#5', 'R1'), (1, 1, '#7', 'R3'), (1, 1, '#8', 'R1'),
    (1, 1, '#8', 'R3'), (1, 1, '#7', 'R1'), (1, 1, '#10', 'R0'), (1, 1, '#10', 'R1'),
    (2, 1, '#1', 'R0'), (2, 1, '#4', 'R1'), (2, 1, '#5', 'R3'), (7, 1, '#1', 'R0'),
    (1, 2, '#5', 'R0'), (1, 2, '#2', 'R0'), (1, 2, '#2', 'R3'),
    (8, 1, '#7', 'R0'), (8, 8, '#7', 'R1'), (8, 8, '#7', 'R3')
    ]). % 30

% tests 21c/*
tt_moves_3(L) :- findall((A, B, I, J), (between(2, 7, A), between(2, 7, B),
                                   tile(_, _, _, _, I), rotation(_, J)), LI),
    tt_SR(LI, 15, L). % 15

% tests 31/* and 32/*
tt_1moves([
    (1, 1, '#7', 'R0', [0, 1, 1, 0]),
    (1, 1, '#7', 'R2', [0, 1, 1, 0]),
    (1, 1, '#8', 'R0', [0, 1, 1, 0]),
    (1, 1, '#9', 'R0', [0, 1, 1, 0]),
    (8, 1, '#7', 'R1', [9, 1, 8, 0]),
    (8, 1, '#8', 'R1', [9, 1, 8, 0]),
    (8, 1, '#9', 'R0', [9, 1, 8, 0]),
    (8, 8, '#7', 'R0', [9, 8, 8, 9]),
    (8, 8, '#7', 'R2', [9, 8, 8, 9]),
    (8, 8, '#8', 'R0', [9, 8, 8, 9]),
    (3, 1, '#2', 'R0', [3, 0]),
    (4, 1, '#3', 'R0', [4, 0]),
    (5, 1, '#4', 'R0', [5, 0]),
    (3, 1, '#3', 'R1', [3, 0]),
    (1, 7, '#4', 'R1', [0, 7]),
    (1, 4, '#5', 'R2', [0, 4]),
    (1, 4, '#5', 'R3', [0, 4]),
    (8, 5, '#6', 'R0', [9, 5]),
    (8, 2, '#7', 'R0', [9, 2]),
    (8, 2, '#7', 'R1', [9, 2]),
    (8, 4, '#5', 'R1', [9, 4]),
    (7, 8, '#8', 'R0', [7, 9]),
    (5, 8, '#9', 'R0', [5, 9]),
    (5, 8, '#8', 'R1', [5, 9]),
    (5, 8, '#10', 'R0', [5, 9])
    ]). %25

% tests 41/*
tt_move_series(TT, MVS, Op, Cl, MT) :-
    member((TT, MVS, Op, Cl, MT), [
               (   1, [2/1/'#5'/'R0',
                       2/2/'#7'/'R2',
                       3/2/'#2'/'R1',
                       2/3/'#3'/'R2',
                       3/1/'#6'/'R0',
                       1/3/'#9'/'R0'
                      ], [
                          (0/3, ['#9'/'R0', '#3'/'R2'])
                      ], [
                          (2/0, ['#5'/'R0', '#6'/'R0',
                                 '#2'/'R1', '#7'/'R2',
                                 '#5'/'R0']),
                          (3/0, ['#6'/'R0', '#5'/'R0',
                                 '#7'/'R2', '#3'/'R2',
                                 '#9'/'R0'])
                      ], 3*24-3*18),
               (   2, [8/1/'#7'/'R1',
                       8/2/'#5'/'R1',
                       8/3/'#7'/'R1',
                       8/4/'#8'/'R0',
                       8/5/'#7'/'R0',
                       8/6/'#2'/'R0',
                       8/7/'#8'/'R0',
                       8/8/'#8'/'R0',

                       7/1/'#7'/'R3',
                       7/2/'#3'/'R3',
                       7/3/'#5'/'R2',
                       7/4/'#7'/'R3',
                       7/5/'#5'/'R1',
                       7/6/'#10'/'R0',
                       7/7/'#7'/'R2',
                       7/8/'#5'/'R3',

                       6/1/'#7'/'R2',
                       6/2/'#9'/'R0',
                       6/3/'#4'/'R1',
                       6/4/'#3'/'R1',
                       6/5/'#2'/'R3',
                       6/6/'#1'/'R0',
                       6/7/'#7'/'R3',
                       6/8/'#3'/'R2',

                       5/1/'#4'/'R0',
                       5/2/'#2'/'R0',
                       5/3/'#10'/'R0',
                       5/6/'#7'/'R1',
                       5/7/'#6'/'R0',
                       5/8/'#5'/'R2',

                       4/1/'#3'/'R0',
                       4/2/'#7'/'R0',
                       4/3/'#7'/'R0',
                       4/6/'#6'/'R0',
                       4/7/'#5'/'R0',
                       4/8/'#3'/'R2'
                      ], [
                          (4/0, ['#3'/'R0', '#7'/'R0', '#2'/'R0', '#7'/'R0'])
                      ], [
(5/0,['#4'/'R0','#2'/'R0','#9'/'R0','#3'/'R3','#5'/'R2','#7'/'R3','#3'/'R1','#4'/'R1','#3'/'R1']),
(6/0,['#7'/'R2','#9'/'R0','#4'/'R1','#9'/'R0','#7'/'R2','#4'/'R0','#7'/'R2','#7'/'R3','#7'/'R1']),
(7/0,['#7'/'R3','#7'/'R2']),
(8/0,['#7'/'R1','#5'/'R1']),
(4/9,['#3'/'R2','#5'/'R0','#6'/'R0','#7'/'R1']),
(5/9,['#5'/'R2','#3'/'R2','#5'/'R2','#6'/'R0','#7'/'R3','#7'/'R2','#8'/'R0','#8'/'R0']),
(6/9,['#3'/'R2','#7'/'R3','#1'/'R0','#7'/'R3','#6'/'R0','#7'/'R1','#1'/'R0','#7'/'R1','#6'/'R0']),
(7/9,['#5'/'R3','#3'/'R2','#5'/'R3','#8'/'R0']),
(8/9,['#8'/'R0','#5'/'R3']),
(9/1,['#7'/'R1','#7'/'R3','#3'/'R3','#7'/'R3']),
(9/2,['#5'/'R1','#3'/'R3','#9'/'R0','#2'/'R0','#4'/'R0']),
(9/3,['#7'/'R1','#5'/'R2','#7'/'R1','#5'/'R1','#7'/'R1','#8'/'R0']),
(9/4,['#8'/'R0','#7'/'R1']),
(9/5,['#7'/'R0','#5'/'R1','#2'/'R3','#1'/'R0','#2'/'R3','#5'/'R1','#7'/'R3','#5'/'R2','#4'/'R1','#10'/'R0']),
(9/6,['#2'/'R0','#7'/'R0','#8'/'R0','#7'/'R3','#5'/'R1','#7'/'R0','#2'/'R0']),
(9/7,['#8'/'R0','#2'/'R0','#8'/'R0']),
(9/8,['#8'/'R0','#8'/'R0','#7'/'R2','#10'/'R0','#1'/'R0','#10'/'R0','#5'/'R1','#10'/'R0','#2'/'R0','#10'/'R0','#7'/'R2','#5'/'R3','#7'/'R2','#7'/'R3','#3'/'R2','#5'/'R2','#3'/'R2'])
                      ], 24*4-14*18-2*4)
           ]).

% random subset of size N
tt_SR(_, 0, []) :- !.
tt_SR(L, N, [E|O]) :- N1 is N - 1, length(L, LL), R is random(LL),
    nth0(R, L, E), tt_SR(L, N1, O).


% builders / getters
tt_get_build_move(M, Move, Dir, TT) :-
    M = X/Y/T/R,
    (   get_move_space(Move, (X, Y)), ! ;
           tte("Unable to ~w move space: ~w", TT, [Dir, (X, Y)])),
    (   get_move_tile_id(Move, T), ! ;
           tte("Unable to ~w move tile id: ~w", TT, [Dir, T])),
    (   get_move_rotation_id(Move, R), ! ;
           tte("Unable to ~w move rotation: ~w", TT, [Dir, R])).

tt_get_game_components(GS, Tiles, Open, Closed, TT):-
    (   get_game_tiles(GS, Tiles), !; tte("Unable to retrieve game tiles", TT)),
    (   is_list(Tiles), !; tte("Tile list is not a list: ~w", TT, [Tiles])),
    (   get_closed_paths(GS, Closed), !; tte("Unable to retrieve closed paths", TT)),
    (   is_list(Closed), !; tte("Closed paths list is not a list: ~w", TT, [Closed])),
    (   get_open_paths(GS, Open), !; tte("Unable to retrieve open paths", TT)),
    (   is_list(Open), !; tte("Open paths list is not a list: ~w", TT, [Open])).

tt_get_path_components(P, E, TS, TT) :-
    (   get_path_entry(P, E), !; tte("Unable to retrieve entry point from ~w", TT, [P])),
    (   get_path_tiles(P, TS), !; tte("Unable to retrieve tiles from path ~w", TT, [P])).

tt_makeMove(GS, X, Y, T, R, GSR, TT) :-
    tt_get_build_move(X/Y/T/R, Move, set, TT),
    (   available_move(GS, Move), !;
               tte("Move ~w should be available but is not", TT, [Move])),
    (   apply_move(GS, Move, GSR), !;
               tte("Apply move ~w in initial state failed", TT, [Move]))
    .


tt_makeMoves([], GSR, GSR, _, _).
tt_makeMoves([M|MVS], GS, GSR, Applied, TT) :-
    tt_get_build_move(M, Move, set, TT),
    (   available_move(GS, Move), ! ;
           tte("Move ~w should be available after applying moves ~w", TT, [M, Applied])),
    (   apply_move(GS, Move, GS2), !;
           tte("Applying move ~w failed after applying moves ~w", TT, [M, Applied])),
    tt_makeMoves(MVS, GS2, GSR, [M|Applied], TT).
tt_makeMoves(MVS, GSR, TT) :-
    (   initial_game_state(GS), ! ; tte("No initial game state", TT)),
%    trace,
    tt_makeMoves(MVS, GS, GSR, [], TT).

tt_testInitialMoves(TT, Var) :- TT = _/X/Y/T/R,
    (   ttc->true;
        (   initial_game_state(GS), ! ; tte("No initial game state", TT)),
        tt_get_build_move(X/Y/T/R, Move, set, TT),
        (   Var == aff ->
            (   available_move(GS, Move), ! ; tte("Move ~w should be available", TT, [TT])) ;
            (   \+ available_move(GS, Move), ! ; tte("Move ~w should not be available", TT, [TT]))
        )
    ).


tt_check_1path(P, X, Y, T, R, TT) :-
    tt_get_path_components(P, E, TS, TT),
    (   E == (X, Y), !; tte("Entry point should be ~w but is ~w", TT, [(X, Y), E])),
    (   TS == [(T, R)], !;
          tte("Path should just have tile ~w but tile list is ~w", TT, [(T, R), TS]))
    .

tt_check_1open(T, R, [A, B, C, D], Open, TT) :-
    (   findall(P, (member(P, Open), get_path_tiles(P, [_])), OpenF), !;
             tte("Unable to filter paths with a single tile from: ~w", TT, [Open])),
    length(OpenF, Len),
    (   Len == 2, !; tte("There should be 2 open paths but there are ~w paths in ~w", TT,
                 [Len, OpenF])),
    OpenF = [P1, P2],
    (   retract(longform), tt_check_1path(P1, A, B, T, R, TT), tt_check_1path(P2, C, D, T, R, TT), !
    ;   ( tt_check_1path(P2, A, B, T, R, TT), tt_check_1path(P1, C, D, T, R, TT), !;
          assert(longform), tt_check_1path(P1, A, B, T, R, TT), tt_check_1path(P2, C, D, T, R, TT))),
    assert(longform).
tt_check_1open(T, R, [A, B], Open, TT) :-
    (   findall(P, (member(P, Open), get_path_tiles(P, [_])), OpenF), !;
             tte("Unable to filter paths with a single tile from: ~w", TT, [Open])),
    length(OpenF, Len),
    (   Len == 1, !; tte("There should be 1 open path but there are ~w paths in ~w", TT,
                 [Len, OpenF])),
    OpenF = [P1],
    tt_check_1path(P1, A, B, T, R, TT)
    .



% check game components after 1 move
tt_1moveA(TT) :- ttc->true; TT = _/X/Y/T/R,
    tt_1moves(L), member((X, Y, T, R, ES), L),
    (   initial_game_state(GS), ! ; tte("No initial game state", TT)),
    tt_makeMove(GS, X, Y, T, R, GSR, TT),
    % game state
    % tile
    tt_get_game_components(GSR, Tiles, Open, Closed, TT),
    length(Tiles, NT),
    (   NT == 1, !;
          tte("There should be 1 tile in the game but there are ~w tiles in ~w", TT,
              [NT, Tiles])),
    Tiles = [TL],
    (   TL == ((X, Y), T, R), !; tte("Incorrect tile in game: ~w", TT, [TL])),
    % closed paths
    (   Closed == [], !; tte("There should be no closed paths but retrieved ~w", TT, [Closed])),
    % open paths
    tt_check_1open(T, R, ES, Open, TT)
    .

% available moves
tt_1moveBa(GSR, X, Y, TT) :-
    limits(A, B, C, D),
    findall((X1, Y1), (
                (   (X1 is X + 1 ; X1 is X - 1), Y1 = Y ;
                    (Y1 is Y + 1 ; Y1 is Y - 1), X1 = X),
                X1 =< C, X1 >= A, Y1 =< D, Y1 >= B,
                (   get_move_space(M, (X1, Y1)), get_move_tile_id(M, '#9'),
                    get_move_rotation_id(M, 'R0'), !;
                      tte("Unable to construct move for space ~w", TT, (X1, Y1))),
                (   available_move(GSR, M), !;
                      tte("Move #9,R0 should be available for space ~w after adding tile ~w",
                          TT, [(X1, Y1), (X, Y)]))), _).
% not available moves
tt_1moveBb(GSR, X, Y, TT) :-
    limits(A, B, C, D),
    (   get_move_space(M0, (X, Y)), get_move_tile_id(M0, '#9'),
        get_move_rotation_id(M0, 'R0'), !;
           tte("Unable to construct move for space ~w", TT, (X, Y))),
    (   \+ available_move(GSR, M0), !;
        tte("Move should not be available for the same space", TT)),
    findall((X1, Y1), (
                (X1 is X + 1 ; X1 is X - 1),
                (Y1 is Y + 1 ; Y1 is Y - 1),
                X1 < C, X1 > A, Y1 < D, Y1 > B,
                (   get_move_space(M, (X1, Y1)), get_move_tile_id(M, '#9'),
                    get_move_rotation_id(M, 'R0'), !;
                      tte("Unable to construct move for space ~w", TT, (X1, Y1))),
                (   \+ available_move(GSR, M), !;
                      tte("Move #9,R0 should not be available for space ~w after adding tile ~w",
                          TT, [(X1, Y1), (X, Y)]))), _).

tt_1moveB(TT) :- ttc->true; TT = _/X/Y/T/R,
    tt_1moves(L), member((X, Y, T, R, ES), L),
    (   initial_game_state(GS), ! ; tte("No initial game state", TT)),
    tt_makeMove(GS, X, Y, T, R, GSR, TT),
    % available moves fixed good
    tt_1moveBa(GSR, X, Y, TT),

    % available moves fixed bad
    tt_1moveBb(GSR, X, Y, TT),

    % available moves all
    (   length(ES, 4) -> NM = 4; NM is 18 - 24  ), Rem is 448 - NM,
    findall(Move, available_move(GSR, Move), Moves),
    length(Moves, NMoves),
    (   NMoves =:= Rem, !
    ;   tte("Incorrect number ~w of remaining available moves, expected ~w", TT,
            [NMoves, Rem]))
    .

tt_out(_, _, [], _) :- !, fail.
tt_out(FC, E, [EG|R], R) :- call(FC, E, EG), !.
tt_out(FC, E, [H|R], [H|S]) :- tt_out(FC, E, R, S).


tt_pcheck_tiles([], [], _).
tt_pcheck_tiles([TR|Ret], [T/R|Given], TT) :- TR == (T, R), tt_pcheck_tiles(Ret, Given, TT).
tt_pcheck_tiles(Ret, Give, TT) :-
    tte("tiles should be ~w but returned are ~w", TT, [Give, Ret]).

tt_pcheck((X/Y, Tiles), PRet) :-
    tt_get_path_components(PRet, REntry, RTiles, '--'),
    unifiable(REntry, (_, _), _),
% tte("Entry point should be a pair (X, Y), but is ~w", TT, [REntry])),
    REntry = (X, Y),
% tte("Entry point should be ~w, but is ~w", TT, [(X, Y), REntry])),
    tt_pcheck_tiles(RTiles, Tiles, '--').

tt_check_given_tiles([], [], _, _) :- !.
tt_check_given_tiles(Returned, [], _, TT) :- Returned \= [],
    tte("Additional tiles found: ~w", TT, [Returned]).
tt_check_given_tiles(Returned, [M|Given], RR, TT) :- M = X/Y/T/R,
    (   tt_out(==, ((X, Y), T, R), Returned, RetO), ! ;
        tte("Tile equivalent to ~w not found in results: ~w ~n(all results returned: ~w)", TT, [M, Returned, RR])),
    tt_check_given_tiles(RetO, Given, RR, TT).

tt_check_given_closed([], [], _, _).
tt_check_given_closed(Returned, [], _, TT) :- Returned \= [],
    tte("Additional closed paths found: ~w", TT, [Returned]).
tt_check_given_closed(Returned, [P|Given], RR, TT) :-
    (   tt_out(tt_pcheck, P, Returned, RetO), !;
        tte("Closed path equivalent to ~w not found in results: ~w ~n(all results returned: ~w)", TT, [P, Returned, RR])),
    tt_check_given_closed(RetO, Given, RR, TT).

tt_check_given_open(Returned, [], _, TT) :- !,
    forall(member(P, Returned), (tt_get_path_components(P, _, TS, TT),
                                 (   TS == [], ! ;
                                 tte("Non-void open path found: ~w", TT, [P])))).
tt_check_given_open(Returned, [P|Given], RR, TT) :-
    (   tt_out(tt_pcheck, P, Returned, RetO), ! ;
        tte("Open path equivalent to ~w not found in results: ~w ~n(all results returned: ~w)", TT, [P, Returned, RR])),
    tt_check_given_open(RetO, Given, RR, TT).

% ====== tests
% tt(?TestID)

% 1: game state
tt(11):-ttc->true; (initial_game_state(GS), nonvar(GS) ;
                   tte("Game state should be instantiated", 11)).
tt(12):-ttc->true; (initial_game_state(GS), get_game_tiles(GS, TS), TS == [] ;
                   tte("Tile list should be empty", 12)).
tt(13):-ttc->true; (initial_game_state(GS), get_open_paths(GS, OPS), nonvar(OPS) ;
                   tte("Open paths should be instantiated", 13)).
tt(14):-ttc->true; (initial_game_state(GS), get_closed_paths(GS, CPS), CPS == [] ;
                   tte("Closed paths should be empty", 14)).

% 2: moves available initially
% availability check
tt(Test21a) :- tt_moves_1(Moves), member((X,Y,T,R), Moves),
        Test21a = '21a'/X/Y/T/R,
        tt_testInitialMoves(Test21a, aff).
tt(Test21b) :- tt_moves_2(Moves), member((X,Y,T,R), Moves),
    Test21b = '21b'/X/Y/T/R,
    tt_testInitialMoves(Test21b, neg).
tt(Test21c) :- (ttc -> tt_moves_3(Moves), member((X,Y,T,R), Moves);true),
    Test21c = '21c'/X/Y/T/R,
    tt_testInitialMoves(Test21c, neg).

% generation check
tt(22) :- ttc->true;
    initial_game_state(GS), findall(Move, available_move(GS, Move), Moves),
    length(Moves, NMoves),
    (   NMoves == 448
    ;   tte("Incorrect number of available moves in initial state", 22)).

% 3. apply move
% game state
tt(Test31) :- tt_1moves(Moves), member((X, Y, T, R, _), Moves),
    Test31 = 31/X/Y/T/R, tt_1moveA(Test31).
% available moves
tt(Test32) :- tt_1moves(Moves), member((X, Y, T, R, _), Moves),
    Test32 = 32/X/Y/T/R, tt_1moveB(Test32).


% 4. apply move repeated
tt(Test41) :- tt_move_series(Test, Moves, TOpen, TClosed, MovesPlus),
    Test41 = 41/Test,
    (   ttc->true;
        tt_makeMoves(Moves, GSR, Test41),
        tt_get_game_components(GSR, Tiles, Open, Closed, Test41),
% game state
        tt_check_given_tiles(Tiles, Moves, Tiles, Test41),
        tt_check_given_open(Open, TOpen, Open, Test41),
        tt_check_given_closed(Closed, TClosed, Closed, Test41),
% available moves
        findall(Move, available_move(GSR, Move), AMoves),
        length(AMoves, NAMoves),
        Exp is 448 + MovesPlus,
        (   NAMoves =:= Exp, !;
            tte("Incorrect number of moves available; returned ~w, expected ~w",
                Test41, [NAMoves, Exp]))
    )
    .

tt(TT) :- TT = 50, ( ttc -> true;
    tt_reset_stack, assert(tt_tile_range(['#9'])),
    tt_run_play(GSR, TT), tt_check_final(GSR, 60, 0, 32, ['#9'], TT)
                   ).
tt(TT) :- TT = 51, ( ttc -> true;
    tt_reset_stack, assert(tt_tile_range(['#8'])),
    tt_run_play(GSR, TT), tt_check_final(GSR, 60, 0, 32, ['#8'], TT)
                   ).
tt(TT) :- TT = 52, ( ttc -> true;
    tt_reset_stack, assert(tt_tile_range(['#6'])),
    tt_run_play(GSR, TT), tt_check_final(GSR, 56, 4, 20, ['#6'], TT)
                   ).
tt(TT) :- TT = 53, ( ttc -> true;
    tt_reset_stack, assert(tt_tile_range(['#10'])),
    tt_run_play(GSR, TT), tt_check_final(GSR, 56, 4, 20, ['#10'], TT)
                   ).
tt(TT) :- TT = 54, ( ttc -> true;
    tt_reset_stack, assert(tt_tile_range(['#1'])),
    tt_run_play(GSR, TT), tt_check_final(GSR, 0, 0, 0, [], TT)
                   ).
tt(TT) :- TT = 55, ( ttc -> true;
    tt_reset_stack, findall(T, tile(_, _, _, _, T), L), assert(tt_tile_range(L)),
    tt_run_play(GSR, TT), tt_check_final(GSR, -1, -1, -1, L, TT)
                   ).
tt(TT) :- TT = 56, ( ttc -> true; tt_free(30, 1, 0, TT)).
tt(TT) :- TT = 57, ( ttc -> true; tt_free(30, 1, 0, TT)).
tt(TT) :- TT = 58, ( ttc -> true; tt_free(30, 1, 0, TT)).
tt(TT) :- TT = 59, ( ttc -> true; tt_free(30, 1, 0, TT)).

tt_free(MLT, MLO, MLC, TT) :-
    tt_reset_stack, findall(T, (tile(_, _, _, _, T), T \= '#1'), L), assert(tt_tile_range(L)),
    tt_run_play(GSR, TT), tt_check_final(GSR, -1, -1, -1, L, TT),
    tt_get_game_components(GSR, Tiles, Open, Closed, TT),
    length(Tiles, LT), length(Open, LO), length(Closed, LC),
    (   LT >= MLT, ! ; tte("There are ~w tiles but there should be at least ~w", TT, [LT, MLT])),
    (   (LC >= MLO ; LO >= MLO), ! ;
              tte("There are ~w open paths but there should be at least ~w", TT, [LO, MLO])),
    (   LC >= MLC, ! ; tte("There are ~w closed paths but there should be at least ~w", TT, [LC, MLC])).


tt_reset_stack :- retractall(tt_tile_range(_)), retractall(tt_tiles_returned(_)).
:- dynamic tt_tile_range/1, tt_tiles_returned/1.
:- retractall(next_tile(_, _)).
:- assert(next_tile(Tm, Ti):-tt_next_tile(Tm, Ti)).
:- tt_reset_stack.

tt_set_tile_range(Range) :- tt_reset_stack, assert(tt_tile_range(Range)).

tt_generate_tile(Tile):-
    tt_tile_range(Range), !,
    length(Range, Len),
    R is random(Len),
    nth0(R, Range, Tile).
tt_generate_tile(_):-
    tte("No tile range defined", '##').

tt_next_tile(0, Tile) :-
    \+ tt_tiles_returned(_), !,
    tt_generate_tile(Tile),
    assert(tt_tiles_returned([Tile])).
tt_next_tile(T, _) :- T > 0,
    \+ tt_tiles_returned(_), !,
    tte("Tile requested for future time ~w (current time 0)", '##', [T]).
tt_next_tile(Time, Tile) :- tt_tiles_returned(Tiles), length(Tiles, ActTime),
    (   Time =< ActTime -> true; !, tte("Tile requested for future time ~w (current time ~w)",
                                '##', [Time, ActTime])),
    Time < ActTime, !, nth0(Time, Tiles, Tile).
tt_next_tile(Time, Tile) :- tt_tiles_returned(Tiles), length(Tiles, ActTime),
    (   Time =< ActTime, !; tte("Tile requested for future time ~w (current time ~w).",
                                '##', [Time, ActTime])),
    Time == ActTime, !,
    tt_generate_tile(Tile), append(Tiles, [Tile], TilesNew),
    retractall(tt_tiles_returned(_)),
    assert(tt_tiles_returned(TilesNew)).
tt_next_tile(Time, _) :- tte("Internal error for next tile at time ~w", '##', [Time]).

tt_run_play(GSR, TT) :-
    (   play_game(GSR), !; tte("play_game failed", TT)).

tt_check_d([]).
tt_check_d([E|T]) :- \+ member(E, T), tt_check_d(T).

tt_check_final(GS, NT, NOpen, NClosed, TAv, TT) :-
    tt_get_game_components(GS, Tiles, Open, Closed, TT),
%    writeln(Tiles), writeln("--"), writeln(Open), writeln("--"),
%    forall(member(CC, Closed), format("~w~n~n", CC)),
    length(Tiles, NTs),
    (   NT >= 0 ->
        (   NTs == NT, ! ; tte("Game contains ~w tiles but should contain ~w", TT, [NTs, NT]))
        ;
        (   NTs =< 60, ! ; tte("Game contains ~w tiles but should contain at most 60", TT, [NTs]))
    ),
    findall(T, (member(T, Tiles), T = (_, ID, _), \+ member(ID, TAv)), L),
    (   L == [], ! ; tte("Invalid tiles found: ~w", TT, [L])),
    length(Open, NPO),
    (   ( NOpen =< 0, ! ; NPO == NOpen), ! ;
           tte("There are ~w open paths but ~w are expected", TT, [NPO,NOpen])),
    length(Closed, NPC),
    (   (NClosed =<0, ! ; NPC == NClosed), ! ;
           tte("There are ~w closed paths but ~w are expected", TT, [NPC,NClosed])),
    findall(Entry, ((member(P, Closed) ; member(P, Open))
                   , get_path_entry(P, Entry)), Entries),
    (   tt_check_d(Entries), ! ; tte("Duplicate entry points found for closed paths: ~w",
                                     TT, Entries)),
    forall(member(PC, Closed), tt_check_path(PC, Tiles, closed, TT)),
    forall(member(PO, Open), tt_check_path(PO, Tiles, open, TT))
    .

tt_check_path(Path, Tiles, Type, TT) :-
    tt_get_path_components(Path, (XI, YI), PTiles, TT), entry_point(XI, YI, DI),
    rts(Rts), member('#9'/'R0'/_/DI/DX/DY, Rts), X2 is XI-2+DX, Y2 is YI-2+DY,
    tt_path_it(X2, Y2, DI, PTiles, Tiles, Type, Path, TT).

tt_path_it(X, Y, _, Path, _, Type, PPath, TT) :-
%    format("~w ~w~n", [X, Y]),
    exit_point(X, Y, _), !,
    (   Type == closed, ! ;
          tte("Path was supposed to be open but reached exit point: ~w", TT, [PPath])),
    (   Path == [], ! ; tte("Tiles beyond exit point in path: ~w. Tiles found: ~w",
                            TT, [PPath, Path])).
tt_path_it(X, Y, _, [], Tiles, open, PPath, TT) :-
    \+ member(((X, Y), _, _), Tiles), ! ;
             tte("Open path ~w stopped at (~w, ~w) but tile is present", TT, [PPath, X, Y]).
tt_path_it(X, Y, D, [(T, R) | Path], Tiles, Type, PPath, TT) :-
    (   member(((X, Y), T, R), Tiles), ! ;
             tte("Tile ~w part of path ~w is not a tile placed in the game",
                 TT, [((X, Y), T, R), PPath])),
    rts(Rts),
    (   member(T/R/D/E/DXA/DYA, Rts), ! ;
             tte("Internal: Cannot find information on tile ~w/~w", TT, [T, R])),
    X2 is X-2+DXA, Y2 is Y-2+DYA,
%    format("from ~w ~w ~w to ~w ~w ~w~n", [X, Y, D, X2, Y2, E]),
    tt_path_it(X2, Y2, E, Path, Tiles, Type, PPath, TT).
tt_path_it(X, Y, D, Path, _, Type, PPath, TT) :-
    tte("Other error relating to ~w path ~w when reached ~w/~w/~w with remaining tiles ~w",
        TT, [Type, PPath, X, Y, D, Path]).


% Runs all tests.
ttestAll :-
    assert(ttc),
    findall(ID, tt(ID), AllTests),
    retractall(ttc),
    length(AllTests, LT),
    findall(IDD, (member(IDD, AllTests), ttestOne(IDD)), Succeeded),
    findall(T, (member(T, AllTests), \+ member(T, Succeeded)), Failed),
    length(Succeeded, LS), length(Failed, LF),
    format("+0.0: Result: ~w / ~w tests succeeded. tests failed: ~w.~n",
           [LS, LT, LF]),
    findall(Points, (member(IDD, Succeeded), ttpoints(IDD, Points)), LPoints),
    sum_list(LPoints, SumPoints),
    format("+0.0: Total Points: ~w~n", [SumPoints]),
    !.

% ttestOne(+TestID) -- runs 1 test
ttestOne(TestID) :- ttimeLimit(T),
    catch(
        call_with_time_limit(T, ttestOneFree(TestID)),
        time_limit_exceeded,
        (
            format("+0.0: Time limit [~w] exceeded.~n", [T]),
            !, fail
        )).

% ttestOneA(+TestID) -- runs 1 test, with no time limit
ttestOneFree(TestID) :- tt(TestID),
    (   ttpoints(TestID, Points) ;
        format("internal error: points not found for ~w~n", [TestID])),
    format("+~w: Test ~w OK.~n", [Points, TestID]).
ttestOneFree(TestID) :- !, format("+0.0: Test ~w failed.~n", [TestID]), fail.

% nu utilizați acest cod în temă
rts(['#1'/'R0'/e/w/1/2,'#1'/'R0'/s/n/2/1,'#1'/'R0'/w/e/3/2,'#1'/'R0'/n/s/2/3,'#2'/'R0'/e/w/1/2,'#2'/'R0'/s/e/3/2,'#2'/'R0'/w/n/2/1,'#2'/'R0'/n/s/2/3,'#3'/'R0'/e/w/1/2,'#3'/'R0'/s/s/2/3,'#3'/'R0'/w/n/2/1,'#3'/'R0'/n/e/3/2,'#2'/'R3'/e/w/1/2,'#2'/'R3'/s/n/2/1,'#2'/'R3'/w/s/2/3,'#2'/'R3'/n/e/3/2,'#4'/'R0'/e/w/1/2,'#4'/'R0'/s/s/2/3,'#4'/'R0'/w/e/3/2,'#4'/'R0'/n/n/2/1,'#5'/'R0'/e/w/1/2,'#5'/'R0'/s/e/3/2,'#5'/'R0'/w/s/2/3,'#5'/'R0'/n/n/2/1,'#6'/'R0'/e/s/2/3,'#6'/'R0'/s/w/1/2,'#6'/'R0'/w/n/2/1,'#6'/'R0'/n/e/3/2,'#3'/'R3'/e/s/2/3,'#3'/'R3'/s/n/2/1,'#3'/'R3'/w/w/1/2,'#3'/'R3'/n/e/3/2,'#3'/'R2'/e/s/2/3,'#3'/'R2'/s/w/1/2,'#3'/'R2'/w/e/3/2,'#3'/'R2'/n/n/2/1,'#7'/'R0'/e/s/2/3,'#7'/'R0'/s/e/3/2,'#7'/'R0'/w/w/1/2,'#7'/'R0'/n/n/2/1,'#2'/'R2'/e/s/2/3,'#2'/'R2'/s/n/2/1,'#2'/'R2'/w/e/3/2,'#2'/'R2'/n/w/1/2,'#8'/'R0'/e/s/2/3,'#8'/'R0'/s/e/3/2,'#8'/'R0'/w/n/2/1,'#8'/'R0'/n/w/1/2,'#3'/'R1'/e/e/3/2,'#3'/'R1'/s/w/1/2,'#3'/'R1'/w/n/2/1,'#3'/'R1'/n/s/2/3,'#4'/'R1'/e/e/3/2,'#4'/'R1'/s/n/2/1,'#4'/'R1'/w/w/1/2,'#4'/'R1'/n/s/2/3,'#9'/'R0'/e/e/3/2,'#9'/'R0'/s/s/2/3,'#9'/'R0'/w/w/1/2,'#9'/'R0'/n/n/2/1,'#7'/'R3'/e/e/3/2,'#7'/'R3'/s/w/1/2,'#7'/'R3'/w/s/2/3,'#7'/'R3'/n/n/2/1,'#7'/'R2'/e/e/3/2,'#7'/'R2'/s/s/2/3,'#7'/'R2'/w/n/2/1,'#7'/'R2'/n/w/1/2,'#5'/'R3'/e/e/3/2,'#5'/'R3'/s/n/2/1,'#5'/'R3'/w/s/2/3,'#5'/'R3'/n/w/1/2,'#2'/'R1'/e/n/2/1,'#2'/'R1'/s/w/1/2,'#2'/'R1'/w/e/3/2,'#2'/'R1'/n/s/2/3,'#5'/'R1'/e/n/2/1,'#5'/'R1'/s/e/3/2,'#5'/'R1'/w/w/1/2,'#5'/'R1'/n/s/2/3,'#7'/'R1'/e/n/2/1,'#7'/'R1'/s/s/2/3,'#7'/'R1'/w/w/1/2,'#7'/'R1'/n/e/3/2,'#8'/'R1'/e/n/2/1,'#8'/'R1'/s/w/1/2,'#8'/'R1'/w/s/2/3,'#8'/'R1'/n/e/3/2,'#5'/'R2'/e/n/2/1,'#5'/'R2'/s/s/2/3,'#5'/'R2'/w/e/3/2,'#5'/'R2'/n/w/1/2,'#10'/'R0'/e/n/2/1,'#10'/'R0'/s/e/3/2,'#10'/'R0'/w/s/2/3,'#10'/'R0'/n/w/1/2]).

































