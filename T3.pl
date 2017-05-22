% Sterian Cosmin-Cristian 322CC

:- ensure_loaded('T3t.pl').
:- ensure_loaded('T3base.pl').
:- dynamic next_tile/2.


% initial_game_state(-GameState).
% Întoarce în GameState starea inițială a jocului.
% Initial state = Tiles:OpenPaths:ClosedPaths
initial_game_state([]:[]:[]).


% get_game_tiles(+GameState, -PlacedTileList)
% Întoarce lista de cărți rotite plasate în joc.
% Lista este o listă de triplete conținând:
% - coordonatele (X, Y) ale spațiului unde este cartea
% - identificatorul cărții (#1..#10)
% - identificatorul rotației (R0..R3)
get_game_tiles(PlacedTileList:_:_, PlacedTileList).
%get_game_tiles(GameState, PlacedTileList) :- GameState = PlacedTileList1:_:_, Tile = Coord:TID:RID,
%								findall(Result,
%									(member(Tile, PlacedTileList1), Result = (Coord, TID, RID)),
%									PlacedTileList).

% get_open_paths(+GameState, -Paths)
% Întoarce în Paths o listă de trasee care pornesc
% dintr-un punct de intrare și nu ajung într-un punct
% de ieșire. Elementele din fiecare traseu vor fi citite
% folosind predicatele get_path_*
get_open_paths(GameState, Paths) :- GameState = _:Paths:_.

% get_closed_paths(+GameState, -ClosedPaths)
% Întoarce în ClosedPaths traseele care pornesc
% dintr-un punct de intrare și ajung într-un punct
% de ieșire. Elementele din fiecare traseu vor fi citite
% folosind predicatele get_path_*
get_closed_paths(GameState, ClosedPaths) :- GameState = _:_:ClosedPaths.


% get_path_entry(+Path, -Point)
% Întoarce în Point o pereche (X, Y) care este punctul
% de intrare pentru un traseu. Pentru X și Y întoarse
% trebuie ca entry_point(X, Y, _) să fie adevărat.
get_path_entry(Path, Point) :- Path = Point:_.

% get_path_tiles(+Path, -IDs)
% Întoarce în IDs o listă de perechi (TID, RID)
% care conține, în ordinea în care sunt parcurse de traseu,
% pentru fiecare carte parcursă identificatorul cărții și
% identificatorul rotației acesteia.
% Este posibil ca o carte să apară în traseu de mai multe ori,
% dacă traseul trece de mai multe ori prin ea.
get_path_tiles(Path, IDs) :- Path = _:IDs.



get_neighbours(PlacedTileList:_:_, Coord, NeighList) :- 
											Coord = (X, Y),
											findall((X1, Y1),
												(
													(X1 is X-1, Y1 is Y, (exit_point(X1,Y1,_); member(((X1,Y1),_,_), PlacedTileList)));
													(X1 is X, Y1 is Y+1, (exit_point(X1,Y1,_); member(((X1,Y1),_,_), PlacedTileList)));
													(X1 is X+1, Y1 is Y, (exit_point(X1,Y1,_); member(((X1,Y1),_,_), PlacedTileList)));
													(X1 is X, Y1 is Y-1, (exit_point(X1,Y1,_); member(((X1,Y1),_,_), PlacedTileList)))
												), NeighList2),
											setof(Elem, member(Elem, NeighList2), NeighList).	% Nu imi dau seama de ce, dar daca fac setof direct in loc de findall, imi intoarce pe rand (cu ; la consola) solutiile cu duplicate

% valid_rotation(+TID, ?RID)
% Verifica ce rotatii valide are cartea respectiva
valid_rotation(_, 0).
valid_rotation(TID, 1) :- \+member(TID, ['#1', '#6', '#9', '#10']).
valid_rotation(TID, RID) :- \+member(TID, ['#1', '#4', '#6', '#8', '#9', '#10']), member(RID, [2,3]).

% highest_rotation(+TID, -RID)
% Gaseste cea mai mare rotatie valida(ca id).
highest_rotation(TID, RID) :- member(RID, [3,2,1,0]), valid_rotation(TID, RID), !.

% gen_all_tiles(-TilesList)
% Generarea unei liste cu toate cartile si rotatiile lor posibile(si care au sens).
gen_all_tiles(TilesList) :- 
					Tile = TID:RID,
					findall(Tile,
						(tile(_, _, _, _, TID), member(Rot, [0,1,2,3]), valid_rotation(TID, Rot), rotation(Rot, RID)),
						TilesList).

% Genereaza o lista de permutari ale parametrilor Delta ale unei carti date.
generate_rotation(Tile, Rot, Acc1, ResultTilesList, Acc2, ResultRotationList) :- Tile = DW:DS:DE:DN:TID, ResultTile = DN:DW:DS:DE:TID, highest_rotation(TID, MaxRot), Rot < MaxRot, ResultRotation is Rot + 1,
															generate_rotation(ResultTile, ResultRotation, [ResultTile|Acc1], ResultTilesList, [ResultRotation|Acc2], ResultRotationList).
generate_rotation(Tile, Rot, Acc1, Res1, Acc2, Acc2) :- Tile = _:_:_:_:TID, highest_rotation(TID, MaxRot), Rot = MaxRot, reverse(Acc1, Res1), !.

%generate_rotation(_, 0, Acc1, Res1, Acc2, Res2) :- reverse(Acc1, Res1), reverse(Acc2, Res2), !.
%generate_rotation(Tile, Rot, Acc1, ResultTileList, Acc2, ResultRotationList) :- Tile = DW:DS:DE:DN:TID, ResultTile = DN:DW:DS:DE:TID, ResultRotation is Rot - 1, 
%												generate_rotation(ResultTile, ResultRotation, [ResultTile|Acc1], ResultTileList, [ResultRotation|Acc2], ResultRotationList).

% get_valid_rotations_in_margin1(+TID, +Dir, -RotationsList)
% Genereaza rotatiile posibile pentru cartea respectiva, in cazul cu un singur vecin entry_point
get_valid_rotations_in_margin1(TID, Dir, RotationsList) :- 
										tile(DW, DS, DE, DN, TID),
										generate_rotation(DW:DS:DE:DN:TID, 0, [], ResultTileList, _, _),
										findall(Elem:I,
											(member(I, [1,2,3]), nth1(I, ResultTileList, Elem)),
											Tiles),	% Aduc elementele la forma (DeltaW:DeltaS:DeltaE:DeltaN:TID):RID, adica mai concatenez :RID la elementele din lista. 
													% Stiu ca au fost facute in ordine, deci RID e chiar indexul lor in lista
										Tiles2 = [(DW:DS:DE:DN:TID):0 | Tiles],
										Elem2 = (DW2:DS2:DE2:DN2:_):_,
										findall(Elem2,
											(member(Elem2, Tiles2),
												((Dir = e, DE2 \= 0); (Dir = n, DN2 \= 0); (Dir = w, DW2 \= 0); (Dir = s, DS2 \= 0))),
											RotationsList).

% get_valid_rotations_in_margin2(+TID, +Dir1, +Dir2, -RotationsList)
% Genereaza rotatiile posibile pentru cartea respectiva, in cazul cu doi vecini entry_point
get_valid_rotations_in_margin2(TID, Dir1, Dir2, RotationsList) :-
													tile(DW, DS, DE, DN, TID),
													generate_rotation(DW:DS:DE:DN:TID, 0, [], ResultTileList, _, _),
													findall(Elem:I,
														(member(I, [1,2,3]), nth1(I, ResultTileList, Elem)),
														Tiles),
													Tiles2 = [(DW:DS:DE:DN:TID):0 | Tiles],
													Elem2 = (DW2:DS2:DE2:DN2:_):_,
													findall(Elem2,
														(member(Elem2, Tiles2),
															(
																(((Dir1 = e, Dir2 = n); (Dir1 = n, Dir2 = e)), DE2 \= 0, DN2 \= 0, DE2 \= 1, DN2 \= 3); 
																(((Dir1 = n, Dir2 = w); (Dir1 = w, Dir2 = n)), DN2 \= 0, DW2 \= 0, DN2 \= 1, DW2 \= 3); 
																(((Dir1 = w, Dir2 = s); (Dir1 = s, Dir2 = w)), DW2 \= 0, DS2 \= 0, DW2 \= 1, DS2 \= 3); 
																(((Dir1 = s, Dir2 = e); (Dir1 = e, Dir2 = s)), DS2 \= 0, DE2 \= 0, DS2 \= 1, DE2 \= 3)
																)),
														RotationsList).

% filter_tiles_by_neighbours(+Tiles, +NeighList, -ResultTilesList)
% Pastreaza din lista de carti doar pe cele care pot fi folosite, tinand cont de vecinii din margine.
filter_tiles_by_neighbours(Tiles, NeighList, ResultTilesList) :-
															Tile = TID:_,
															Neigh = (Xn, Yn),
															Neigh2 = (Xn, Yn, Dir),
															findall(Neigh2,
																(member(Neigh, NeighList), exit_point(Xn, Yn, Dir)),
																NeighList2),	% Fac o lista cu vecinii din marginea hartii, memorand si directia spre care pleaca trenul.
															findall(TileResult,
																(member(Tile, Tiles),
																	(NeighList2 = [], TileResult = Tile);
																	(NeighList2 = [N1], N1 = (_, _, DirN1), get_valid_rotations_in_margin1(TID, DirN1, RotationsList), 
																		member(T, RotationsList), T = (_:_:_:_:ResTID):ResRot, rotation(ResRot, ResRID), TileResult = ResTID:ResRID);
																	(NeighList2 = [N1, N2], N1 = (_, _, DirN1), N2 = (_, _, DirN2), get_valid_rotations_in_margin2(TID, DirN1, DirN2, RotationsList),
																		member(T, RotationsList), T = (_:_:_:_:ResTID):ResRot, rotation(ResRot, ResRID), TileResult = ResTID:ResRID)
																	),
																ResultTilesList).

% available_move(+GameState, -Move)
% Predicatul leagă argumentul Move la o mutare disponibilă
% în starea GameState a jocului. Formatul pentru Move trebuie
% să fie același ca și în cazul utilizării predicatelor
% get_move_*.
% Dacă Move este legat, predicatul întoarce true dacă mutarea
% este disponibilă.
% Dacă Move nu este legat, soluții succesive (obținute folosind
% comanda ; la consolă, sau obținute cu findall) vor oferi
% diverse mutări valide în starea de joc dată.
% findall(Move, available_move(State, Move), Moves) trebuie să
% întoarcă în Moves toate mutările valide în starea dată, fără
% duplicate.
available_move(GameState, Move) :- 
							Move = (Coord, TID, RID),
							%GameState = PlacedTileList:_:_,
							Coord = (X, Y),
							limits(1, 1, W, H),
							between(1, W, X), between(1, H, Y), \+center_space(X, Y),	% Elimin coordonatele din centru ca fiind solutii posibile
							get_neighbours(GameState, Coord, NeighList),
							get_game_tiles(GameState, PlacedTileList), \+member((Coord,_,_), PlacedTileList),	%Elimin coordonatele altor carti ca fiind pozitii disponibile
							findall(Neigh,
								(member(Neigh, NeighList), Neigh = (Xn, Yn), (entry_point(Xn, Yn, _); member((Neigh,_,_), PlacedTileList))),
								[ _ |_]),	% Elimin coordonatele care nu sunt invecinate cu spatii deja ocupate(cartea trebuie plasata langa o margine a hartii sau langa o alta carte)
							gen_all_tiles(Tiles),
							filter_tiles_by_neighbours(Tiles, NeighList, ResultTilesList),
							member(Elem, ResultTilesList), Elem = TID:RID.


% Atenție! Folosirea celor 3 predicate get_move_* pe aceeași
% variabilă neinstanțiată Move trebuie să rezulte în legarea
% lui Move la o descriere completă a mutării, astfel încât
% available_move(FS, Move) să dea adevărat dacă mutarea
% descrisă este validă în starea GS a jocului.

% get_move_space(?Move, ?Space)
% Predicatul este adevărat dacă Space corespunde spațiului
% (X, Y) de pe hartă unde a fost pusă o carte în urma
% mutării Move.
% Vezi și observația de mai sus.
get_move_space((Space, _, _), Space).

% get_move_tile_id(?Move, ?TID)
% Predicatul este adevărat dacă TID corespunde
% identificatorului ('#1'..'#10') cărții care a fost plasată
% pe hartă în urma mutării Move.
% Vezi și observația de mai sus.
get_move_tile_id((_, TID, _), TID).

% get_move_rotation_id(?Move, ?RotID)
% Predicatul este adevărat dacă RotID corespunde
% identificatorului rotației ('R0'..'R3') cărții care a fost
% plasată în urma mutării Move.
% Vezi și observația de mai sus.
get_move_rotation_id((_, _, RotID), RotID).



% apply_move(+GameStateBefore, +Move, -GameStateAfter)
% Leagă al treilea argument la starea de joc care rezultă
% în urma aplicării mutării Move în starea GameStateBefore.
apply_move(PlacedTileList:OpenPaths:ClosedPaths, (Coord, TID, RID), GameStateAfter) :- 
							get_neighbours(PlacedTileList:_:_, Coord, NeighList),
							(	%Am doar vecini entry_point, deci se porneste o cale noua.
								forall(member((Xn, Yn), NeighList), entry_point(Xn, Yn, _)),
								(NeighList = [N1], NewPath = N1:[(TID, RID)], ResultOpenPaths = [NewPath | OpenPaths]);	%Am doar un vecin entry_point
								(NeighList = [N1, N2], NewPath1 = N1:[(TID, RID)], NewPath2 = N2:[(TID, RID)], ResultOpenPaths = [NewPath1, NewPath2 | OpenPaths])	%Am doi vecini entry_point
								),
							GameStateAfter = [(Coord, TID, RID) | PlacedTileList]:ResultOpenPaths:ClosedPaths.	%+OpenPaths si ClosedPaths

% pick_move(+GameState, +TID, -Move)
% Alege o mutare care folosește cartea cu identificatorul TID,
% pentru a fi aplicată în starea GameState. Mutarea este
% întoarsă în Move.
pick_move(_,_,_) :- fail.

% play_game(-FinalGameState)
% Joacă un joc complet, pornind de la starea inițială a jocului
% și continuând până când nu se mai poate pune cartea care
% este la rând.
% Cărțile de plasat se obțin folosind
% predicatul next_tile(+Time, -TID), unde Time este legat la
% numărul de mutări realizate până în momentul curent, iar
% predicatul next_tile va lega TID la un identificator de carte.
play_game(_) :- fail.



















































