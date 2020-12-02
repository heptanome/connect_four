:- module(minmax, [call_minmax/5]).

:- use_module(utilities, [changePlayer/2, isColumnFull/1, updateColumn/3]).
:- use_module(attack_heuristics, [heuristic_max/3, heuristic_sum/3, heuristic_alert/3]).
:- use_module(defense_heur, [heuristic_def/3]).

% Usage : Passer le board actuel dans Board, il renverra un NextBoard possible
%         dans la variable NextBoard. Si IndexColonne n est pas specifie, il
%         renverra tous les moves possibles atteignables
possible_move(Board, NextBoard, Player) :-
  Board=NextBoard,
  nth1(IndexColumn, Board, Column),
  not(isColumnFull(Column)),
  updateColumn(Column, NewColumn, Player),
  nth1(IndexColumn, NextBoard, NewColumn).

comp_best_val('max', Val1, Board1, Val2, _, Val1, Board1) :-
    Val1 > Val2.

comp_best_val('max', Val1, _, Val2, Board2, Val2, Board2) :-
    Val1 =< Val2.

comp_best_val('min', Val1, Board1, Val2, _, Val1, Board1) :-
    Val1 =< Val2.

comp_best_val('min', Val1, _, Val2, Board2, Val2, Board2) :-
    Val1 > Val2.

changeMaximizing('max', 'min').
changeMaximizing('min', 'max').

% l'argument apres 'min' est le nombre de coups a regarder plus loin:
% 1: regarder seulement 1 coup plus loin, etc.
call_minmax(Board, Player, Heur, BestBoard, BestVal) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    minmax(PossibleBoards, Heur, Opponent, 'min', 0, BestVal, BestBoard),
    writeln("Bestval: " + BestVal).

minmax([Board], Heur, Player, _, 0, Val, Board) :- 
    changePlayer(Player, Opponent),
    value_of(Board, Opponent, Val, Heur), !.

minmax([Board1|Tail], Heur, Player, MaximPlayer, 0, BestVal, BestBoard) :- 
    changePlayer(Player, Opponent),
    value_of(Board1, Opponent, Val1, Heur),
    minmax(Tail, Heur, Player, MaximPlayer, 0, Val2, Board2),
    comp_best_val(MaximPlayer, Val1, Board1, Val2, Board2, BestVal, BestBoard), !.

minmax([Board|Tail], Heur, Player, 'max', 0, RecVal, RecBoard) :- 
    changePlayer(Player, Opponent),
    value_of(Board, Opponent, Val, Heur),
    minmax(Tail, Heur, Player, 'max', 0, RecVal, RecBoard),
    RecVal >= Val, !.
 
minmax([Board1|Tail], Heur, Player, MaximPlayer, Depth, BestVal, BestBoard) :-
    findall(NextBoard, possible_move(Board1, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    changeMaximizing(MaximPlayer, MaximOppon),
    minmax(PossibleBoards, Heur, Opponent, MaximOppon, NewDepth, Val1, Board1),
    minmax(Tail, Heur, Player, MaximPlayer, Depth, Val2, Board2),
    comp_best_val(MaximPlayer, Val1, Board1, Val2, Board2, BestVal, BestBoard).

minmax([Board], Heur, Player, MaximPlayer, Depth, Val, Board) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    changeMaximizing(MaximPlayer, MaximOppon),
    minmax(PossibleBoards, Heur, Opponent, MaximOppon, NewDepth, Val, Board).

/*
minmax([Board1|Tail], Player, 'min', 0, BestVal, BestBoard) :- 
    value_of(Board1, Player, Val1, 'random'),
    minmax(Tail, Player, 'min', 0, Val2, Board2),
    comp_best_val('min', Val1, Board1, Val2, Board2, BestVal, BestBoard).
*/

/*
minmax([Board|Tail], Player, 'max', 0, Val, Board) :- 
    value_of(Board, Player, Val, 'random'),
    minmax(Tail, Player, 'max', 0, RecVal, _),
    RecVal < Val, !.
*/

/*
% dans les coups plus bas on change min et max avec le joueur
minmax([Board1|Tail], Player,'min', Depth, BestVal, BestBoard) :-
    writeln("min: " + Depth),
    findall(NextBoard, possible_move(Board1, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    minmax(PossibleBoards, Opponent, 'max', NewDepth, Val1, Board1),
    minmax(Tail, Player, 'min', Depth, Val2, Board2),
    comp_best_val('min', Val1, Board1, Val2, Board2, BestVal, BestBoard).
*/
/*
minmax([Board], Player,'max', Depth, Val, Board) :-
    writeln("max: " + Depth),
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    minmax(PossibleBoards, Opponent, 'min', NewDepth, Val, Board).
*/

call_minmax2(Board, Player, Heur, BestBoard, BestVal) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    minmax_breadth(PossibleBoards, Opponent, Heur, 'min', 3, BestBoard, BestVal),
    write('Bestval: '), writeln(BestVal).

% breadth: look at "brother" boards
minmax_breadth([Board], Player, Heur, MaximPlayer, Depth, Board, Val) :-
    NewDepth is Depth - 1,
    minmax_depth(Board, Player, Heur, MaximPlayer, NewDepth, Val).

minmax_breadth([Board1 | Tail], Player, Heur, MaximPlayer, Depth, BestBoard, BestVal) :-
    NewDepth is Depth - 1,
    minmax_depth(Board1, Player, Heur, MaximOpponent, NewDepth, Val1),
    minmax_breadth(Tail, Player, Heur, MaximPlayer, Depth, Board2, Val2),
    comp_best_val(MaximPlayer, Val1, Board1, Val2, Board2, BestVal, BestBoard),
    write(BestVal), writeln(MaximPlayer), !. 

% depth: look at "children" boards (nodes)
minmax_depth(Board, Player, Heur, _, 0, Val) :-
    changePlayer(Player, Opponent),
    value_of(Board, Opponent, Val, Heur), writeln(Val), !.

minmax_depth(Board, Player, Heur, MaximPlayer, Depth, BestVal) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    PossibleBoards \== [],
    changePlayer(Player, Opponent),
    changeMaximizing(MaximPlayer, MaximOpponent),
    minmax_breadth(PossibleBoards, Opponent, Heur, MaximOpponent, Depth, _, BestVal).

minmax_depth(Board, Player, Heur, _, _, Val) :-
    changePlayer(Player, Opponent),
    value_of(Board, Opponent, Val, Heur), writeln(Val), !.

% Utilise l heuristique pour calculer la valeur du coup (par default calcule
% juste une valeur random)
value_of(Board, Player, Cost, 'attack_heur_max') :-
    heuristic_max(Board, Player, Cost), writeln(Player).
    
value_of(Board, Player, Cost, 'attack_heur_sum') :-
    heuristic_sum(Board, Player, Cost).
    
 value_of(Board, Player, Cost, 'attack_heur_alert') :-
    heuristic_alert(Board, Player, Cost),
    writeln(Cost + " " + Player).

value_of(Board, Player, Cost,  'defense_heur') :-
    heuristic_def(Board, Player, Cost), !.

value_of(_, _, Value, _) :-
    Value is random(20), !.

