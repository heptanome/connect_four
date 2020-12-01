:- module(minmax, [call_minmax/5]).

:- use_module(utilities, [isColumnFull/1, updateColumn/3]).
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
    PossibleBoards \== [],
    changePlayer(Player, Opponent),
    minmax_breadth(PossibleBoards, Opponent, Heur, 'min', 3, BestBoard, BestVal),
    write('Bestval: '), writeln(BestVal).

% breadth: look at "brother" boards
minmax_breadth([Board], Player, Heur, MaximPlayer, Depth, Board, Val) :-
    NewDepth is Depth - 1,
    changeMaximizing(MaximPlayer, MaximOpponent),
    %changePlayer(Player, Opponent),
    minmax_depth(Board, Player, Heur, MaximOpponent, NewDepth, Val).

minmax_breadth([Board1 | Tail], Player, Heur, MaximPlayer, Depth, BestBoard, BestVal) :-
    NewDepth is Depth - 1,
    changeMaximizing(MaximPlayer, MaximOpponent),
    %changePlayer(Player, Opponent),
    minmax_depth(Board1, Player, Heur, MaximOpponent, NewDepth, Val1),
    minmax_breadth(Tail, Player, Heur, MaximPlayer, Depth, Board2, Val2),
    comp_best_val(MaximPlayer, Val1, Board1, Val2, Board2, BestVal, BestBoard), !. 

% depth: look at "children" boards (nodes)
minmax_depth(Board, Player, Heur, _, 0, Val) :-
    changePlayer(Player, Opponent),
    value_of(Board, Opponent, Val, Heur), writeln(Val), !.

minmax_depth(Board, Player, Heur, MaximPlayer, Depth, BestVal) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    PossibleBoards \== [],
    changePlayer(Player, Opponent),
    minmax_breadth(PossibleBoards, Opponent, Heur, MaximPlayer, Depth, _, BestVal).

minmax_depth(Board, Player, Heur, _, _, Val) :-
    changePlayer(Player, Opponent),
    value_of(Board, Opponent, Val, Heur), writeln(Val), !.

% Utilise l heuristique pour calculer la valeur du coup (par default calcule
% juste une valeur random)
value_of(Board, Player, Cost, 'attack_heur_max') :-
    heuristic_max(Board, Player, Cost).
    
value_of(Board, Player, Cost, 'attack_heur_sum') :-
    heuristic_sum(Board, Player, Cost).
    
 value_of(Board, Player, Cost, 'attack_heur_alert') :-
    heuristic_alert(Board, Player, Cost),
    writeln(Cost + " " + Player).

value_of(Board, Player, Cost,  'defense_heur') :-
    heuristic_def(Board, Player, Cost), !.

value_of(_, _, Value, _) :-
    Value is random(20), !.

