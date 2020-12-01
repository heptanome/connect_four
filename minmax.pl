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
    Val1 >= Val2.

comp_best_val('max', Val1, _, Val2, Board2, Val2, Board2) :-
    Val1 < Val2.

comp_best_val('min', Val1, Board1, Val2, _, Val1, Board1) :-
    Val1 < Val2.

comp_best_val('min', Val1, _, Val2, Board2, Val2, Board2) :-
    Val1 >= Val2.

changeMaximizing('max', 'min').
changeMaximizing('min', 'max').

% l'argument apres 'min' est le nombre de coups a regarder plus loin:
% 1: regarder seulement 1 coup plus loin, etc.
call_minmax(Board, Player, Heur, BestBoard, BestVal) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    PossibleBoards \== [],
    changePlayer(Player, Opponent),
    minmax_breadth(PossibleBoards, Opponent, Heur, 'min', 1, BestBoard, BestVal),
    write('Bestval: '), writeln(BestVal).

% breadth: look at "brother" boards
minmax_breadth([Board], Player, Heur, MaximPlayer, Depth, Board, Val) :-
    NewDepth is Depth - 1,
    changeMaximizing(MaximPlayer, MaximOpponent),
    changePlayer(Player, Opponent),
    minmax_depth(Board, Opponent, Heur, MaximOpponent, NewDepth, Val).

minmax_breadth([Board1 | Tail], Player, Heur, MaximPlayer, Depth, BestBoard, BestVal) :-
    NewDepth is Depth - 1,
    changeMaximizing(MaximPlayer, MaximOpponent),
    changePlayer(Player, Opponent),
    minmax_depth(Board1, Opponent, Heur, MaximOpponent, NewDepth, Val1),
    minmax_breadth(Tail, Player, Heur, MaximPlayer, Depth, Board2, Val2),
    comp_best_val(MaximPlayer, Val1, Board1, Val2, Board2, BestVal, BestBoard), !. 

% depth: look at "children" boards (nodes)
minmax_depth(Board, Player, Heur, _, 0, Val) :-
    value_of(Board, Player, Val, Heur), writeln(Val), !.

minmax_depth(Board, Player, Heur, MaximPlayer, Depth, BestVal) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    PossibleBoards \== [],
    minmax_breadth(PossibleBoards, Player, Heur, MaximPlayer, Depth, _, BestVal).
    %changeMaximizing(MaximPlayer, MaximOpponent),
    %changePlayer(Player, Opponent),
    %minmax_breadth(PossibleBoards, Opponent, Heur, MaximOpponent, Depth, _, BestVal).

minmax_depth(Board, Player, Heur, _, _, Val) :-
    value_of(Board, Player, Val, Heur), writeln(Val), !.

/*
minmax(Board,BestBoard,BestValue,Player,Heur,Depth, 0) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    NextDepth is Depth - 1,
    foldl(get_max_val, PossibleBoards, 0, BestBoard, BestValue, Player, Heur, NextDepth).
*/
% Usage : si val1 > val2 on dit que pos1 est a garder et vice-versa.
% compare(Pos1,Val1,   _,Val2,Pos1,Val1) :- Val1 < Val2, !.
% compare(   _,Val1,Pos2,Val2,Pos2,Val2) :- Val2 =< Val1.

compare_boards(Board1,Value1,_,Value2,Board1,Value1) :-
    Value1 < Value2,
    !.

compare_boards(_,Value1,Board2,Value2,Board2,Value2) :-
    Value2 =< Value1.

% Usage : Chercher la meilleure disposition du plateau atteignable depuis la
% disposition initiale du plateau 
%         Y prend la valeur de cette position.
% minmax(+Current,-BestNext,-Value,+Player) :
%  - Current  : dispostion du plateau actuel
%  - BestNext : la meilleure disposition du plateau
%  - Value    : valeur non écrite mais renvoyée
%  - Player   : Numéro du joueur actuel (1 ou 2)
% minmax(Current,BestNext,Value,Player, Heur) :-
%  findall(NextPos, possible_move(Current,NextPos,Player),ListNextPos),
%  ListNextPos \== [],
%  best_of_list(ListNextPos, BestNext, Value, Player, Heur),!.
%  on s arrete dès qu on a un meilleur element

% si possible_move(current,nextPos) ne revoie aucune valeur pour nextPos,
% alors c est que current n a pas de successeur :
% minmax(Current,_,Value, Player, Heur) :-
%  value_of(Current,Player, Value,Heur).

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

% best_of_list: d'apres la liste de tous les prochains coups possibles,
% recuperation du meilleur coup a jouer en fonction de l'heuristique (TODO).
% la liste: ensemble des coups possibles
% BestBoard: Le meilleur coup a jouer (remonte par la fonction)
% BestVal: la meilleure valeur, aussi remontee
% Player: soit '1', soit '2'
% Heur : l'heuristique à utiliser

/*
best_of_list([Board], Board, Value, Player, Heur) :-
  value_of(Board,Player,Value, Heur).

% on determine la valeur, on regarde la prochaine, on compare
best_of_list([Board1 | EndList],BestBoard, BestVal, Player, Heur) :-
  value_of(Board1, Player, Val1, Heur),
  best_of_list(EndList,Board2,Val2, Player, Heur),
  compare(Board1,Val1,Board2,Val2,BestBoard,BestVal).
*/
