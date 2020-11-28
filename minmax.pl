:- module(minmax, [find_best_next_pos/4]).

:- use_module(utilities, [isColumnFull/1, updateColumn/3]).
:- use_module(firstHeuristic, [heuristic/3]).

% Usage : Passer le board actuel dans Board, il renverra un NextBoard possible
%         dans la variable NextBoard. Si IndexColonne n'est pas specifie, il renverra
%         tous les moves possibles atteignables
possible_move(Board, NextBoard, Player) :-
  Board=NextBoard,
  nth1(IndexColumn, Board, Column),
  not(isColumnFull(Column)),
  updateColumn(Column, NewColumn, Player),
  nth1(IndexColumn, NextBoard, NewColumn).

% Usage : si val1 > val2 on dit que pos1 est a garder et vice-versa.
compare(Pos1,Val1,   _,Val2,Pos1,Val1) :- Val1 < Val2, !.
compare(   _,Val1,Pos2,Val2,Pos2,Val2) :- Val2 =< Val1.

% Usage : Chercher la meilleure disposition du plateau atteignable depuis la disposition initiale du plateau 
%         Y prend la valeur de cette position.
% find_best_next_pos(+Current,-BestNext,-Value,+Player) :
%  - Current  : dispostion du plateau actuel
%  - BestNext : la meilleure disposition du plateau
%  - Value    : valeur non écrite mais renvoyée
%  - Player   : Numéro du joueur actuel (1 ou 2)
find_best_next_pos(Current,BestNext,Value,Player) :-
  findall(NextPos, possible_move(Current,NextPos,Player),ListNextPos),
  ListNextPos \== [],
  best_of_list(ListNextPos, BestNext, Value, Player),!.
  % on s arrete dès qu'on a un meilleur element

% si possible_move(current,nextPos) ne revoie aucune valeur pour nextPos,
% alors c'est que current n'a pas de successeur :
find_best_next_pos(Current,_,Value, Player) :-
  value_of(Current,Player, Value).

value_of(Board, Player, Cost) :-
    heuristic(Board, Player, Cost).


% best_of_list: d'apres la liste de tous les prochains coups possibles,
% recuperation du meilleur coup a jouer en fonction de l'heuristique (TODO).
% la liste: ensemble des coups possibles
% BestBoard: Le meilleur coup a jouer (remonte par la fonction)
% BestVal: la meilleure valeur, aussi remontee
% Player: soit '1', soit '2'

best_of_list([Board], Board, Value, Player) :-
  value_of(Board,Player,Value).

% on determine la valeur, on regarde la prochaine, on compare
best_of_list([Board1 | EndList],BestBoard, BestVal, Player) :-
  value_of(Board1, Player, Val1),
  best_of_list(EndList,Board2,Val2, Player),
  compare(Board1,Val1,Board2,Val2,BestBoard,BestVal).

