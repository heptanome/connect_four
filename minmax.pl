:- module(minmax, [find_best_next_pos/5]).

:- use_module(utilities, [isColumnFull/1, updateColumn/3]).
:- use_module(attack_heuristics, [heuristic_max/3, heuristic_sum/3, heuristic_alert/3]).
:- use_module(defense_heur, [heuristic_def/3]).

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
find_best_next_pos(Current,BestNext,Value,Player, Heur) :-
  findall(NextPos, possible_move(Current,NextPos,Player),ListNextPos),
  ListNextPos \== [],
  best_of_list(ListNextPos, BestNext, Value, Player, Heur),!.
  % on s arrete dès qu'on a un meilleur element

% si possible_move(current,nextPos) ne revoie aucune valeur pour nextPos,
% alors c'est que current n'a pas de successeur :
find_best_next_pos(Current,_,Value, Player, Heur) :-
  value_of(Current,Player, Value,Heur).

% Utilise l'heuristique pour calculer la valeur du coup (par default calcule juste une valeur random)
value_of(Board, Player, Cost, 'attack_heur_max') :-
    heuristic_max(Board, Player, Cost).
    
value_of(Board, Player, Cost, 'attack_heur_sum') :-
    heuristic_sum(Board, Player, Cost).
    
 value_of(Board, Player, Cost, 'attack_heur_alert') :-
    heuristic_alert(Board, Player, Cost).

value_of(Board, Player, Cost,  'defense_heur') :-
    heuristic_def(Board, Player, Cost), !.

value_of(_, _, Value,_) :-
    Value is random(20), !.

% best_of_list: d'apres la liste de tous les prochains coups possibles,
% recuperation du meilleur coup a jouer en fonction de l'heuristique (TODO).
% la liste: ensemble des coups possibles
% BestBoard: Le meilleur coup a jouer (remonte par la fonction)
% BestVal: la meilleure valeur, aussi remontee
% Player: soit '1', soit '2'
% Heur : l'heuristique à utiliser

best_of_list([Board], Board, Value, Player, Heur) :-
  value_of(Board,Player,Value, Heur).

% on determine la valeur, on regarde la prochaine, on compare
best_of_list([Board1 | EndList],BestBoard, BestVal, Player, Heur) :-
  value_of(Board1, Player, Val1, Heur),
  best_of_list(EndList,Board2,Val2, Player, Heur),
  compare(Board1,Val1,Board2,Val2,BestBoard,BestVal).

