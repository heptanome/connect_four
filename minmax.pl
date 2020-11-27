:- module(minmax, [find_best_next_pos/4]).

:- use_module(utilities, [isColonneFull/1, updateColonne/3]).

% usage: passer le board actuel dans Board, il renverra un NextBoard possible
% dans la variable NextBoard. Si IndexColonne n'est pas specifie, il renverra
% tous les moves possibles atteignables
% player: 1 ou 2
possible_move(Board, NextBoard, Player) :-
  Board=NextBoard,
  nth1(IndexColonne, Board, Colonne),
  not(isColonneFull(Colonne)),
  updateColonne(Colonne, NewColonne, Player),
  nth1(IndexColonne, NextBoard, NewColonne).

% compare: si val1 > val2 on dit que pos1 est a garder et vice-versa.
compare(Pos1,Val1,_,Val2,Pos1,Val1) :- Val1 < Val2, !.
compare(_,Val1,Pos2,Val2,Pos2,Val2) :- Val2 =< Val1.

% usage : find_best_next(current,X,Y) -> X prend la valeur de la meilleure
%position atteignable depuis current. Y prend la valeur de cette position.
% current: currentBoard
% bestNext: le meilleur des tableaux qu'on va trouver
% value: valeur non écrit mais renvoyée
find_best_next_pos(Current,BestNext,Value,Player) :-
  findall(NextPos, possible_move(Current,NextPos,Player),ListNextPos),
  ListNextPos \== [],
  best_of_list(ListNextPos, BestNext, Value, Player),!.
  % on s arrete dès qu'on a un meilleur element

% si possible_move(current,nextPos) ne revoie aucune valeur pour nextPos,
% alors c'est que current n'a pas de successeur :
find_best_next_pos(Current,_,Value, Player) :-
  value_of(Current,Player, Value).

% value_of(Board, Player, Value) :-
value_of(_, _, Value) :-
  Value is random(20).


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

