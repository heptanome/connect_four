:- module(minmax, [minmax/8]).

:- use_module(utilities, [isColumnFull/1, updateColumn/3,changePlayer/2]).
:- use_module(firstHeuristic, [heuristic/3]).
:- use_module(defense_heur, [heuristic_def/3]).


% Usage : Passer le board actuel dans Board, il renverra un NextBoard possible
%         dans la variable NextBoard. Si IndexColonne n est pas specifie, il
%         renverra tous les moves possibles atteignables
possible_move(Board, NextBoard, Player) :-
  % Board=NextBoard,
  nth1(IndexColumn, Board, Column), % prendre une colonne dans current
  not(isColumnFull(Column)),        % verifier qu'elle ne soit pas pleine
  updateColumn(Column, NewColumn, Player),
  nth1(IndexColumn, NextBoard, NewColumn).
  
% minmax(Board,LastBoard,BestValue,Player,Heur,0) :-
%    value_of(Board,Player,Value1,Heur),
%    compare_boards(Board,Value1,LastBoard,BestValue,TmpBoard,TmpValue),
%    writeln(Value1+"    "+BestValue+"   "+TmpValue),
%    !.

find_next_move(Current,Next,Player,Heur,Depth) :-
    findall([NextMove|Value], minmax(Current,Value,NextMove,_,Player,Heur,0,Depth),ListNextMoves),
    writeln(ListNextMoves),
    best_of_list(ListNextMoves,Next,_),
    !.
    
minmax(Board,Value,_,Board,Player,Heur,MaxDepth,MaxDepth) :- value_of(Board,Player,Value,Heur),writeln('MM1'+ +MaxDepth+    +Board+   +Value).

minmax(Board,Value,_,LastBoard,Player,Heur,0,MaxDepth) :-
    possible_move(Board,NextBoard,Player),
    writeln(Board+  +NextBoard),
    NewDepth is 1,
    changePlayer(Player,Opponent),
% nextBoard et Board ont la meme valeur ici.
    minmax(NextBoard,Value,NextBoard,LastBoard,Opponent,Heur,NewDepth,MaxDepth).

minmax(Board,Value,NextBoardToBePlayed,LastBoard,Player,Heur,Depth,MaxDepth) :-
    Depth \== MaxDepth,
    Depth \== 0,
    possible_move(Board,NextBoard,Player),
    writeln(Board+  +NextBoard),
    NewDepth is Depth+1,
    changePlayer(Player,Opponent),
    minmax(NextBoard,Value,NextBoardToBePlayed,LastBoard,Opponent,Heur,NewDepth,MaxDepth).

% Usage : si val1 > val2 on dit que pos1 est a garder et vice-versa.
 compare(Pos1,Val1,   _,Val2,Pos1,Val1) :- Val1 < Val2, !.
 compare(   _,Val1,Pos2,Val2,Pos2,Val2) :- Val2 =< Val1.

 compare_boards([Board1|Score1],[_|Score2],[Board1|Score1]) :-
    Score1 < Score2,!.
 
compare_boards([_|Score1],[Board2|Score2],[Board2|Score2]) :-
    Score1 >= Score2.

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
value_of(Board, Player, Cost, 'first_heur') :-
    heuristic(Board, Player, Cost), !.

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

best_of_list([[Board|Score]], Board, Score).

% on determine la valeur, on regarde la prochaine, on compare
best_of_list([[Board1|Score1] | EndList],BestBoard, BestScore) :-
  best_of_list(EndList,Board2,Score2),
  compare_boards([Board1|Score1],[Board2|Score2],[BestBoard|BestScore]).

