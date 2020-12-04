:- module(alphabeta, [call_alphabeta/6]).


:- use_module(heuristics/heuristics,
              [heuristic_max/3, heuristic_aSum/3, heuristic_dSum/3,
              heuristic_fSum/3, heuristic_alert/4, heuristic_fAlert/3]).
:- use_module(utils/utilities,
              [changePlayer/2, isColumnFull/1, updateColumn/3]).


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
    Val1 =< Val2.

comp_best_val('min', Val1, _, Val2, Board2, Val2, Board2) :-
    Val1 > Val2.

changeMaximizing('max', 'min').
changeMaximizing('min', 'max').

call_alphabeta(Board, Player, Heur, BestBoard, BestVal, Depth) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    alphabeta(PossibleBoards, Heur, Opponent, 'max', Depth,
              BestVal, BestBoard, -9999, 9999).

% specific to ALPHABETA, used to store the biggest A when branching
pruning('max', A, B, Val, Val, B) :-
    Val > A.

% same principle
pruning('min', A, B, Val, A, Val) :-
    Val < B.

pruning(_, A, B, _, A, B).

% ALPHABETA: works on the same principle as MINMAX excepts it will not 'branch
% in' in boards doomed to get an uninteresting score by storing the highest
% minimum and lowest maximum in A and B.
% https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning

% le cas ou on arrive au bout de la recursion (vis a vis de la profondeur)
% et ou il ne reste qu'un tableau a tester
alphabeta([Board], Heur, Player, _, 0, Val, _, _, _) :- 
    changePlayer(Player, Opponent),
    value_of(Board, Opponent, Val, Heur), !.

% le cas ou on arrive au bout de la recursion mais il reste plus d'un tableaux
% a tester
alphabeta([Board1|Tail], Heur, Player, MaximPlayer, 0,
          BestVal, BestBoard, A, B) :- 
    changePlayer(Player, Opponent),
    value_of(Board1, Opponent, Val1, Heur),
    alphabeta(Tail, Heur, Player, MaximPlayer, 0, Val2, Board2, A, B),
    comp_best_val(MaximPlayer, Val1, Board1, Val2, Board2, BestVal, BestBoard), !.

% le cas ou la recursion n'est pas arrivee a son terme et ou la liste ne
% contient plus qu'un element
alphabeta([Board], Heur, Player, MaximPlayer, Depth, Val, Board, A, B) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    changeMaximizing(MaximPlayer, MaximOppon),
    alphabeta(PossibleBoards, Heur, Opponent, MaximOppon, NewDepth, Val, _, A, B),
    pruning(MaximPlayer, A, B, Val, _, _), !.

% le cas ou la recursion n'est pas arrivee a son terme et ou la liste a plus
% d'un element
alphabeta([Board1|Tail], Heur, Player, MaximPlayer, Depth,
          BestVal, BestBoard, A, B) :-
    findall(NextBoard, possible_move(Board1, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    changeMaximizing(MaximPlayer, MaximOppon),
    alphabeta(PossibleBoards, Heur, Opponent, MaximOppon, NewDepth, Val1, _, A, B),
    pruning(MaximPlayer, A, B, Val1, NewA, NewB),
    alphabeta(Tail, Heur, Player, MaximPlayer, Depth, Val2, Board2, NewA, NewB),
    comp_best_val(MaximPlayer, Val1, Board1, Val2, Board2, BestVal, BestBoard).

% Utilise l heuristique pour calculer la valeur du coup (par default calcule
% juste une valeur random)
value_of(Board, Player, Cost, 'attack_max') :-
    heuristic_max(Board, Player, Cost).
    
value_of(Board, Player, Cost, 'attack_sum') :-
    heuristic_aSum(Board, Player, Cost).
    
 value_of(Board, Player, Cost, 'attack_alert') :-
    heuristic_alert(Board, Player, Cost, 4).
    
value_of(Board, Player, Cost,  'defense_sum') :-
    heuristic_dSum(Board, Player, Cost).

value_of(Board, Player, Cost,  'full_sum') :-
    heuristic_fSum(Board, Player, Cost).
    
value_of(Board, Player, Cost,  'full_alert') :-
    heuristic_fAlert(Board, Player, Cost).

value_of(_, _, Value, _) :-
    Value is random(20), !.

