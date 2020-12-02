:- module(alphabeta, [call_alphabeta/5]).


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

% l'argument apres 'min' est le nombre de coups a regarder plus loin:
% 1: regarder seulement 1 coup plus loin, etc.
call_alphabeta(Board, Player, Heur, BestBoard, BestVal) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    alphabeta(PossibleBoards, Heur, Opponent, 'max', 3,
              BestVal, BestBoard, -9999, 9999).

pruning('max', A, B, Val, Val, B) :-
    %writeln('max' + A + ' ' + B + ' ' + Val),
    Val > A, !.

pruning('min', A, B, Val, A, Val) :-
    %writeln('min' + A + ' ' + B + ' ' + Val),
    Val < B, !.

pruning(_, A, B, _, A, B).

alphabeta([Board], Heur, Player, _, 0, Val, _, _, _) :- 
    changePlayer(Player, Opponent),
    value_of(Board, Opponent, Val, Heur), !.

alphabeta([Board1|Tail], Heur, Player, MaximPlayer, 0,
          BestVal, BestBoard, A, B) :- 
    changePlayer(Player, Opponent),
    value_of(Board1, Opponent, Val1, Heur),
    alphabeta(Tail, Heur, Player, MaximPlayer, 0, Val2, Board2, A, B),
    comp_best_val(MaximPlayer, Val1, Board1, Val2, Board2, BestVal, BestBoard), !.

alphabeta([Board], Heur, Player, MaximPlayer, Depth, Val, Board, A, B) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    changeMaximizing(MaximPlayer, MaximOppon),
    alphabeta(PossibleBoards, Heur, Opponent, MaximOppon, NewDepth, Val, _, A, B),
    pruning(MaximPlayer, A, B, Val, _, _), !.

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

