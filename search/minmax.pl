:- module(minmax, [call_minmax/6]).


:- use_module(heuristics/heuristics, [heuristic_max/3, heuristic_aSum/3,
                                     heuristic_dSum/3, heuristic_fSum/3,
                                     heuristic_alert/4, heuristic_fAlert/3]).
:- use_module(utils/utilities, [changePlayer/2, isColumnFull/1, updateColumn/3]).


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

% predicat utilise par connect_four qui appelle le minmax recursif avec les bons
% parametres (l'heuristique, le fait qu'on cherche a maximiser les points et
% la profondeur de recherche (apres le 'max'). Le meilleur tableaux est stocke
% dans BestBoard
call_minmax(Board, Player, Heur, BestBoard, BestVal, Depth) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    minmax(PossibleBoards, Heur, Opponent, 'max', Depth, BestVal, BestBoard).

% MINMAX: cherche a partir d'une liste de tableaux ses enfants en placant
% les pions du joueur et de l'ennemi chacun son tour puis en trouvant le
% meilleur a renvoyer en utilisant l'algorithme de minmax
% https://en.wikipedia.org/wiki/Minimax

% le cas ou on arrive au bout de la recursion (vis a vis de la profondeur)
% et ou il ne reste qu'un tableau a tester
minmax([Board], Heur, Player, _, 0, Val, _) :- 
    changePlayer(Player, Opponent),
    value_of(Board, Opponent, Val, Heur), !.

% le cas ou on arrive au bout de la recursion mais il reste plus d'un tableaux
% a tester
minmax([Board1|Tail], Heur, Player, MaximPlayer, 0, BestVal, BestBoard) :- 
    changePlayer(Player, Opponent),
    value_of(Board1, Opponent, Val1, Heur),
    minmax(Tail, Heur, Player, MaximPlayer, 0, Val2, Board2),
    comp_best_val(MaximPlayer, Val1, Board1, Val2, Board2, BestVal, BestBoard), !.

% le cas ou la recursion n'est pas arrivee a son terme et ou la liste a plus
% d'un element
minmax([Board1|Tail], Heur, Player, MaximPlayer, Depth, BestVal, BestBoard) :-
    findall(NextBoard, possible_move(Board1, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    changeMaximizing(MaximPlayer, MaximOppon),
    minmax(PossibleBoards, Heur, Opponent, MaximOppon, NewDepth, Val1, _),
    minmax(Tail, Heur, Player, MaximPlayer, Depth, Val2, Board2),
    comp_best_val(MaximPlayer, Val1, Board1, Val2, Board2, BestVal, BestBoard), !.

% le cas ou la recursion n'est pas arrivee a son terme et ou la liste ne
% contient plus qu'un element
minmax([Board], Heur, Player, MaximPlayer, Depth, Val, Board) :-
    findall(NextBoard, possible_move(Board, NextBoard, Player), PossibleBoards),
    changePlayer(Player, Opponent),
    NewDepth is Depth - 1,
    changeMaximizing(MaximPlayer, MaximOppon),
    minmax(PossibleBoards, Heur, Opponent, MaximOppon, NewDepth, Val, _).

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

% si l'heuristique n'est pas reconnue, on prend une valeur aleatoire
value_of(_, _, Value, _) :-
    Value is random(20), !.

