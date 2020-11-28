:- module(firstHeuristic, [heuristic/3]).

%heuristic(+Board, +Player, -Cout) :
% - Board  : état du plateau après avoir joué le coup
% - Player : numéro du joueur actuel
% - Cout   : cout de la dispostion du plateau
heuristic(Board, Player, FinalCost) :-
    getColumnCostList(Board, Player, CostsListColumn),
    getRowCostList(Board, Player, CostsListRow),
    append(CostsListColumn, CostsListRow, CostsListAll),
    max_list(CostsListAll, MaxCost),
    FinalCost is 4-MaxCost.

%getColumnCostList(+Board, +Player, -List) :
% - Board  : état du plateau après avoir joué le coup
% - Player : numéro du joueur actuel
% - List   : liste des sommes de jetons alignés d un joueur pour chaque colonne du plateau
getColumnCostList([], _, []).
getColumnCostList([ActualColonne|Rest], Player, [Cost|List]) :-
	nonvar(Player),
    reverse(ActualColonne, ReversedColonne),
    sumColumn(Player, ReversedColonne, Cost),
    getColumnCostList(Rest, Player, List).

%sumColumn(+Player, +Column, -Sum) :
% - Player : numéro du joueur actuel
% - Column : colonne sur laquelle on calcule le nombre de jetons alignés
% - Sum    : nombre de jetons alignés du joueur.
sumColumn(_, [], 0).
sumColumn(Player, [H|T], Sum) :-
    var(H),
    sumColumn(Player, T, Sum).
sumColumn(Player, [H|_], 0) :-
    H \= Player.
sumColumn(Player, [H|T], AlignedTokens) :-
    nonvar(H),
    H = Player,
    sumColumn(Player, T, Sum),
    AlignedTokens is Sum+1.

%getRowCostList(+Board, +Player, -List) :
% - Board  : état du plateau après avoir joué le coup
% - Player : numéro du joueur actuel
% - List   : liste des sommes de jetons alignés d un joueur pour chaque ligne du plateau
getRowCostList([[],[],[],[],[],[],[]],_,[]).
getRowCostList([[H1|R1], [H2|R2], [H3|R3], [H4|R4], [H5|R5], [H6|R6], [H7|R7]], Player, [MaxCostRow|List]) :-
    nonvar(Player),
    CurrentLigne = [H1, H2, H3, H4, H5, H6, H7],
    sumRow(Player, CurrentLigne, LastSum, ListCost),
    max_list([LastSum|ListCost], MaxCostRow),
    getRowCostList([R1,R2,R3,R4,R5,R6,R7], Player,List).

%sumColumn(+Player, +Row, -Sum, -ListSum) :
% - Player  : numéro du joueur actuel
% - Row     : ligne sur laquelle on calcule le nombre de jetons alignés
% - Sum     : nombre de jetons alignés du joueur en début de ligne
% - ListSum : liste des sommes des jetons consécutifs du joueur sur une ligne
sumRow(_, [], 0, []).
sumRow(Player, [H|T], Sum, [NewSum|ListSum]) :-
    (var(H) ; H \= Player),
    sumRow(Player, T, NewSum, ListSum),
    Sum is 0.
sumRow(Player, [H|T], Sum, ListSum) :-
    nonvar(H),
    H = Player, 
    sumRow(Player, T, NewSum, ListSum),
    Sum is NewSum+1.

%%% TESTS %%%
testSumRow(Player, Sum, ListSum) :- sumRow(Player, [1, 2, 1, _, 1, 2, 1, 2, 1, 1, 2], Sum, ListSum).
 
testGetColumnCostList(Player, List) :- getColumnCostList([[1, 1, 1, _], [1, 2, 1, _], [1, 2, _, _], [2, 1, 1, _]], Player, List).   
testGetRowCostList(Player, Sum) :- getRowCostList([[1, 1, 1, _], [1, 2, 1, _], [1, 2, _, _], [2, 1, 1, _], [2, 1, 2, _], [2, 2, 2, _], [1, _, _, _]], Player, Sum).

testHeuristic(Player, Cost) :- heuristic([[1, 1, 1, _], [1, 2, 1, _], [1, 2, _, _], [2, 1, 1, _], [2, 1, 2, _], [2, 2, 2, _], [1, _, _, _]], Player, Cost).
