:- module(defense_heur, [heuristic_def/3]).
:- use_module(utilities_heuristics, [getEveryDescDiags/2, reverseEveryColumns/2, 
    getEveryDescDiagsHalfBoard/3, createOneDescDiag/5]).
% Usage : Obtenir le coût de la dispotion actuelle du plateau en
%         cherchant le nombre maximum de jetons alignés du joueur adverse sur le plateau.
%         Ces jetons peuvent être aligné sur une ligne, une colonne ou une diagonale.
% heuristic(+Board, +Player, -FinalCost) :
% - Board     : état du plateau après avoir joué le coup
% - Player    : numéro du joueur actuel
% - FinalCost : cout de la dispostion du plateau
heuristic_def(Board, Player, FinalCost) :-
    nonvar(Player),
    getColumnCostList(Board, Player, CostsListColumn),
    max_list(CostsListColumn, MaxCostColumn),
    getRowCostList(Board, Player, CostsListRow),
    max_list(CostsListRow, MaxCostRow),
    getDescendingDiagsCostList(Board, Player, CostsListDescDiags),
    max_list(CostsListDescDiags, MaxCostDescDiags),
    getAscendingDiagsCostList(Board, Player, CostsListAscDiags),
    max_list(CostsListAscDiags, MaxCostAscDiags),
    FinalCost is max(MaxCostColumn, max(MaxCostRow, max(MaxCostDescDiags, MaxCostAscDiags))).

% Usage : Obtenir le nombre de jetons consécutifs alignés du joueur adverse sur chaque colonne du plateau
%         On ne compte que les jetons du joueur qui sont au dessus du plus haut jeton du joueur actuel
% getColumnCostList(+Board, +Player, -List) :
% - Board  : état du plateau après avoir joué le coup
% - Player : numéro du joueur actuel
% - List   : liste des sommes de jetons alignés du joueur pour chaque colonne du plateau
getColumnCostList([], _, []).
getColumnCostList([ActualColonne|Rest], Player, [Cost|List]) :-
    reverse(ActualColonne, ReversedColonne),
    sumColumn(Player, ReversedColonne, Cost),
    getColumnCostList(Rest, Player, List).

% Usage : Compter le nombre de jetons consécutifs alignés du joueur adverse sur une colonne
%         On ne compte que les jetons du joueur qui sont au dessus du plus haut jeton du joueur actuel
% sumColumn(+Player, +Column, -Sum) :
% - Player : numéro du joueur actuel
% - Column : colonne sur laquelle on calcule le nombre de jetons alignés
% - Sum : nombre de jetons alignés du joueur
% adverse en haut de la colonne
sumColumn(_, [], 0).
sumColumn(Player, [H|T], Sum) :-
    var(H),
    sumColumn(Player, T, Sum).
sumColumn(Player, [H|_], 0) :-
    nonvar(H),
    H = Player.
sumColumn(Player, [H|T], AlignedTokens) :-
    H \= Player,
    sumColumn(Player, T, Sum),
    AlignedTokens is Sum+1.

% Usage : Obtenir le nombre de jetons consécutifs alignés du joueur adverse sur chaque ligne du plateau
% getRowCostList(+Board, +Player, -List) :
% - Board  : état du plateau après avoir joué le coup
% - Player : numéro du joueur actuel
% - List   : liste des sommes de jetons alignés du joueur pour chaque ligne du plateau
getRowCostList([[],[],[],[],[],[],[]],_,[]).
getRowCostList([[H1|R1], [H2|R2], [H3|R3], [H4|R4], [H5|R5], [H6|R6], [H7|R7]], Player, [MaxCostRow|List]) :-
    CurrentLigne = [H1, H2, H3, H4, H5, H6, H7],
    sumRow(Player, CurrentLigne, LastSum, ListCost),
    max_list([LastSum|ListCost], MaxCostRow),
    getRowCostList([R1,R2,R3,R4,R5,R6,R7], Player,List).

% Usage : Compter le nombre de jetons consécutifs alignés du joueur adverse sur une ligne
% sumRow(+Player, +Row, -Sum, -ListSum) :
% - Player  : numéro du joueur actuel
% - Row     : ligne sur laquelle on calcule le nombre de jetons alignés
% - Sum     : nombre de jetons alignés du joueur en début de ligne
% - ListSum : liste des sommes des jetons consécutifs du joueur sur une ligne hors début de ligne
sumRow(_, [], 0, []).
sumRow(Player, [H|T], Sum, [NewSum|ListSum]) :-
    (var(H) ; H = Player),
    sumRow(Player, T, NewSum, ListSum),
    Sum is 0.
sumRow(Player, [H|T], Sum, ListSum) :-
    nonvar(H),
    H \= Player,
    sumRow(Player, T, NewSum, ListSum),
    Sum is NewSum+1.

% Usage : Obtenir le nombre de jetons consécutifs alignés du joueur adverse
%         sur les diagonales ascendantes numéro 4 à 9.
%         On ne prend pas en compte les diagonales 1, 2, 3, 10, 11 et 12,
%         car on ne peut aligner 4 jetons desssus.
% getAscendingDiagsCostList(+Board, +Player, -List):
% - Board  : état du plateau après avoir joué le coup
% - Player : numéro du joueur actuel
% - List   : liste des sommes de jetons consécutifs du joueur pour les diagonales ascendantes 4 à 9
getAscendingDiagsCostList(Board, Player, List) :-
    reverse(Board, ReversedBoard),
    getDescendingDiagsCostList(ReversedBoard, Player, List).

% Usage : Obtenir le nombre de jetons consécutifs alignés du joueur adverse
%         sur les diagonales descendantes numéro 4 à 9.
%         On ne prend pas en compte les diagonales 1, 2, 3, 10, 11 et 12,
%         car on ne peut aligner 4 jetons desssus.
% getDescendingDiagsCostList(+Board, +Player, -List):
% - Board  : état du plateau après avoir joué le coup
% - Player : numéro du joueur actuel
% - List   : liste des sommes de jetons consécutifs du joueur pour les diagonales descendantes N°4 à 9
getDescendingDiagsCostList(Board, Player, List) :-
    getEveryDescDiags(Board, CompleteListDiags),
    sumDiag(Player, CompleteListDiags, List).

sumDiag(_, [], []).
sumDiag(Player, [Diag|Rest], [MaxCostDiag|ListSum]) :-
    sumRow(Player, Diag, LastSum, ListCost),
    max_list([LastSum|ListCost], MaxCostDiag),
    sumDiag(Player, Rest, ListSum).


%%% TESTS %%%
board([['1', '1', '1', '2', '1', _], ['1', '2', '1', _, _, _], ['1', '2', '2', '2', _, _], ['2', '1', '1', '1', '2', '2'], ['2', '1', '2', '1', '2', _], ['2', '2', '2', '1', '1', _], ['1', _, _, _, _, _]]).
board2([['1', '2', _, _, _, _], ['1', _, _, _, _, _], [_, _, _, _, _, _], [_, _, _, _, _, _], [_, _, _,_, _, _], ['2', _, _, _, _, _], [_, _, _, _, _, _]]).


%%% SOMMES DES JETONS SUR LES COLONNES
testSumRow(Player, Sum, ListSum) :- sumRow(Player, [1, 2, 1, _, 1, 2, 1, 2, 1, 1, 2], Sum, ListSum).
testGetColumnCostList(Player, List) :- board(Board), getColumnCostList(Board, Player, List).

%%% SOMMES DES JETONS SUR LES LIGNES
testGetRowCostList(Player, Sum) :- board(Board), getRowCostList(Board, Player, Sum).

%%% SOMMES DES JETONS SUR LES DIAGONALES
testGetDescDiagsCostList(Player, Sum) :- board(Board), getDescendingDiagsCostList(Board, Player, Sum).
testGetAscDiagsCostList(Player, Sum) :- board(Board), getAscendingDiagsCostList(Board, Player, Sum).

%%% HEURISTIC
testHeuristic(Player, Cost) :- board(Board), heuristic_def(Board, Player, Cost).
