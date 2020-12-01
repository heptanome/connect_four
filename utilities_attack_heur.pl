:- module(utilities_attack_heur, [getColumnCostList/3, getRowCostList/3,
    getAscendingDiagsCostList/3, getDescendingDiagsCostList/3]).

:- use_module(utilities_heuristics, [getEveryDescDiags/2, reverseEveryColumns/2, 
    getEveryDescDiagsHalfBoard/3, createOneDescDiag/5]).

% Usage : Obtenir le nombre de jetons consécutifs alignés du joueur actuel sur chaque colonne du plateau
%         On ne compte que les jetons du joueur qui sont au dessus du plus haut jeton du joueur opposé
% getColumnCostList(+Board, +Player, -List) :
% - Board  : état du plateau après avoir joué le coup
% - Player : numéro du joueur actuel
% - List   : liste des sommes de jetons alignés du joueur pour chaque colonne du plateau
getColumnCostList([], _, []).
getColumnCostList([ActualColonne|Rest], Player, [Cost|List]) :-
    reverse(ActualColonne, ReversedColonne),
    sumColumn(Player, ReversedColonne, Cost),
    getColumnCostList(Rest, Player, List).

% Usage : Compter le nombre de jetons consécutifs alignés du joueur actuel sur une colonne
%         On ne compte que les jetons du joueur qui sont au dessus du plus haut jeton du joueur opposé
% sumColumn(+Player, +Column, -Sum) :
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

% Usage : Obtenir le nombre de jetons consécutifs alignés du joueur actuel sur chaque ligne du plateau
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

% Usage : Compter le nombre de jetons consécutifs alignés du joueur actuel sur une ligne
% sumRow(+Player, +Row, -Sum, -ListSum) :
% - Player  : numéro du joueur actuel
% - Row     : ligne sur laquelle on calcule le nombre de jetons alignés
% - Sum     : nombre de jetons alignés du joueur en début de ligne
% - ListSum : liste des sommes des jetons consécutifs du joueur sur une ligne hors début de ligne
sumRow(_, [], 0, []).
sumRow(Player, [H|T], Sum, [NewSum|ListSum]) :-
    var(H),
    sumRow(Player, T, NewSum, ListSum),
    Sum is 0.
sumRow(Player, [H|T], Sum, ListSum) :-
    H \= Player,
    sumRow(Player, T, _, ListSum),
    Sum is 0.
sumRow(Player, [H|T], Sum, ListSum) :-
    nonvar(H),
    H = Player, 
    sumRow(Player, T, NewSum, ListSum),
    Sum is NewSum+1.

% Usage : Obtenir le nombre de jetons consécutifs alignés du joueur actuel 
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

% Usage : Obtenir le nombre de jetons consécutifs alignés du joueur actuel 
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
