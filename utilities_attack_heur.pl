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
    sum(Player, CurrentLigne, TransSum, LastSum, Position, ListCost),
    Position >=4,
    MaxSum is max(TransSum, LastSum),
    max_list([MaxSum|ListCost], MaxCostRow),
    getRowCostList([R1,R2,R3,R4,R5,R6,R7], Player,List).
getRowCostList([[H1|R1], [H2|R2], [H3|R3], [H4|R4], [H5|R5], [H6|R6], [H7|R7]], Player, [MaxCostRow|List]) :-
    CurrentLigne = [H1, H2, H3, H4, H5, H6, H7],
    sum(Player, CurrentLigne, _, _, Position, ListCost),
    Position < 4,
    max_list(ListCost, MaxCostRow),
    getRowCostList([R1,R2,R3,R4,R5,R6,R7], Player,List).

% Usage : Compter le nombre de jetons du joueur adverse alignés sans blocage sur une ligne
% sum(+Player, +Row, -TransitionalSum, -Sum, -FreedomDegree, -ListSum) :
% - Player          : numéro du joueur actuel
% - Row             : ligne sur laquelle on calcule le nombre de jetons alignés
% - Sum             : nombre max de jetons alignés du joueur sur une ligne délimité par les jetons du joueur adverse ou des bodures.
%                     ex : [1,_,1,1,2,1,1] -> Sum = 2; [1,_,1,_,2,1,1] -> Sum = 2; [1,_,1,_,1,1,1] -> Sum = 3
% - Sum2            : variable permettant à Sum de récupérer le max entre lui même et une autre variable
% - TransitionalSum : nombre de jetons alignés du joueur sur une ligne délimité par les jetons du joueur adverse,
%                     des bodures ou une case vide.A chaque réinitialisation de TransitionalSum, Sum prend sa valeur 
%                     si TransitionalSum est plus grande.
%                     ex : [1,1,_,...] -> TransitionalSum = 2; [1,_,...] -> TransitionalSum = 1; [_,_,1,1,1,2] -> TransitionalSum = 3
% - FreedomDegree : nombre de case vides ou contenant des jetons adverses alignées (nb cases pouvant contenir un alignement adverse)
% - ListSum : liste des sommes des jetons alignés sur une ligne hors début de ligne
sum(_,[],0,0,0, [0]).
sum(Player, [H|T], TransitionalSum, Sum, FreedomDegree, ListSum) :-
    var(H),
    sum(Player, T, NewTransitionalSum, NewSum, NewFreedomDegree, ListSum),
    FreedomDegree is NewFreedomDegree+1,
    max_list([NewSum,NewTransitionalSum],Sum),
    TransitionalSum is 0.
sum(Player, [H|T], TransitionalSum, NewSum, FreedomDegree, ListSum) :-
    nonvar(H),
    H = Player,
    sum(Player,T, NewTransitionalSum, NewSum, NewFreedomDegree, ListSum),
    FreedomDegree is NewFreedomDegree+1,
    TransitionalSum is NewTransitionalSum+1.
sum(Player, [H|T], TransitionalSum, Sum, FreedomDegree, [Sum2| ListSum]) :-
    nonvar(H),
    H \= Player,
    sum(Player,T, NewTransitionalSum, NewSum, NewFreedomDegree,  ListSum),
    NewFreedomDegree >= 4,
    FreedomDegree is 0,
    max_list([NewSum,NewTransitionalSum],Sum2),
    Sum is 0,
    TransitionalSum is 0.
sum(Player, [H|T], TransitionalSum, Sum, FreedomDegree, ListSum) :-
    nonvar(H),
    H \= Player,
    sum(Player,T,_,_,NewFreedomDegree,ListSum),
    NewFreedomDegree < 4,
    FreedomDegree is 0,
    Sum is 0,
    TransitionalSum is 0.

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
    sum(Player, Diag, TransSum, LastSum, Position, ListCost),
    Position >= 4,
    MaxSum is max(TransSum, LastSum),
    max_list([MaxSum|ListCost], MaxCostDiag),
    sumDiag(Player, Rest, ListSum).
sumDiag(Player, [Diag|Rest], [MaxCostDiag|ListSum]) :-
    sum(Player, Diag, _, _, Position, ListCost),
    Position < 4,
    max_list(ListCost, MaxCostDiag),
    sumDiag(Player, Rest, ListSum).