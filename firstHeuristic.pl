%:- module(firstHeuristic, [firstHeuristic/1]).

%heuristic(+Board, +Player, -Cout) :
% - Board  : état du plateau après avoir joué le coup
% - Player : numéro du joueur actuel
% - Cout   : cout de la dispostion du plateau
heuristic(Board, Player, FinalCost) :-
    nonvar(Player),
    getColumnCostList(Board, Player, CostsListColumn),
    max_list(CostsListColumn, MaxCostColumn),
    getRowCostList(Board, Player, CostsListRow),
    max_list(CostsListRow, MaxCostRow),
    getRightDiagsCostList(Board, Player, CostsListRightDiags),
    max_list(CostsListRightDiags, MaxCostRightDiags),
    getLeftDiagsCostList(Board, Player, CostsListLeftDiags),
    max_list(CostsListLeftDiags, MaxCostLeftDiags),
    FinalCost is 4-max(MaxCostColumn, max(MaxCostRow, max(MaxCostRightDiags, MaxCostLeftDiags))).

%getColumnCostList(+Board, +Player, -List) :
% - Board  : état du plateau après avoir joué le coup
% - Player : numéro du joueur actuel
% - List   : liste des sommes de jetons alignés d un joueur pour chaque colonne du plateau
getColumnCostList([], _, []).
getColumnCostList([ActualColonne|Rest], Player, [Cost|List]) :-
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
    CurrentLigne = [H1, H2, H3, H4, H5, H6, H7],
    sumRow(Player, CurrentLigne, LastSum, ListCost),
    max_list([LastSum|ListCost], MaxCostRow),
    getRowCostList([R1,R2,R3,R4,R5,R6,R7], Player,List).

%sumRow(+Player, +Row, -Sum, -ListSum) :
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

getLeftDiagsCostList(Board, Player, ListSum) :-
    reverse(Board, ReversedBoard),
    getRightDiagsCostList(ReversedBoard, Player, ListSum).

getRightDiagsCostList(Board, Player, ListSum) :-
    getEveryRightDiags(Board, CompleteListDiags),
    sumDiag(Player, CompleteListDiags, ListSum).

sumDiag(_, [], []).
sumDiag(Player, [Diag|Rest], [MaxCostDiag|ListSum]) :-
    sumRow(Player, Diag, LastSum, ListCost),
    max_list([LastSum|ListCost], MaxCostDiag),
    sumDiag(Player, Rest, ListSum).

%Crée une liste des 6 diagonales droites centrées au milieu du plateau.
%Elles correspondent aux seules diagonales à droite où 4 jetons peuvent être alignés.
%getEveryRightDiags(+Board, -CompleteListDiags)
% - Board             : état du plateau après avoir joué le coup
% - CompleteListDiags : Liste de toutes les diagonales droites du plateau
getEveryRightDiags(Board, CompleteListDiags) :-
    getEveryDiagsHalfBoard(Board, 4, ListDiags1),
    reverse(Board, ReversedBoard),
    reverseEveryColumns(ReversedBoard, ReversedColumnsBoard),
    getEveryDiagsHalfBoard(ReversedColumnsBoard, 4, ListDiags2),
    reverse(ListDiags2, ReversedListDiags2),
    reverseEveryColumns(ReversedListDiags2, ReversedColumnsListDiags2),
    append(ListDiags1, ReversedColumnsListDiags2, CompleteListDiags).

%Renverse toutes les colonnes du plateau.
%reverseEveryColumns(+Board, -ReversedBoard)
% - Board         : état du plateau après avoir joué le coup
% - ReversedBoard : nouveau plateau avec des colonnes inversées par rapport à Board 
reverseEveryColumns([], []).
reverseEveryColumns([Column|Rest], [ReversedColumn|ReversedRest]) :-
    reverse(Column, ReversedColumn),
    reverseEveryColumns(Rest, ReversedRest).

%Crée une liste de diagonales de la moitié du plateau Board.
%Les numéros de ces diagonales sont compris entre Rank et 6.
%Rank est un entier entre 1 et 6 inclus.
%getEveryDiagsHalfBoard(+Board, +Rank, -ListDiags)
% - Board     : état du plateau après avoir joué le coup
% - Rank      : rang à partir duquel on veut les diagonales
% - ListDiags : listes des diagonales de rang Rank au rang 6
getEveryDiagsHalfBoard(_, 7, []).
getEveryDiagsHalfBoard(Board, Rank, [Diag|Rest]) :-
    createOneRightDiag(Board, Rank, 1, Rank, Diag),
    NextRank is Rank + 1,
    getEveryDiagsHalfBoard(Board, NextRank, Rest).

%Crée une liste Diag représentant une diagonale de rang Rank du plateau Board
%createOneRightDiag(+Board, +Rank, 1, +Rank, -Diag)
% - Board       : état du plateau après avoir joué le coup
% - Rank        : rang de la diagonale créée (Entier entre 1 et 6 inclus)
% - IndexColumn : index de la colonne du jeton à mettre dans la diagonale (initialisé à 1 au premier appel)
% - IndexToken  : index du jeton dans sa colonne (initialisé à Rank au premier appel)
% - Diag        : diagonale créée
createOneRightDiag(_, Rank, IndexColumn, 0, []) :-
	IndexColumn is Rank + 1,!.
createOneRightDiag(Board, Rank, IndexColumn, IndexToken, [Token|Rest]) :-
    nth1(IndexColumn, Board, Column),
    nth1(IndexToken, Column, Token),
    NewIndexColumn is IndexColumn+1,
    NewIndexToken is IndexToken-1,
    createOneRightDiag(Board, Rank, NewIndexColumn, NewIndexToken, Rest).
	
    
%%% TESTS %%%
board([[1, 1, 1, 2, 1, _], [1, 2, 1, _, _, _], [1, 2, 2, 2, _, _], [2, 1, 1, 1, 2, 2], [2, 1, 2, 1, 2, _], [2, 2, 2, 1, 1, _], [1, _, _, _, _, _]]).

%%% SOMMES DES JETONS SUR LES COLONNES
testSumRow(Player, Sum, ListSum) :- sumRow(Player, [1, 2, 1, _, 1, 2, 1, 2, 1, 1, 2], Sum, ListSum).
testGetColumnCostList(Player, List) :- board(Board), getColumnCostList(Board, Player, List).

%%% SOMMES DES JETONS SUR LES LIGNES
testGetRowCostList(Player, Sum) :- board(Board), getRowCostList(Board, Player, Sum).

%%% CREATION DE TOUTES LES DIAGONALES INTERESSANTES
testCreateDiag(Rank, Diag) :- board(Board), createOneRightDiag(Board, Rank, 1, Rank, Diag).
testGetEveryDiagsHalfBoard(Start, ListDiags) :- board(Board), getEveryDiagsHalfBoard(Board, Start, ListDiags).
testGetEveryRightDiags(ListDiags) :- board(Board), getEveryRightDiags(Board, ListDiags).
testReverseBoard(ReversedColumnsBoard) :- board(Board), reverse(Board, ReversedBoard), reverseEveryColumns(ReversedBoard, ReversedColumnsBoard).

%%% SOMMES DES JETONS SUR LES DIAGONALES
testSumDiag(Player, Result) :- board(Board), getEveryRightDiags(Board, CompleteListDiags), sumDiag(Player, CompleteListDiags, Result).
testGetRightDiagsCostList(Player, Sum) :- board(Board), getRightDiagsCostList(Board, Player, Sum).
testGetLeftDiagsCostList(Player, Sum) :- board(Board), getLeftDiagsCostList(Board, Player, Sum).

%%% HEURISTIC
testHeuristic(Player, Cost) :- board(Board), heuristic(Board, Player, Cost).