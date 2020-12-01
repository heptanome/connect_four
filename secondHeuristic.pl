:- module(secondHeuristic, [heuristicSecond/3]).

% Usage : Obtenir le coût de la dispotion actuelle du plateau en
%         cherchant le nombre maximum de jetons alignés du joueur actuel sur le plateau.
%         Ces jetons peuvent être aligné sur une ligne, une colonne ou une diagonale.
% heuristicSecond(+Board, +Player, -FinalCost) :
% - Board     : état du plateau après avoir joué le coup
% - Player    : numéro du joueur actuel
% - FinalCost : cout de la dispostion du plateau
heuristicSecond(Board, Player, FinalCost) :-
    nonvar(Player),
    getColumnCostList(Board, Player, CostsListColumn),
    write('Column     : '),
    printVal(CostsListColumn),
    max_list(CostsListColumn, MaxCostColumn),
    getRowCostList(Board, Player, CostsListRow),
    write('Row        : '),
    printVal(CostsListRow),
    max_list(CostsListRow, MaxCostRow),
    getDescendingDiagsCostList(Board, Player, CostsListDescDiags),
    write('Desc Diags : '),
    printVal(CostsListDescDiags),
    max_list(CostsListDescDiags, MaxCostDescDiags),
    getAscendingDiagsCostList(Board, Player, CostsListAscDiags),
    write('Asc Diags  : '),
    printVal(CostsListAscDiags),
    writeln(''),
    max_list(CostsListAscDiags, MaxCostAscDiags),
    Max is MaxCostColumn + MaxCostRow + MaxCostDescDiags + MaxCostAscDiags,
    FinalCost is 16-Max.

printVal([]) :-
    writeln('').
printVal([H|T]) :-
    write(H), write(' '), printVal(T).

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

% Usage : Créer une liste des diagonales descendantes N°4 à 9.
% getEveryDescDiags(+Board, -CompleteListDiags)
% - Board             : état du plateau après avoir joué le coup
% - CompleteListDiags : liste de toutes les diagonales descendantes du plateau
getEveryDescDiags(Board, CompleteListDiags) :-
    getEveryDescDiagsHalfBoard(Board, 4, ListDiags1),
    reverse(Board, ReversedBoard),
    reverseEveryColumns(ReversedBoard, ReversedColumnsBoard),
    getEveryDescDiagsHalfBoard(ReversedColumnsBoard, 4, ListDiags2),
    reverse(ListDiags2, ReversedListDiags2),
    reverseEveryColumns(ReversedListDiags2, ReversedColumnsListDiags2),
    append(ListDiags1, ReversedColumnsListDiags2, CompleteListDiags).

% Usage : Inverser toutes les colonnes du plateau.
% reverseEveryColumns(+Board, -ReversedBoard)
% - Board         : état du plateau après avoir joué le coup
% - ReversedBoard : nouveau plateau avec des colonnes inversées par rapport à Board 
reverseEveryColumns([], []).
reverseEveryColumns([Column|Rest], [ReversedColumn|ReversedRest]) :-
    reverse(Column, ReversedColumn),
    reverseEveryColumns(Rest, ReversedRest).

% Usage : Créer une liste de diagonales de la moitié du plateau Board.
%         Les numéros de ces diagonales sont compris entre Rank et 6.
%         Rank est un entier entre 1 et 6 inclus.
% getEveryDescDiagsHalfBoard(+Board, +Rank, -ListDiags)
% - Board     : état du plateau après avoir joué le coup
% - Rank      : rang à partir duquel on veut les diagonales
% - ListDiags : listes des diagonales de numéro Rank au numéro 6
getEveryDescDiagsHalfBoard(_, 7, []).
getEveryDescDiagsHalfBoard(Board, Rank, [Diag|Rest]) :-
    createOneDescDiag(Board, Rank, 1, Rank, Diag),
    NextRank is Rank + 1,
    getEveryDescDiagsHalfBoard(Board, NextRank, Rest).

% Usage : Créer une liste représentant une diagonale de numéro Rank
% createOneDescDiag(+Board, +Rank, 1, +Rank, -Diag)
% - Board       : état du plateau après avoir joué le coup
% - Rank        : rang de la diagonale créée (Entier entre 1 et 6 inclus)
% - IndexColumn : index de la colonne du jeton à mettre dans la diagonale (initialisé à 1 au premier appel)
% - IndexToken  : index du jeton dans sa colonne (initialisé à Rank au premier appel)
% - Diag        : diagonale créée
createOneDescDiag(_, Rank, IndexColumn, 0, []) :-
	IndexColumn is Rank + 1,!.
createOneDescDiag(Board, Rank, IndexColumn, IndexToken, [Token|Rest]) :-
    nth1(IndexColumn, Board, Column),
    nth1(IndexToken, Column, Token),
    NewIndexColumn is IndexColumn+1,
    NewIndexToken is IndexToken-1,
    createOneDescDiag(Board, Rank, NewIndexColumn, NewIndexToken, Rest).
