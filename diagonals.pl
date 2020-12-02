:- module(diagonals, [getEveryDescDiags/2, getEveryDescDiagsHalfBoard/3, createOneDescDiag/5]).

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
