:- module(utilities, [updateColonne/3, isColonneFull/1]).

updateColonne(Colonne,NewColonne, Player) :-
    Colonne = NewColonne,
    nth1(Index,Colonne,X), var(X),!,
    nth1(Index, NewColonne, Player).

isColonneFull(C) :-
    nth1(6, C, X), nonvar(X).
