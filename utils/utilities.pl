:- module(utilities, [updateColumn/3, isColumnFull/1,changePlayer/2, gameover/1]).

% Usage : Jouer un jeton du joueur Player dans une colonne spécifique.
%         Ceci revient à ajouter un jeton du joueur dans la première case vide de la colonne
updateColumn(Column, NewColumn, Player) :-
    Column = NewColumn,
    nth1(Index, Column, Value),
    var(Value),
    !,
    nth1(Index, NewColumn, Player).

% Usage : Vérifier si une colonne est pleine
isColumnFull(Column) :-
    nth1(6, Column, Value), nonvar(Value).

% Usage : Intervertir les numéros de joueur
changePlayer('1','2').
changePlayer('2','1').a

% Usage : regarder si le jeu est fini
gameover(Winner) :- board(Board), winner(Board,Winner, !.
gameover('Draw') :- board(Board), isBoardFull(Board).
