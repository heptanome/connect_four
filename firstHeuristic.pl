%:- module(firstHeuristic, [firstHeuristic/3]).

%Un Move est l index de la colonne sur laquelle est joué le pion
heuristic(Board, Player, Move, Cout) :-
    colonneAligne(Board, Player, Move, NumberColonne),
    ligneAligne(Board, Player, NumberLigne),
    diagonaleGaucheAligne(Board, Player, NumberDiagG),
    diagonaleDroiteAligne(Board, Player, NumberDiagD),
    Cout is 4-max(NumberColonne, max(NumberLigne, max(NumberDiagG,NumberDiagD))).
    
colonneAligne(Board, Player, Move, NumberColonne) :-
	nonvar(Player),
    nth1(Move, Board, Colonne),
    reverse(Colonne, NewColonne),
    compteColonne(Player, NewColonne, NumberColonne).   

%FIXME: dans le cas ou il y a des cases vides dans le tableau ça compte le resultat comme un 1 (cf test)
compteColonne(_, [], 0).
compteColonne(Player, [H|T], NumberColonne) :-
    H = Player,
    compteColonne(Player, T, Compteur),
    NumberColonne is Compteur+1.
compteColonne(Player, [H|_], 0) :-
    H \= Player.
    
testColonneAligne(X, Y) :- colonneAligne([[1, 1, 2, 1], [1, 2, 1, 1], [1, 1, 1, 1], [2, 1, 1, _]], 1, X, Y).
    
%TODO: faire la definition de ligneAligne, diagonaleGaucheAligne et diagonaleDroiteAligne
ligneAligne(_, _, 0).
diagonaleGaucheAligne(_, _, 0).
diagonaleDroiteAligne(_, _, 0).