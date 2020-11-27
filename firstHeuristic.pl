%:- module(firstHeuristic, [firstHeuristic/1]).

%Un Move est l index de la colonne sur laquelle est joué le pion
heuristic(Board, Player, Move, Cout) :-
    colonneAligne(Board, Player, Move, CoutColonne),
    ligneAligne(Board, Player, NumberLigne),
    diagonaleGaucheAligne(Board, Player, NumberDiagG),
    diagonaleDroiteAligne(Board, Player, NumberDiagD),
    Cout is 4-max(CoutColonne, max(NumberLigne, max(NumberDiagG,NumberDiagD))).
    
colonneAligne(Board, Player, Move, CoutColonne) :-
	nonvar(Player),
    nth1(Move, Board, Colonne),
    reverse(Colonne, NewColonne),
    compteColonne(Player, NewColonne, CoutColonne).   

%FIXME: dans le cas ou il y a des cases vides dans le tableau ça compte le resultat comme un 1 (cf test)
%compteColonne(+Player, +Colonne, -CoutColonne)
%Cette methode compte le nombre de pions du joueur Player alignés jusqu à trouver un jeton de l autre joueur.
%Ce nombre calculé se trouve dans la variable CoutColonne.
%Player est le numéro du joueur actuel
%Colonne est la colonne sur laquelle on calcule le nombre de jetons alignés
compteColonne(_, [], 0).
compteColonne(Player, [H|_], 0) :-
    H \= Player.
compteColonne(Player, [H|T], NumberColonne) :-
    H = Player,
    compteColonne(Player, T, Compteur),
    NumberColonne is Compteur+1.
    
testColonneAligne(X, Y) :- colonneAligne([[1, 1, 2, 1], [1, 2, 1, 1], [1, 1, 1, 1], [2, 1, 1, _]], 1, X, Y).
    
%TODO: faire la definition de ligneAligne, diagonaleGaucheAligne et diagonaleDroiteAligne
ligneAligne(_, _, 0).
diagonaleGaucheAligne(_, _, 0).
diagonaleDroiteAligne(_, _, 0).