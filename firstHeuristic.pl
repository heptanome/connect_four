%:- module(firstHeuristic, [firstHeuristic/1]).

%Une heuristique comprend
% - Player : 1 ou 2
% - Move : indice de la colonne dans laquelle le coup est joué
% - CoutColonne : nb de jeton max aligné dans une colonne (en bordure superieure)
heuristic(Board, Player, Move, Cout) :-
    colonneAligne(Board, Player, Move, CoutColonne),
    ligneAligne(Board, Player, NumberLigne),
    diagonaleGaucheAligne(Board, Player, NumberDiagG),
    diagonaleDroiteAligne(Board, Player, NumberDiagD),
    Cout is 4-max(CoutColonne, max(NumberLigne, max(NumberDiagG,NumberDiagD))).

%Cette methode compte le nombre de pions alignés d un joueur sur une colonne en partant de la fin de la colonne.
%compteAligne(+Board, +Player, +Move, -CoutColonne) :
% - Board est le plateau de jeu actuel
% - Player est le numéro du joueur actuel
% - Move est indice de la colonne sur laquelle on calcule le nombre de jetons alignés
% - CoutColonne est le nombre de jetons alignés du joueur.   
colonneAligne(Board, Player, Move, CoutColonne) :-
	nonvar(Player),
    nth1(Move, Board, Colonne),
    reverse(Colonne, NewColonne),
    compteColonne(Player, NewColonne, CoutColonne).   

%Cette methode compte le nombre de pions du joueur Player alignés jusqu à trouver un jeton de l autre joueur.
%compteColonne(+Player, +Colonne, -CoutColonne) :
% - Player est le numéro du joueur actuel
% - Colonne est la colonne sur laquelle on calcule le nombre de jetons alignés
% - CoutColonne est le nombre de jetons alignés du joueur.
compteColonne(_, [], 0).
compteColonne(Player, [H|T], Compteur) :-
    var(H), !,
    compteColonne(Player, T, Compteur).
compteColonne(Player, [H|_], 0) :-
    H \= Player.
compteColonne(Player, [H|T], NumberColonne) :-
    H = Player,
    compteColonne(Player, T, Compteur),
    NumberColonne is Compteur+1.
    
testColonneAligne(Player, IndiceColonne, Cout) :- colonneAligne([[1, 1, 1, _], [1, 2, 1, 1], [1, 1, 1, 1], [2, 1, 1, _]], Player, IndiceColonne, Cout).
    
%TODO: faire la definition de ligneAligne, diagonaleGaucheAligne et diagonaleDroiteAligne
ligneAligne(_, _, 0).
diagonaleGaucheAligne(_, _, 0).
diagonaleDroiteAligne(_, _, 0).