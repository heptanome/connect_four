%:- module(firstHeuristic, [firstHeuristic/1]).

%heuristic(+Board, +Player, -Cout)
%Une heuristique comprend
% - Board : état du plateau après avoir joué le coup
% - Player : numéro du joueur 1 ou 2
% - Cout : cout du board
heuristic(Board, Player, Cout) :-
    getListCoutColonne(Board, Player, ListCouts),
    max_list(ListCouts, CoutMaxColonne),
    Cout is 4-CoutMaxColonne.

%Cette methode compte le nombre de pions alignés d un joueur sur une colonne en partant de la fin de la colonne.
%compteAligne(+Board, +Player, +Move, -CoutColonne) :
% - Board est le plateau de jeu actuel
% - Player est le numéro du joueur actuel
% - Move est indice de la colonne sur laquelle on calcule le nombre de jetons alignés
% - CoutColonne est le nombre de jetons alignés du joueur.   
getListCoutColonne([], _, []).
getListCoutColonne([ActualColonne|Rest], Player, [Cout|List]) :-
	nonvar(Player),
    reverse(ActualColonne, ReversedColonne),
    compteColonne(Player, ReversedColonne, Cout),
    getListCoutColonne(Rest, Player, List).

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
    
testColonneAligne(Player, List) :- getListCoutColonne([[1, 1, 1, _], [1, 2, 1, _], [1, 2, _, _], [2, 1, 1, _]], Player, List).
testHeuristic(Player, Cout) :- heuristic([[1, 2, 1, _], [1, 2, 2, _], [1, 2, _, _], [2, 2, 2, _]], Player, Cout).
