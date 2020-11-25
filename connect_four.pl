:- dynamic board/1.

%Regarder si le jeu est fini
gameover(Winner) :- board(Board), winner(Board, Winner), !.
gameover('Draw') :- board(Board), isBoardFull(Board).

winner(Board,Winner) :- winnerToutesColonnes(Board,Winner),!.
winner(Board,Winner) :- winnerToutesLignes(Board,Winner),!.
winner(Board,Winner) :- winnerDiagonaleGauche(Board,Winner),!.
winner(Board,Winner) :- winnerDiagonaleDroite(Board,Winner),!.

winnerColonne([P,Q,R,S,_,_], P) :- P==Q, Q==R, R==S, nonvar(P).
winnerColonne([_,P,Q,R,S,_], P) :- P==Q, Q==R, R==S, nonvar(P).
winnerColonne([_,_,P,Q,R,S], P) :- P==Q, Q==R, R==S,  nonvar(P).

% Itération sur les colonnes pour vérifier si une des colonnes est
% gagnante
winnerToutesColonnes([X|_],Winner) :- winnerColonne(X,Winner),!.
winnerToutesColonnes([_|C],Winner) :- winnerToutesColonnes(C,Winner).

winnerLigne([P,Q,R,S,_,_,_], P) :- P==Q, Q==R, R==S,  nonvar(P).
winnerLigne([_,P,Q,R,S,_,_], P) :- P==Q, Q==R, R==S,  nonvar(P).
winnerLigne([_,_,P,Q,R,S,_], P) :- P==Q, Q==R, R==S,  nonvar(P).
winnerLigne([_,_,_,P,Q,R,S], P) :- P==Q, Q==R, R==S,  nonvar(P).

%Itération sur les lignes
winnerToutesLignes([[A|_], [B|_], [C|_], [D|_], [E|_], [F|_], [G|_]], Winner) :-
winnerLigne([A,B,C,D,E,F,G], Winner) ,!.
winnerToutesLignes([[_|Q1], [_|Q2], [_|Q3], [_|Q4], [_|Q5], [_|Q6], [_|Q7]], Winner) :-
winnerToutesLignes([Q1,Q2,Q3,Q4,Q5,Q6,Q7], Winner) .

winnerDiagonaleGauche([[P,_,_,_,_,_], [_,Q,_,_,_,_], [_,_,R,_,_,_], [_,_,_,S,_,_],_,_,_], P) :- P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([[_,P,_,_,_,_], [_,_,Q,_,_,_], [_,_,_,R,_,_], [_,_,_,_,S,_],_,_,_], P) :- P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([[_,_,P,_,_,_], [_,_,_,Q,_,_], [_,_,_,_,R,_], [_,_,_,_,_,S],_,_,_], P) :- P==Q, Q==R, R==S,  nonvar(P).

winnerDiagonaleGauche([_, [P,_,_,_,_,_], [_,Q,_,_,_,_], [_,_,R,_,_,_], [_,_,_,S,_,_],_,_], P) :- P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([_, [_,P,_,_,_,_], [_,_,Q,_,_,_], [_,_,_,R,_,_], [_,_,_,_,S,_],_,_], P) :- P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([_, [_,_,P,_,_,_], [_,_,_,Q,_,_], [_,_,_,_,R,_], [_,_,_,_,_,S],_,_], P) :- P==Q, Q==R, R==S,  nonvar(P).

winnerDiagonaleGauche([_,_, [P,_,_,_,_,_], [_,Q,_,_,_,_], [_,_,R,_,_,_], [_,_,_,S,_,_],_], P) :- P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([_,_, [_,P,_,_,_,_], [_,_,Q,_,_,_], [_,_,_,R,_,_], [_,_,_,_,S,_],_], P) :- P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([_,_, [_,_,P,_,_,_], [_,_,_,Q,_,_], [_,_,_,_,R,_], [_,_,_,_,_,S],_], P) :- P==Q, Q==R, R==S, nonvar(P).

winnerDiagonaleGauche([_,_,_, [P,_,_,_,_,_], [_,Q,_,_,_,_], [_,_,R,_,_,_], [_,_,_,S,_,_]], P) :- P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([_,_,_,[_,P,_,_,_,_], [_,_,Q,_,_,_], [_,_,_,R,_,_], [_,_,_,_,S,_]], P) :-  P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([_,_,_,[_,_,P,_,_,_], [_,_,_,Q,_,_], [_,_,_,_,R,_], [_,_,_,_,_,S]], P) :-  P==Q, Q==R, R==S, nonvar(P).

winnerDiagonaleDroite(Board,Winner) :- reverse(Board, BoardInverse),
    winnerDiagonaleGauche(BoardInverse, Winner).

isBoardFull([]).
isBoardFull([H|C]) :- isColonneFull(H), isBoardFull(C).
%Itérer sur toutes les colonnes et pour chaque colonne.
isColonneFull(C) :- nth1(6, C, X), nonvar(X).

%On applique le coup joué
applyIt(Board,NewBoard) :- retract(board(Board)), assert(board(NewBoard)).
% Predicate to get the next player
changePlayer('1','2').
changePlayer('2','1').


%IA ernvoie colonne complète
ia(Board, IndexColonne,_) :- repeat, I is random(7), IndexColonne is I+1,  nth1(IndexColonne, Board, Elem), not(isColonneFull(Elem)), write('Chose column '),writeln(IndexColonne), !.



% Game is over, we cut to stop the search, and display the winner.
play(_) :- gameover(Winner), !, write('Game is Over. Winner: '), writeln(Winner), board(Board),displayBoard(Board).
% The game is not over, we play the next turn
play(Player) :- write('New turn for:'), writeln(Player), board(Board), displayBoard(Board),ia(Board, IndexColonne, Player), playMove(Board, IndexColonne, NewBoard, Player),  applyIt(Board, NewBoard), changePlayer(Player,NextPlayer), play(NextPlayer).


% Joue le coup (pour l'instant par du principe que la colonne donnée
% n'est pas pleine


playMove(Board,IndexColonne,NewBoard,P) :- Board=NewBoard,
    nth1(IndexColonne, Board, Colonne),
    updateColonne(Colonne, NewColonne, P),
    nth1(IndexColonne, NewBoard, NewColonne).



updateColonne(Colonne,NewColonne, Player) :- Colonne = NewColonne,
    nth1(Index,Colonne,X), var(X),!,
    nth1(Index, NewColonne, Player).






%Affichage du jeu : ligne par ligne
displayBoard(B) :- writeln('*------------*'),
    printLigne(6,B), printLigne(5,B), printLigne(4,B),
    printLigne(3,B), printLigne(2,B), printLigne(1,B),
    writeln('*------------*').

%Affichage d'une ligne
printLigne(L, B) :- nth1(1,B,C1), printVal(C1,L),
    nth1(2,B,C2), printVal(C2,L),
    nth1(3,B,C3), printVal(C3,L),
    nth1(4,B,C4), printVal(C4,L),
    nth1(5,B,C5), printVal(C5,L),
    nth1(6,B,C6), printVal(C6,L),
    nth1(7,B,C7), printVal(C7,L),
    writeln('').

% Afficher le contenu de la case à l'indice N de la colonne C (?, x or
% o)
printVal(C,N) :- nth1(N,C,Val), var(Val), write('? '), ! .
printVal(C,N) :- nth1(N,C,Val), write(Val), write(' ').


%Début du jeu
init :- length(C1,6), length(C2,6),length(C3,6),
    length(C4,6),length(C5,6),length(C6,6),length(C7,6),
    assert(board([C1,C2,C3,C4,C5,C6,C7])), play('1').




