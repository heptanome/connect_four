:- module(winner, [winner/2]).

winner(Board,Winner) :-
    winnerToutesColonnes(Board,Winner),!.
winner(Board,Winner) :-
    winnerToutesLignes(Board,Winner),!.
winner(Board,Winner) :-
    winnerDiagonaleGauche(Board,Winner),!.
winner(Board,Winner) :-
    winnerDiagonaleDroite(Board,Winner),!.

winnerColonne([P,Q,R,S,_,_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerColonne([_,P,Q,R,S,_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerColonne([_,_,P,Q,R,S], P) :-
    P==Q, Q==R, R==S,  nonvar(P).

%Iteration sur les colonnes pour verifier si une des colonnes est gagnante
winnerToutesColonnes([X|_],Winner) :-
    winnerColonne(X,Winner),!.
winnerToutesColonnes([_|C],Winner) :-
    winnerToutesColonnes(C,Winner).

winnerLigne([P,Q,R,S,_,_,_], P) :-
    P==Q, Q==R, R==S,  nonvar(P).
winnerLigne([_,P,Q,R,S,_,_], P) :-
    P==Q, Q==R, R==S,  nonvar(P).
winnerLigne([_,_,P,Q,R,S,_], P) :-
    P==Q, Q==R, R==S,  nonvar(P).
winnerLigne([_,_,_,P,Q,R,S], P) :-
    P==Q, Q==R, R==S,  nonvar(P).

%Iteration sur les lignes
winnerToutesLignes([[A|_], [B|_], [C|_], [D|_], [E|_], [F|_], [G|_]], Winner) :-
    winnerLigne([A,B,C,D,E,F,G], Winner) ,!.
winnerToutesLignes([[_|Q1], [_|Q2], [_|Q3], [_|Q4], [_|Q5], [_|Q6], [_|Q7]], Winner) :-
    winnerToutesLignes([Q1,Q2,Q3,Q4,Q5,Q6,Q7], Winner) .

winnerDiagonaleGauche([[P,_,_,_,_,_], [_,Q,_,_,_,_], [_,_,R,_,_,_], [_,_,_,S,_,_],_,_,_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([[_,P,_,_,_,_], [_,_,Q,_,_,_], [_,_,_,R,_,_], [_,_,_,_,S,_],_,_,_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([[_,_,P,_,_,_], [_,_,_,Q,_,_], [_,_,_,_,R,_], [_,_,_,_,_,S],_,_,_], P) :-
    P==Q, Q==R, R==S,  nonvar(P).

winnerDiagonaleGauche([_, [P,_,_,_,_,_], [_,Q,_,_,_,_], [_,_,R,_,_,_], [_,_,_,S,_,_],_,_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([_, [_,P,_,_,_,_], [_,_,Q,_,_,_], [_,_,_,R,_,_], [_,_,_,_,S,_],_,_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([_, [_,_,P,_,_,_], [_,_,_,Q,_,_], [_,_,_,_,R,_], [_,_,_,_,_,S],_,_], P) :-
    P==Q, Q==R, R==S,  nonvar(P).

winnerDiagonaleGauche([_,_, [P,_,_,_,_,_], [_,Q,_,_,_,_], [_,_,R,_,_,_], [_,_,_,S,_,_],_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([_,_, [_,P,_,_,_,_], [_,_,Q,_,_,_], [_,_,_,R,_,_], [_,_,_,_,S,_],_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([_,_, [_,_,P,_,_,_], [_,_,_,Q,_,_], [_,_,_,_,R,_], [_,_,_,_,_,S],_], P) :-
    P==Q, Q==R, R==S, nonvar(P).

winnerDiagonaleGauche([_,_,_, [P,_,_,_,_,_], [_,Q,_,_,_,_], [_,_,R,_,_,_], [_,_,_,S,_,_]], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([_,_,_,[_,P,_,_,_,_], [_,_,Q,_,_,_], [_,_,_,R,_,_], [_,_,_,_,S,_]], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerDiagonaleGauche([_,_,_,[_,_,P,_,_,_], [_,_,_,Q,_,_], [_,_,_,_,R,_], [_,_,_,_,_,S]], P) :-
    P==Q, Q==R, R==S, nonvar(P).

winnerDiagonaleDroite(Board,Winner) :-
    reverse(Board, BoardInverse), winnerDiagonaleGauche(BoardInverse, Winner).
