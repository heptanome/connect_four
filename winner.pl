:- module(winner, [winner/2]).

% Usage : Vérifier si il y a un gagnant.
%         On vérifie si une position gagnante est atteinte sur :
%         - une colonne
%         - une ligne
%         - une diagonale ascendante
%         - une diagonale descendate.
winner(Board,Winner) :-
    winnerWithAColumn(Board,Winner),!.
winner(Board,Winner) :-
    winnerWithARow(Board,Winner),!.
winner(Board,Winner) :-
    winnerWithADescendingDiag(Board,Winner),!.
winner(Board,Winner) :-
    winnerWithAnAscendingDiag(Board,Winner),!.

% Usage : Itérer sur les colonnes pour vérifier si une des colonnes est gagnante.
winnerWithAColumn([Column|_],Winner) :-
    winingColumn(Column,Winner),!.
winnerWithAColumn([_|NextColumns],Winner) :-
    winnerWithAColumn(NextColumns,Winner).
    
% Usage : Définir les positions gagnantes pour une colonne.
winingColumn([P,Q,R,S,_,_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winingColumn([_,P,Q,R,S,_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winingColumn([_,_,P,Q,R,S], P) :-
    P==Q, Q==R, R==S,  nonvar(P).

% Usage : Itérer sur les lignes pour vérifier si une des lignes est gagnante.
winnerWithARow([[A|_], [B|_], [C|_], [D|_], [E|_], [F|_], [G|_]], Winner) :-
    winningRow([A,B,C,D,E,F,G], Winner) ,!.
winnerWithARow([[_|Q1], [_|Q2], [_|Q3], [_|Q4], [_|Q5], [_|Q6], [_|Q7]], Winner) :-
    winnerWithARow([Q1,Q2,Q3,Q4,Q5,Q6,Q7], Winner) .

% Usage : Définir les positions gagnantes pour une ligne.
winningRow([P,Q,R,S,_,_,_], P) :-
    P==Q, Q==R, R==S,  nonvar(P).
winningRow([_,P,Q,R,S,_,_], P) :-
    P==Q, Q==R, R==S,  nonvar(P).
winningRow([_,_,P,Q,R,S,_], P) :-
    P==Q, Q==R, R==S,  nonvar(P).
winningRow([_,_,_,P,Q,R,S], P) :-
    P==Q, Q==R, R==S,  nonvar(P).

% Usage : Définir les positions gagnantes pour une diagonale descendante.
winnerWithADescendingDiag([[P,_,_,_,_,_], [_,Q,_,_,_,_], [_,_,R,_,_,_], [_,_,_,S,_,_],_,_,_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerWithADescendingDiag([[_,P,_,_,_,_], [_,_,Q,_,_,_], [_,_,_,R,_,_], [_,_,_,_,S,_],_,_,_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerWithADescendingDiag([[_,_,P,_,_,_], [_,_,_,Q,_,_], [_,_,_,_,R,_], [_,_,_,_,_,S],_,_,_], P) :-
    P==Q, Q==R, R==S,  nonvar(P).

winnerWithADescendingDiag([_, [P,_,_,_,_,_], [_,Q,_,_,_,_], [_,_,R,_,_,_], [_,_,_,S,_,_],_,_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerWithADescendingDiag([_, [_,P,_,_,_,_], [_,_,Q,_,_,_], [_,_,_,R,_,_], [_,_,_,_,S,_],_,_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerWithADescendingDiag([_, [_,_,P,_,_,_], [_,_,_,Q,_,_], [_,_,_,_,R,_], [_,_,_,_,_,S],_,_], P) :-
    P==Q, Q==R, R==S,  nonvar(P).

winnerWithADescendingDiag([_,_, [P,_,_,_,_,_], [_,Q,_,_,_,_], [_,_,R,_,_,_], [_,_,_,S,_,_],_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerWithADescendingDiag([_,_, [_,P,_,_,_,_], [_,_,Q,_,_,_], [_,_,_,R,_,_], [_,_,_,_,S,_],_], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerWithADescendingDiag([_,_, [_,_,P,_,_,_], [_,_,_,Q,_,_], [_,_,_,_,R,_], [_,_,_,_,_,S],_], P) :-
    P==Q, Q==R, R==S, nonvar(P).

winnerWithADescendingDiag([_,_,_, [P,_,_,_,_,_], [_,Q,_,_,_,_], [_,_,R,_,_,_], [_,_,_,S,_,_]], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerWithADescendingDiag([_,_,_,[_,P,_,_,_,_], [_,_,Q,_,_,_], [_,_,_,R,_,_], [_,_,_,_,S,_]], P) :-
    P==Q, Q==R, R==S, nonvar(P).
winnerWithADescendingDiag([_,_,_,[_,_,P,_,_,_], [_,_,_,Q,_,_], [_,_,_,_,R,_], [_,_,_,_,_,S]], P) :-
    P==Q, Q==R, R==S, nonvar(P).

% Usage : Définir les positions gagnantes pour une diagonale ascendante.
winnerWithAnAscendingDiag(Board,Winner) :-
    reverse(Board, InversedBoard), winnerWithADescendingDiag(InversedBoard, Winner).
