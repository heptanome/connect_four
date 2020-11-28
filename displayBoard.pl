:- module(displayBoard, [displayBoard/1]).

%Affichage du jeu : ligne par ligne
displayBoard(B) :-
    writeln('*------------*'),
    printLigne(6,B), printLigne(5,B), printLigne(4,B),
    printLigne(3,B), printLigne(2,B), printLigne(1,B),
    writeln('*------------*'),
    writeln('').

%Affichage d une ligne
printLigne(L, B) :-
    nth1(1,B,C1), printVal(C1,L),
    nth1(2,B,C2), printVal(C2,L),
    nth1(3,B,C3), printVal(C3,L),
    nth1(4,B,C4), printVal(C4,L),
    nth1(5,B,C5), printVal(C5,L),
    nth1(6,B,C6), printVal(C6,L),
    nth1(7,B,C7), printVal(C7,L),
    writeln('').

%Afficher le contenu de la case a l indice N de la colonne C (?, x or o)
printVal(C,N) :-
    nth1(N,C,Val), var(Val), write('  '), ! .
printVal(C,N) :-
    nth1(N,C,Val), write(Val), write(' ').

