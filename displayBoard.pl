:- module(displayBoard, [displayBoard/1]).

% Usage : Afficher le plateau ligne par ligne.
displayBoard(Board) :-
    writeln('*------------*'),
    printRow(6,Board), printRow(5,Board), printRow(4,Board),
    printRow(3,Board), printRow(2,Board), printRow(1,Board),
    writeln('*------------*'),
    writeln('').

% Usage : Afficher une ligne.
printRow(Row, Board) :-
    nth1(1,Board,C1), printVal(C1,Row),
    nth1(2,Board,C2), printVal(C2,Row),
    nth1(3,Board,C3), printVal(C3,Row),
    nth1(4,Board,C4), printVal(C4,Row),
    nth1(5,Board,C5), printVal(C5,Row),
    nth1(6,Board,C6), printVal(C6,Row),
    nth1(7,Board,C7), printVal(C7,Row),
    writeln('').

% Usage : Afficher le contenu de la case à l'indice N de la colonne C
% c'est à dire soit un ' ' si la case est vide sinon le numéro du joueur (1 ou 2).
printVal(Column,Index) :-
    nth1(Index, Column, Value), var(Value), write('  '), ! .
printVal(Column,Index) :-
    nth1(Index,Column,Value), write(Value), write(' ').