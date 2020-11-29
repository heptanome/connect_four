:- dynamic board/1.

:- use_module(displayBoard, [displayBoard/1]).
:- use_module(winner, [winner/2]).
:- use_module(minmax, [find_best_next_pos/5]).
:- use_module(utilities, [isColumnFull/1, updateColumn/3]).

% Usage : Regarder si le jeu est fini
gameover(Winner) :- board(Board), winner(Board, Winner), !.
gameover('Draw') :- board(Board), isBoardFull(Board).

% Usage : Vérifier si le plateau est plein
isBoardFull([]).
isBoardFull([H|C]) :- isColumnFull(H), isBoardFull(C).

% Usage : Appliquer le coup du joueur
applyIt(Board,NewBoard) :- retract(board(Board)), assert(board(NewBoard)).

% Usage : Changer le joueur
changePlayer('1','2').
changePlayer('2','1').

%IA ernvoie colonne complete
% Usage : Calcule un coup optimum pour gagner une partie : IA renvoie une colonne complete avec son coup
ia(Board, BestNext, Value, Player, Heur) :- find_best_next_pos(Board, BestNext, Value, Player, Heur).

% Usage : Récupérer le coup du joueur Humain
readColumn(X) :-
    writeln('Quelle colonne voulez-vous jouer (1 à 7)?'),
    read(X),
    nth1(Index, [1,2,3,4,5,6,7], X),
    not(isColumnFull(Index)).

%Game is over, we cut to stop the search, and display the winner.
play(_,_,_) :- gameover(Winner), !, write('Game is Over. Winner: '), writeln(Winner), board(Board),displayBoard(Board).
%The game is not over, we play the next turn

% in case the player 1 is human
% Heur 1: heuristic used by AI 1, same goes for Heur2
play(Player, 'human', Heur2) :-
    Player = '1',
    write('New turn for:'),
    writeln(Player),
    board(Board),
    displayBoard(Board),
    readColumn(IndexColumn),
    playMove(Board, IndexColumn, NewBoard, Player),
    applyIt(Board, NewBoard),
    changePlayer(Player, NextPlayer),
    play(NextPlayer, 'human', Heur2), !.

play(Player, Heur1, Heur2) :-
    Player = '1',
    write('New turn for:'),
    writeln(Player),
    board(Board),
    displayBoard(Board),
    ia(Board, NewBoard, _, Player, Heur1),
    applyIt(Board, NewBoard),
    changePlayer(Player,NextPlayer),
    play(NextPlayer, Heur1, Heur2), !.

play(Player, Heur1, Heur2) :-
    Player = '2',
    write('New turn for:'),
    writeln(Player),
    board(Board),
    displayBoard(Board),
    ia(Board, NewBoard, _, Player, Heur2),
    applyIt(Board, NewBoard),
    changePlayer(Player,NextPlayer),
    play(NextPlayer, Heur1, Heur2).

%Joue le coup (pour l'instant par du principe que la colonne donnee n'est pas pleine
playMove(Board, IndexColumn, NewBoard, P) :- 
    Board=NewBoard,
    nth1(IndexColumn, Board, Column),
    updateColumn(Column, NewColumn, P),
    nth1(IndexColumn, NewBoard, NewColumn).


%Debut du jeu
init :-
    length(C1,6),
    length(C2,6),
    length(C3,6),
    length(C4,6),
    length(C5,6),
    length(C6,6),
    length(C7,6),
    abolish(board/1),
    assert(board([C1,C2,C3,C4,C5,C6,C7])).

% lancer le jeu
start_game(Heur1, Heur2) :- init(), play('1', Heur1, Heur2).

