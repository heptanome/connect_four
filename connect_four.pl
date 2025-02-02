:- module(connect_four, [init/0, start_game/3]).

:- dynamic board/1.

:- use_module(utils/displayBoard, [displayBoard/1]).
:- use_module(utils/winner, [winner/2]).
:- use_module(search/minmax, [call_minmax/6]).
:- use_module(search/alphabeta, [call_alphabeta/6]).
:- use_module(utils/utilities, [changePlayer/2, isColumnFull/1, updateColumn/3]).

% Usage : Regarder si le jeu est fini
gameover(Winner) :- board(Board), winner(Board, Winner), !.
gameover('Draw') :- board(Board), isBoardFull(Board).

% Usage : Vérifier si le plateau est plein
isBoardFull([]).
isBoardFull([H|C]) :- isColumnFull(H), isBoardFull(C).

% Usage : Appliquer le coup du joueur
applyIt(Board,NewBoard) :-
    retract(board(Board)),
    assert(board(NewBoard)).

% Usage : Calcule un coup optimum pour gagner une partie : IA renvoie une colonne complete avec son coup.
ia(Board, BestNext, Value, Player, Heur, Depth) :-
    call_minmax(Board, Player, Heur, BestNext, Value, Depth).
    %call_alphabeta(Board, Player, Heur, BestNext, Value, Depth).

% Usage : Récupérer le coup du joueur Humain
readColumn(X) :-
    writeln('Quelle colonne voulez-vous jouer (1 à 7)?'),
    read(X),
    nth1(Index, [1,2,3,4,5,6,7], X),
    not(isColumnFull(Index)).

display_info(Board, Player) :-
    write('New turn for: '),
    writeln(Player),
    displayBoard(Board).

%Game is over, we cut to stop the search, and display the winner.
play(_,_,_,_) :-
    gameover(Winner), !,
    write('Game is Over. Winner: '),
    writeln(Winner),
    board(Board),
    displayBoard(Board).

%The game is not over, we play the next turn
% in case the player 1 is human
% Heur 1: heuristic used by AI 1, same goes for Heur2
play(Player, 'human', Heur2, Depth) :-
    Player = '1',
    board(Board),
    display_info(Board, Player),
    readColumn(IndexColumn),
    playMove(Board, IndexColumn, NewBoard, Player),
    applyIt(Board, NewBoard),
    changePlayer(Player, NextPlayer),
    play(NextPlayer, 'human', Heur2, Depth), !.

play(Player, Heur1, Heur2, Depth) :-
    Player = '1',
    board(Board),
    display_info(Board, Player),
    ia(Board, NewBoard, _, Player, Heur1, Depth),
    applyIt(Board, NewBoard),
    changePlayer(Player,NextPlayer),
    play(NextPlayer, Heur1, Heur2, Depth), !.

play(Player, Heur1, Heur2, Depth) :-
    Player = '2',
    board(Board),
    display_info(Board, Player),
    ia(Board, NewBoard, _, Player, Heur2, Depth),
    applyIt(Board, NewBoard),
    changePlayer(Player,NextPlayer),
    play(NextPlayer, Heur1, Heur2, Depth).

% Usage : Jouer un coup.On suppose que la colonne jouée n'est pas pleine.
playMove(Board, IndexColumn, NewBoard, P) :- 
    Board=NewBoard,
    nth1(IndexColumn, Board, Column),
    updateColumn(Column, NewColumn, P),
    nth1(IndexColumn, NewBoard, NewColumn).


% Usage : Initialiser le jeu
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

% Usage : Lancer le jeu
%start_game(Heur1, Heur2) :- init(), play('1', Heur1, Heur2).
start_game(Heur1, Heur2, Depth) :- init(), play('1', Heur1, Heur2, Depth).

