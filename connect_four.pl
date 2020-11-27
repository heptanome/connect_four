:- dynamic board/1.

:- use_module(displayBoard, [displayBoard/1]).
:- use_module(winner, [winner/2]).
:- use_module(minmax, [find_best_next_pos/4]).
:- use_module(utilities, [isColonneFull/1, updateColonne/3]).

%Regarder si le jeu est fini
gameover(Winner) :- board(Board), winner(Board, Winner), !.
gameover('Draw') :- board(Board), isBoardFull(Board).

isBoardFull([]).
isBoardFull([H|C]) :- isColonneFull(H), isBoardFull(C).
%Iterer sur toutes les colonnes et pour chaque colonne.

%On applique le coup du joueur
applyIt(Board,NewBoard) :- retract(board(Board)), assert(board(NewBoard)).
% Predicate to get the next player
changePlayer('1','2').
changePlayer('2','1').

%IA ernvoie colonne complete
% ia(Board, IndexColonne,_) :- repeat, I is random(7), IndexColonne is I+1,  nth1(IndexColonne, Board, Elem), not(isColonneFull(Elem)), write('Chose column '),writeln(IndexColonne), !.
ia(Board, BestNext, Value, Player) :- find_best_next_pos(Board, BestNext, Value, Player).


% Game is over, we cut to stop the search, and display the winner.
play(_) :- gameover(Winner), !, write('Game is Over. Winner: '), writeln(Winner), board(Board),displayBoard(Board).
% The game is not over, we play the next turn
%play(Player) :- write('New turn for:'), writeln(Player), board(Board), displayBoard(Board),ia(Board, IndexColonne, Player), playMove(Board, IndexColonne, NewBoard, Player),  applyIt(Board, NewBoard), changePlayer(Player,NextPlayer), play(NextPlayer).
play(Player) :- write('New turn for:'), writeln(Player), board(Board), displayBoard(Board),ia(Board, NewBoard, _, Player),  applyIt(Board, NewBoard), changePlayer(Player,NextPlayer), play(NextPlayer).

% Joue le coup (pour l'instant par du principe que la colonne donnee
% n'est pas pleine
playMove(Board,IndexColonne,NewBoard,P) :-
    Board=NewBoard,
    nth1(IndexColonne, Board, Colonne),
    updateColonne(Colonne, NewColonne, P),
    nth1(IndexColonne, NewBoard, NewColonne).


%Debut du jeu
init :- length(C1,6), length(C2,6),length(C3,6),
    length(C4,6),length(C5,6),length(C6,6),length(C7,6),
    assert(board([C1,C2,C3,C4,C5,C6,C7])).

% lancer le jeu
start_game :- play('1').

