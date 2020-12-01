% Parcours en profondeur de l arbre des coups possibles
% BestBoard garde en mémoire la configuration du meilleur coup trouvé so far
% Est-ce que c est suffisant ?

minmax(Board,BestBoard,Player,Heur,Value,0) :-
    compare_boards(Board,BestBoard,Tmp),
    BestBoard = tmp,
    !.

minmax(Board,BestBoard,Player,Heur,Depth) :-
    possible_move(Board,NextBoard,Player), 
    Depth is Depth-1,
    changePlayer(Player,Opponent),
    minmax(NextBoard,BestBoard,Opponent,Heur,Depth).

compareBoard(Board1,Board2,Board1,Heur,Player) :-
    value_of(Board1,Player,Value1,Heur),
    value_of(Board2,Player,Value2,Heur),
    Value1 < Value2,
    !.

compareBoard(Board1,Board2,Board2,Heur,Player) :-
    value_of(Board1,Player,Value1,Heur),
    value_of(Board2,Player,Value2,Heur),
    Value2 =< Value1.
