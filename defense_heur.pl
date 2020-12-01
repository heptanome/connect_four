:- module(defense_heur, [heuristic_def/3]).
:- use_module(utilities_heuristics, [getEveryDescDiags/2, reverseEveryColumns/2, 
    getEveryDescDiagsHalfBoard/3, createOneDescDiag/5]).
% Usage : Obtenir le coût de la dispotion actuelle du plateau en
%         cherchant le nombre maximum de jetons alignés par joueur adverse sans blocage par un pion
%         du joueur actuel, dans chaque direction du plateau. Ces directions sont ligne, colonne
%         , diagonale descendante et diagonale ascendante. Le cout total est la somme des couts dans 
%         chacune de ces directions.
% heuristic(+Board, +Player, -FinalCost) :
% - Board     : état du plateau
% - Player    : numéro du joueur actuel
% - FinalCost : cout de la dispostion du plateau. Plus il est élevé, plus il est favorable à l'adversaire
heuristic_def(Board, Player, FinalCost) :-
    nonvar(Player),
    getColumnCostList(Board, Player, CostsListColumn),
    max_list(CostsListColumn, MaxCostColumn),
    getRowCostList(Board, Player, CostsListRow),
    max_list(CostsListRow, MaxCostRow),
    getDescendingDiagsCostList(Board, Player, CostsListDescDiags),
    max_list(CostsListDescDiags, MaxCostDescDiags),
    getAscendingDiagsCostList(Board, Player, CostsListAscDiags),
    max_list(CostsListAscDiags, MaxCostAscDiags),
    S1 is MaxCostColumn,
    S2 is S1 + MaxCostRow,
    S3 is S2 + MaxCostDescDiags,
    FinalCost is S3 + MaxCostAscDiags,
    printAll(CostsListColumn, CostsListRow, CostsListDescDiags, CostsListAscDiags, FinalCost).



    printAll(CostsListColumn, CostsListRow, CostsListDescDiags, CostsListAscDiags, FinalCost) :-
        write('Column     : '),
        printVal(CostsListColumn),
        write('Row        : '),
        printVal(CostsListRow),
        write('Desc Diags : '),
        printVal(CostsListDescDiags),
        write('Asc Diags  : '),
        printVal(CostsListAscDiags),
        write('Final cost  : '),
        write(FinalCost),
        writeln(''),writeln('').

    printVal([]) :-
        writeln('').
    printVal([H|T]) :-
        write(H), write(' '), printVal(T).

% Usage : Obtenir le nombre de jetons consécutifs alignés du joueur adverse sur chaque colonne du plateau
%         On ne compte que les jetons  qui sont au dessus du plus haut jeton du joueur actuel
% getColumnCostList(+Board, +Player, -List) :
% - Board  : état du plateau
% - Player : numéro du joueur actuel
% - List   : liste des sommes de jetons alignés du joueur pour chaque colonne du plateau
getColumnCostList([], _, []).
getColumnCostList([ActualColonne|Rest], Player, [Cost|List]) :-
    reverse(ActualColonne, ReversedColonne),
    sumColumn(Player, ReversedColonne, Cost),
    getColumnCostList(Rest, Player, List).

% Usage : Compter le nombre de jetons consécutifs alignés du joueur adverse sur une colonne
%         On ne compte que les jetons  qui sont au dessus du plus haut jeton du joueur actuel
% sumColumn(+Player, +Column, -Sum) :
% - Player : numéro du joueur actuel
% - Column : colonne sur laquelle on calcule le nombre de jetons alignés
% - Sum : nombre de jetons alignés du joueur adverse
% adverse en haut de la colonne
sumColumn(_, [], 0).
sumColumn(Player, [H|T], Sum) :-
    var(H),
    sumColumn(Player, T, Sum).
sumColumn(Player, [H|_], 0) :-
    nonvar(H),
    H = Player.
sumColumn(Player, [H|T], AlignedTokens) :-
    H \= Player,
    sumColumn(Player, T, Sum),
    AlignedTokens is Sum+1.

% Usage : Obtenir le nombre de jetons alignés par joueur adverse sans blocage du joueur actuel, sur chaque ligne du plateau
% getRowCostList(+Board, +Player, -List) :
% - Board  : état du plateau 
% - Player : numéro du joueur actuel
% - List   : liste des sommes de jetons alignés du joueur pour chaque ligne du plateau
getRowCostList([[],[],[],[],[],[],[]],_,[]).
%Sans ajout de la dernière somme
getRowCostList([[H1|R1], [H2|R2], [H3|R3], [H4|R4], [H5|R5], [H6|R6], [H7|R7]], Player, [MaxCostRow|List]) :-
    CurrentLigne = [H1, H2, H3, H4, H5, H6, H7],
    sumRow(Player, CurrentLigne,_,FreedomDegree, ListCost),
    FreedomDegree > 4,
    max_list(ListCost, MaxCostRow),
    getRowCostList([R1,R2,R3,R4,R5,R6,R7], Player,List).
%Avec ajout de la dernière somme
getRowCostList([[H1|R1], [H2|R2], [H3|R3], [H4|R4], [H5|R5], [H6|R6], [H7|R7]], Player, [MaxCostRow|List]) :-
    CurrentLigne = [H1, H2, H3, H4, H5, H6, H7],
    sumRow(Player, CurrentLigne,LastSum,_, ListCost),
    max_list([LastSum|ListCost], MaxCostRow),
    getRowCostList([R1,R2,R3,R4,R5,R6,R7], Player,List).

% Usage : Compter le nombre de jetons  du joueur adverse alignés sans blocage sur une ligne
% sumRow(+Player, +Row, -Sum, -ListSum) :
% - Player  : numéro du joueur actuel
% - Row     : ligne sur laquelle on calcule le nombre de jetons alignés
% - Sum     : nombre de jetons alignés du joueur en début de ligne
% - FreedomDegree : nombre de case vides ou contenant des jetons adverses alignées (nb cases pouvant contenir un alignement adverse)
% - ListSum : liste des sommes des jetons alignés sur une ligne hors début de ligne

sumRow(_,[],0,0, []).
%Cas jeton IA avec sauvegarde
sumRow(Player, [H|T],Sum, FreedomDegree, [NewSum| ListSum]) :-
    nonvar(H),
    H = Player,
    sumRow(Player, T, NewSum, NewFreedomDegree ,  ListSum),
    NewFreedomDegree > 4,
    FreedomDegree is 0,
    Sum is 0.
%Cas jeton IA sans sauvegarde
sumRow(Player, [H|T],Sum, FreedomDegree, ListSum) :-
    nonvar(H),
    H = Player,
    sumRow(Player, T,_,NewFreedomDegree,  ListSum), % ? sum
    writeln(''),writeln('New Freedom Degree : '),writeln(NewFreedomDegree),writeln(''),
    FreedomDegree is 0,
    Sum is 0.
%Cas jeton Adverse sans sauvegarde
sumRow(Player, [H|T], Sum, FreedomDegree, ListSum) :-
    nonvar(H),
    H \= Player,
    sumRow(Player, T, NewSum, NewFreedomDegree, ListSum),
    FreedomDegree is NewFreedomDegree+1,
    Sum is NewSum+1.
%Cas case libre sans sauvegarde
sumRow(Player, [H|T], Sum, FreedomDegree, ListSum) :-
    var(H),
    sumRow(Player, T, NewSum, NewFreedomDegree, ListSum),
    FreedomDegree is NewFreedomDegree+1,
    Sum is NewSum.



% Usage : Obtenir le nombre de jetons du joueur adverse alignés sans blocage du joueur
%         actuel, sur les diagonales ascendantes numéro 4 à 9.
%         On ne prend pas en compte les diagonales 1, 2, 3, 10, 11 et 12,
%         car on ne peut aligner 4 jetons desssus.
% getAscendingDiagsCostList(+Board, +Player, -List):
% - Board  : état du plateau
% - Player : numéro du joueur actuel
% - List   : liste des sommes de jetons alignés pour les diagonales ascendantes 4 à 9
getAscendingDiagsCostList(Board, Player, List) :-
    reverse(Board, ReversedBoard),
    getDescendingDiagsCostList(ReversedBoard, Player, List).

% Usage : Obtenir le nombre de jetons du joueur adverse alignés sans blocage du joueur
%         actuel, sur les diagonales descendantes numéro 4 à 9.
%         On ne prend pas en compte les diagonales 1, 2, 3, 10, 11 et 12,
%         car on ne peut aligner 4 jetons desssus.
% getDescendingDiagsCostList(+Board, +Player, -List):
% - Board  : état du plateau
% - Player : numéro du joueur actuel
% - List   : liste des sommes de jetons alignés pour les diagonales descendantes N°4 à 9
getDescendingDiagsCostList(Board, Player, List) :-
    getEveryDescDiags(Board, CompleteListDiags),
    sumDiag(Player, CompleteListDiags, List).

sumDiag(_, [], []).
%Sans ajout de la dernière somme
sumDiag(Player, [Diag|Rest], [MaxCostDiag|ListSum]) :-
    sumRow(Player, Diag,_,FreedomDegree, ListCost),
    FreedomDegree > 4,
    max_list(ListCost, MaxCostDiag),
    sumDiag(Player, Rest, ListSum).
%Avec ajout de la dernière somme
sumDiag(Player, [Diag|Rest], [MaxCostDiag|ListSum]) :-
    sumRow(Player, Diag,LastSum,_, ListCost),
    max_list([LastSum|ListCost], MaxCostDiag),
    sumDiag(Player, Rest, ListSum).


%%% TESTS %%%
board([['1', '1', '1', '2', '1', _], ['1', '2', '1', _, _, _], ['1', '2', '2', '2', _, _], ['2', '1', '1', '1', '2', '2'], ['2', '1', '2', '1', '2', _], ['2', '2', '2', '1', '1', _], ['1', _, _, _, _, _]]).
board2([['1', '2', _, _, _, _], [_, _, _, _, _, _], [_, _, _, _, _, _], [_, _, _, _, _, _], [_, _, _,_, _, _], [_, _, _, _, _, _], [_, _, _, _, _, _]]).


%%% SOMMES DES JETONS SUR LES COLONNES
testSumRow(Player, Sum, ListSum) :- sumRow(Player, [1, 2, 1, _, 1, 2, 1, 2, 1, 1, 2], Sum,_, ListSum).
testGetColumnCostList(Player, List) :- board2(Board), getColumnCostList(Board, Player, List).

%%% SOMMES DES JETONS SUR LES LIGNES
testGetRowCostList(Player, Sum) :- board(Board), getRowCostList(Board, Player, Sum).

%%% SOMMES DES JETONS SUR LES DIAGONALES
testGetDescDiagsCostList(Player, Sum) :- board2(Board), getDescendingDiagsCostList(Board, Player, Sum).
testGetAscDiagsCostList(Player, Sum) :- board2(Board), getAscendingDiagsCostList(Board, Player, Sum).

%%% HEURISTIC
testHeuristic(Player, Cost) :- board2(Board), heuristic_def(Board, Player, Cost).
