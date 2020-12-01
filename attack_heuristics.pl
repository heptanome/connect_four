:- module(attack_heuristics, [heuristic_max/3, heuristic_sum/3, heuristic_alert/3]).

:- use_module(displayBoard, [displayBoard/1]).
:- use_module(utilities_attack_heur, [getColumnCostList/3, getRowCostList/3, getAscendingDiagsCostList/3, getDescendingDiagsCostList/3]).

% Usage : Obtenir le coût de la dispotion actuelle du plateau en
%         cherchant le nombre maximum de jetons alignés du joueur actuel sur le plateau.
%         Ces jetons peuvent être aligné sur une ligne, une colonne ou une diagonale.
% heuristic_max(+Board, +Player, -FinalCost) :
% - Board     : état du plateau après avoir joué le coup
% - Player    : numéro du joueur actuel
% - FinalCost : cout de la dispostion du plateau
heuristic_max(Board, Player, FinalCost) :-
    nonvar(Player),
    displayBoard(Board),
    getColumnCostList(Board, Player, CostsListColumn),
    max_list(CostsListColumn, MaxCostColumn),
    getRowCostList(Board, Player, CostsListRow),
    max_list(CostsListRow, MaxCostRow),
    getDescendingDiagsCostList(Board, Player, CostsListDescDiags),
    max_list(CostsListDescDiags, MaxCostDescDiags),
    getAscendingDiagsCostList(Board, Player, CostsListAscDiags),
    max_list(CostsListAscDiags, MaxCostAscDiags),
    FinalCost is -max(MaxCostColumn, max(MaxCostRow, max(MaxCostDescDiags, MaxCostAscDiags))).
    
% Usage : Obtenir le coût de la dispotion actuelle du plateau en
%         cherchant la somme du nombre max de jetons alignés sur une ligne, une colonne,
%         une diagonale ascendante et descendante du plateau.
% heuristic_sum(+Board, +Player, -FinalCost) :
% - Board     : état du plateau après avoir joué le coup
% - Player    : numéro du joueur actuel
% - FinalCost : cout de la dispostion du plateau
heuristic_sum(Board, Player, FinalCost) :-
    nonvar(Player),
    getColumnCostList(Board, Player, CostsListColumn),
    max_list(CostsListColumn, MaxCostColumn),
    getRowCostList(Board, Player, CostsListRow),
    max_list(CostsListRow, MaxCostRow),
    getDescendingDiagsCostList(Board, Player, CostsListDescDiags),
    max_list(CostsListDescDiags, MaxCostDescDiags),
    getAscendingDiagsCostList(Board, Player, CostsListAscDiags),
    max_list(CostsListAscDiags, MaxCostAscDiags),
    Max is MaxCostColumn + MaxCostRow + MaxCostDescDiags + MaxCostAscDiags,
    FinalCost is -Max.

% Usage : Obtenir le coût de la dispotion actuelle du plateau en
%         cherchant la somme du nombre max de jetons alignés sur une ligne, une colonne,
%         une diagonale ascendante et descendante du plateau.
%         Si 4 jetons ou plus sont alignés, cette disposition est favorisée.
% heuristic_alert(+Board, +Player, -FinalCost) :
% - Board     : état du plateau après avoir joué le coup
% - Player    : numéro du joueur actuel
% - FinalCost : cout de la dispostion du plateau
heuristic_alert(Board, Player, FinalCost) :-
    nonvar(Player),
    displayBoard(Board),
    getColumnCostList(Board, Player, CostsListColumn),
	  help_heuristic_alert(CostsListColumn, MaxCostColumns),
    getRowCostList(Board, Player, CostsListRow),
	  help_heuristic_alert(CostsListRow, MaxCostRows),
    getDescendingDiagsCostList(Board, Player, CostsListDescDiags),
    help_heuristic_alert(CostsListDescDiags, MaxCostDescDiags),
    getAscendingDiagsCostList(Board, Player, CostsListAscDiags),
    help_heuristic_alert(CostsListAscDiags, MaxCostAscDiags),
    Max is MaxCostColumns + MaxCostRows + MaxCostDescDiags + MaxCostAscDiags,
    FinalCost is -Max.  
    
help_heuristic_alert(CostsList, FinalCost) :-
	max_list(CostsList, MaxCost),
	MaxCost >= 4,
	FinalCost is 100.
	
help_heuristic_alert(CostsList, MaxCost) :-
	max_list(CostsList, MaxCost),
	MaxCost < 4.

%%%% TO REMOVE : DISPLAY CURRENT VALUES
printAll(CostsListColumn, CostsListRow, CostsListDescDiags, CostsListAscDiags) :-
	write('Column     : '),
    printVal(CostsListColumn),
    write('Row        : '),
    printVal(CostsListRow),
    write('Desc Diags : '),
    printVal(CostsListDescDiags),
    write('Asc Diags  : '),
    printVal(CostsListAscDiags),
    writeln('').
    
printVal([]) :-
    writeln('').
printVal([H|T]) :-
    write(H), write(' '), printVal(T).
