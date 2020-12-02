:- module(attack_heuristics, [heuristic_max/3, heuristic_aSum/3, heuristic_alert/4, heuristic_fSum/3, heuristic_dSum/3, heuristic_fAlert/3]).

:- use_module(displayBoard, [displayBoard/1]).
:- use_module(utilities, [changePlayer/2]).
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
    getColumnCostList(Board, Player, CostsListColumn),
    max_list(CostsListColumn, MaxCostColumn),
    getRowCostList(Board, Player, CostsListRow),
    max_list(CostsListRow, MaxCostRow),
    getDescendingDiagsCostList(Board, Player, CostsListDescDiags),
    max_list(CostsListDescDiags, MaxCostDescDiags),
    getAscendingDiagsCostList(Board, Player, CostsListAscDiags),
    max_list(CostsListAscDiags, MaxCostAscDiags),
    printAll(CostsListColumn, CostsListRow, CostsListDescDiags, CostsListAscDiags),
    FinalCost is -max(MaxCostColumn, max(MaxCostRow, max(MaxCostDescDiags, MaxCostAscDiags))).
    
% Usage : Obtenir le coût de la dispotion actuelle du plateau en sommant le nombre de jetons
%         alignés sur une ligne, une colonne, une diagonale ascendante et descendante du plateau.
% heuristic_aSum(+Board, +Player, -FinalCost) :
% - Board     : état du plateau après avoir joué le coup
% - Player    : numéro du joueur actuel
% - FinalCost : cout de la dispostion du plateau
heuristic_aSum(Board, Player, FinalCost) :-
    nonvar(Player),
    getColumnCostList(Board, Player, CostsListColumn),
    max_list(CostsListColumn, SumCostColumn),
    getRowCostList(Board, Player, CostsListRow),
    max_list(CostsListRow, SumCostRow),
    getDescendingDiagsCostList(Board, Player, CostsListDescDiags),
    max_list(CostsListDescDiags, SumCostDescDiags),
    getAscendingDiagsCostList(Board, Player, CostsListAscDiags),
    max_list(CostsListAscDiags, SumCostAscDiags),
    Sum is SumCostColumn + SumCostRow + SumCostDescDiags + SumCostAscDiags,
    FinalCost is -Sum.
    
heuristic_dSum(Board, Player, FinalCost) :-
    nonvar(Player),
    changePlayer(Player, Opponent),
    heuristic_aSum(Board, Opponent, CostOpponent),
    FinalCost is -CostOpponent.
    
heuristic_fSum(Board, Player, FinalCost) :-
    nonvar(Player),
    heuristic_aSum(Board, Player, CostPlayer),
    heuristic_dSum(Board, Player, CostOpponent),
    FinalCost is CostPlayer+CostOpponent.

% Usage : Obtenir le coût de la dispotion actuelle du plateau en
%         cherchant la somme du nombre max de jetons alignés sur une ligne, une colonne,
%         une diagonale ascendante et descendante du plateau.
%         Si 4 jetons ou plus sont alignés, cette disposition est favorisée.
% heuristic_alert(+Board, +Player, -FinalCost) :
% - Board     : état du plateau après avoir joué le coup
% - Player    : numéro du joueur actuel
% - FinalCost : cout de la dispostion du plateau
heuristic_alert(Board, Player, FinalCost, Number) :-
    nonvar(Player),
    getColumnCostList(Board, Player, CostsListColumn),
	help_heuristic_alert(CostsListColumn, MaxCostColumns, Number),
    getRowCostList(Board, Player, CostsListRow),
	help_heuristic_alert(CostsListRow, MaxCostRows, Number),
    getDescendingDiagsCostList(Board, Player, CostsListDescDiags),
    help_heuristic_alert(CostsListDescDiags, MaxCostDescDiags, Number),
    getAscendingDiagsCostList(Board, Player, CostsListAscDiags),
    help_heuristic_alert(CostsListAscDiags, MaxCostAscDiags, Number),
    Max is MaxCostColumns + MaxCostRows + MaxCostDescDiags + MaxCostAscDiags,
    FinalCost is -Max.  
    
heuristic_fAlert(Board, Player, FinalCost) :-
    nonvar(Player),
    heuristic_alert(Board, Player, CostPlayer, 4),
    changePlayer(Player, Opponent),
    heuristic_alert(Board, Opponent, CostOpponent, 3),
    FinalCost is CostPlayer-CostOpponent.
    
help_heuristic_alert(CostsList, FinalCost, Number) :-
	max_list(CostsList, MaxCost),
	MaxCost >= Number,
	FinalCost is 50*Number.
	
help_heuristic_alert(CostsList, MaxCost, Number) :-
	max_list(CostsList, MaxCost),
	MaxCost < Number.

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
