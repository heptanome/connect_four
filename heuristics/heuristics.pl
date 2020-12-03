:- module(heuristics, [heuristic_max/3, heuristic_aSum/3, heuristic_alert/4,
                      heuristic_fSum/3, heuristic_dSum/3, heuristic_fAlert/3]).

:- use_module(utils/utilities, [changePlayer/2]).
:- use_module(utils/utilities_heur, [getColumnCostList/3, getRowCostList/3,
                                    getAscendingDiagsCostList/3,
                                    getDescendingDiagsCostList/3]).

% Usage : Obtenir le coût de la dispotion actuelle du plateau en
%         cherchant le nombre maximum de jetons alignés du joueur actuel sur le
%         plateau. Ces jetons peuvent être aligné sur une ligne, une colonne ou
%         une diagonale.
%         Le coût final sera négatif.
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
    FinalCost is -max(MaxCostColumn, max(MaxCostRow, max(MaxCostDescDiags, MaxCostAscDiags))).
    
% Usage : Obtenir le coût de la dispotion actuelle du plateau en sommant le
%         nombre maximun de jetons alignés sur une ligne, une colonne, une diagonale
%         ascendante et descendante du plateau.
%         Le coût final sera négatif.
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

% Usage : Obtenir le coût de la dispotion actuelle du plateau en :
%         - en sommant le nombre maximun de jetons alignés du joueur adverse sur 
%           une ligne, une colonne, une diagonale ascendante et descendante du plateau.
%         Le coût final sera positif.
% heuristic_dSum(+Board, +Player, -FinalCost) :
% - Board     : état du plateau après avoir joué le coup
% - Player    : numéro du joueur actuel
% - FinalCost : coût de la dispostion du plateau    
heuristic_dSum(Board, Player, FinalCost) :-
    nonvar(Player),
    changePlayer(Player, Opponent),
    heuristic_aSum(Board, Opponent, CostOpponent),
    FinalCost is -CostOpponent.

% Usage : Obtenir le coût de la dispotion actuelle du plateau en :
%         - en sommant le nombre maximun de jetons alignés du joueur actuel sur 
%           une ligne, une colonne, une diagonale ascendante et descendante du plateau.
%           Cette somme est appelée CostPlayer.
%         - en sommant le nombre maximun de jetons alignés du joueur adverse sur 
%           une ligne, une colonne, une diagonale ascendante et descendante du plateau.
%           Cette somme est appelée CostOpponent.
%         Le coût final correspond à la différence de ces deux sommes (i.e CostPlayer-CostOpponent)
% heuristic_fSum(+Board, +Player, -FinalCost) :
% - Board     : état du plateau après avoir joué le coup
% - Player    : numéro du joueur actuel
% - FinalCost : coût de la dispostion du plateau 
heuristic_fSum(Board, Player, FinalCost) :-
    nonvar(Player),
    heuristic_aSum(Board, Player, CostPlayer),
    heuristic_dSum(Board, Player, CostOpponent),
    FinalCost is CostPlayer+CostOpponent.

% Usage : Obtenir le coût de la dispotion actuelle du plateau en
%         cherchant la somme du nombre max de jetons alignés sur une ligne, une
%         colonne, une diagonale ascendante et descendante du plateau.
%         Si N jetons ou plus sont alignés, cette disposition est favorisée.
%         Le coût final sera négatif.
% heuristic_alert(+Board, +Player, -FinalCost, -Number) :
% - Board     : état du plateau après avoir joué le coup
% - Player    : numéro du joueur actuel
% - FinalCost : coût de la dispostion du plateau
% - Number    : Nombre de jetons alignés à partir du quel on décide favoriser cette disposition
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

% Usage : Obtenir le coût de la dispotion actuelle du plateau en :
%         - cherchant CostPlayer qui est la somme du nombre max de jetons alignés du joueur actuel
%           sur une ligne, une colonne, une diagonale ascendante et descendante du plateau.
%         - cherchant CostOpponent qui est la somme du nombre max de jetons alignés du joueur adversaire
%           sur une ligne, une colonne, une diagonale ascendante et descendante du plateau.
%         Le coût final correspond à la différence de ces deux sommes (i.e CostPlayer-CostOpponent)
%         Si 4 jetons du joueur actuel ou plus sont alignés, cette disposition est favorisée.
%         Si 3 jetons du joueur adversaire ou plus sont alignés, cette disposition est défavorisée.
% heuristic_fAlert(+Board, +Player, -FinalCost) :
% - Board     : état du plateau après avoir joué le coup
% - Player    : numéro du joueur actuel
% - FinalCost : coût de la dispostion du plateau
heuristic_fAlert(Board, Player, FinalCost) :-
    nonvar(Player),
    heuristic_alert(Board, Player, CostPlayer, 4),
    changePlayer(Player, Opponent),
    heuristic_alert(Board, Opponent, CostOpponent, 3),
    FinalCost is CostPlayer-CostOpponent.

% Usage : Si Number jetons du joueur ou plus sont alignés alors on décide de favoriser 
%         cette disposition du plateau en lui attribuant un coût exception de 50*Number
% help_heuristic_alert(+CostsList, -FinalCost, +Number)
% - CostsList : Représente une liste de coûts pour une dimension du plateau.
%               Une dimension est soit toutes les colonnes, soit toutes les lignes, soit toutes les diagonales.
% - FinalCost : Coût Max de la liste CostsList
% - Number    : Nombre de jetons alignés à partir duquel on décide favoriser la disposition
help_heuristic_alert(CostsList, FinalCost, Number) :-
	max_list(CostsList, MaxCost),
	MaxCost >= Number,
	FinalCost is 50*Number.
	
help_heuristic_alert(CostsList, MaxCost, Number) :-
	max_list(CostsList, MaxCost),
	MaxCost < Number.
