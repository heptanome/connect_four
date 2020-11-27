% usage : possible_move(pos1,pos2) -> renvoie true si pos2 peut être atteinte à
% partir de pos1, false sinon
% possible_move(current, X) -> X prend la valeur d une position atteignable depuis current
possible_move(currentPos, nextPos) :-
  /A definir/.

% usage : value_of(current, X) -> X prend la valeur de la position current
value_of(position, value) :-
  /A definir avec l'heuristique/.

% usage : compare(p1,v1,p2,v2,X,Y) -> X prend la valeur p1 si v1 > v2 ou p2 sinon. Y prend la valeur maximale entre v1 et v2.
% compare(+pos1, +val1, +pos2, +val2, -bestPos, -bestVal)
  % pos1 > pos2
compare(pos1,val1,_,val2,pos1,val1) :- val1 > val2. 
  % pos2 > pos1
  % Je pense que `compare(_,_,pos2,val2,pos2,val2).` peut suffire parce que cette deuxième règle ne sera appelée que si la première ne l est pas. Basiquement, cela correspond à "non(premiere regle)" donc il ne doit pas être necessaire de donner trop de détails sur val1 puis sur la comparaison
compare(_,val1,pos2,val2,pos2,val2) :- val2 > val1.

% usage : find_best_next(current,X,Y) -> X prend la valeur de la meilleure position atteignable depuis current. Y prend la valeur de cette position.
find_best_next_pos(current,bestNext,value) :-
  bagof(nextPos, possible_move(current,nextPos),listNextPos),
  best_of_list(listNextPos, bestNext,value),!. 
  % on s arrete dès qu'on a un meilleur element
% si possible_move(current,nextPos) ne revoie aucune valeur pour nextPos, alors c'est que current n'a pas de successeur :
find_best_next_pos(current,_,value) :-
  value_of(current,value).
  

% usage : best_of_list(list,X,Y) -> X prend la valeur de la position qui a la meileure valeur, et Y prend la valeur de X
% un seul élément dans la liste
best_of_list([position], position,value) :- 
  value_of(position,value).

% plusieurs élements dans la liste (on itère sur la liste en comparant 2 à 2)
best_of_list([pos1 | endList],bestPos,val1) :-
  value_of(pos1, val1),
  % on itère sur la liste
  best_of_list(endList,pos2,val2),
  % on compare
  compare(pos1,val1,pos2,val2,bestPos,bestValue).



