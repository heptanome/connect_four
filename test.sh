#!/bin/bash

wins1=0
wins2=0
nb_games=50

for i in $(seq 1 $nb_games)
do
  echo $i
  winner=$(swipl -s test.pl | grep "Winner" | awk '{print $5}')
  if [ $winner == "1" ]; then
    wins1=$((wins1+1))
  else
    wins2=$((wins2+1))
  fi
done

draws=$((nb_games-wins1-wins2))
echo "wins for player 1: $wins1"
echo "wins for player 2: $wins2"
echo "draws: $draws"
