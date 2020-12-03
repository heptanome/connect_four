# Projet connect\_four

*Hexanôme H4213*
*Auteurs* : BATEL Arthur, BODELOT Paul, BUONOMO Fanny, GUERRAOUI Camélia, KESSIBI Guillaume, PELTIER Camille, ZIEGER Luise

## Introduction :
Dans le cadre de notre projet d'Approche Logique pour l'Intelligence Artificielle, nous avons développé un jeu du Puissance 4 à base de prédicats Prolog. Nous n'avons pas mis en place d'interface graphique, le but de ce document est donc de vous guider dans l'utilisation du programme.

## Rapide présentation des règles :
Pour rappel, l'objectif dans le jeu du Puissance 4 est d'aligner, avant le joueur adverse, 4 pions de sa couleur, en ligne, colonne ou diagonale. Lors d'un tour de jeu, le joueur choisit uniquement la colonne dans laquelle jouer, et son pion tombera dans cette colonne pour occuper la place libre la plus basse. Il n'est pas autorisé de jouer dans une colonne pleine. Les joueurs jouent chacun leur tour.

## Comment jouer :
Pour jouer, vous devez disposer d'une machine possédant Prolog. Le programme principal se nomme **connect_four**. Pour le charger, il faut lancer la commande `[connect_four].`
    Pour lancer une partie, il suffit de taper la commande `start_game(Mode1,Mode2).`, en précisant les valeurs souhaitées pour les différents modes de jeu. Par exemple, pour faire jouer un joueur humain contre une IA complète, il faut lancer : `start_game('human','full_alert').`.
Les différents modes disponibles sont :
    * attack\_max   : une IA offensive (va essayer de gagner sans se soucier du joueur adverse)
    * attack\_sum   : une IA offensive plus efficace
    * attack\_alert : une IA offensive encore plus efficace
    * defense\_sum  : une IA défensive (va essayer de bloquer l'adversaire sans se soucier de gagner)
    * full\_sum     : une IA complète, qui cherche à gagner tout en empêchant l'adversaire de gagner
    * full\_alert   : une IA complète plus efficace

Si le jeu est lancé avec un ou deux modes qui ne correspondent pas aux modes précisés ci-dessus, le mode par défaut sera utilisé. Il s'agit d'un mode de jeu aléatoire.
   Pour le Mode1 uniquement, il est possible de mettre le mode 'human'. Le programme va alors demander à l'utilisateur le numéro de la colonne dans laquelle il souhaite jouer. Ce numéro varie de 1 à 7. Si l'utilisateur rentre une valeur qui n'est pas un entier compris entre 1 et 7, nous ne pouvons pas garantir le bon fonctionnement du programme.
   Lorsqu'un joueur aligne 4 pions, le jeu s'arrête, le vainqueur est annoncé, et il est possible de rejouer en utilisant de nouveau la commande `start_game(Mode1,Mode2).`.
