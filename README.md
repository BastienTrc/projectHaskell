Groupe : Bastien Turco & Jules Singer - Telecom Nancy 3A IL

## Projet :
Le projet consiste à convertir des données structurées (S expressions) en JSON avec le langage Haskell.

## Initialisation
Pour compiler le projet, à la racine du projet : `cabal build`

Pour lancer le programme, à la racine du projet : `cabal run`

Pour nettoyer les fichiers générés lors de la compilation et supprimer les packages téléchargés dans le répertoire de cache de Cabal, à la racine du projet : `cabal clean`

Pour changer le port utilisé : aller dans le fichier config.json

Bonus UI effectué : http://127.0.0.1:3000/ui

## Librairies externes
Ce projet utilise la librairie `aeson` qui nous permet de facilement sérialiser le résultat obtenu par le parser.

## Choix particulier
Dans le cas ou une chaîne de caractère commence ou finit par `.` ou contient plus d'un `.`, nous avons considéré que la chaîne de caractère serait analysé en tant que symbole.

## Difficultés rencontrées
Au départ, les données de la requête POST étaient récupérés de manière brut en récupérant le body. Cela nous donnait une chaîne de caractères encodée dans le format URL ce qui posait problème pour le parser.

