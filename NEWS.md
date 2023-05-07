# COGugaison 1.0.0

## nouveautés

### juillet 2017

* Création des fonctions `apparier_COG` et `modification_Oudon`.
* Ajout du paramètre *COG* dans la fonction `nivsupra` permettant d'agréger à des niveaux supra-communaux de plusieurs millésimes (et pas uniquement le dernier).
* Suppression du paramètre *liste_complete* de `COG_akinator` qui n'était pas très utile.

### août 2017

* ajout des options methode_fusion="methode_classe_absorbee" et methode_fusion="methode_classe_absorbee" dans `changement_COG_typo` et des paramètres associés *classe_absorbee* et *mot_fusion*
* ajout des niveaux supra-communaux des COG de 2008 à 2013

### novembre 2017

* création des fonctions `trajectoire_commune` et `trajectoire_commune_shiny`

### avril 2018

* mise à jour des données de population du COG2017 suite à la publication des données de population du RP2015 (géographie 2017) sorties en janvier 2018 
* ajout du COG2018 sortie en avril 2018

### mars 2019

* mise à jour de COG2019, PASSAGE_2018_2019 et PASSAGE_2019_2018 (informations partielles en attendant de la sortie officielle du COG par l'Insee plus tard dans l'année)

### mai 2019

* mise à jour de table_supracom_2019 et libelles_supracom_2019 [la fonction modifications_communales n'est pour le moment plus maintenue en 2019 suite à un changement de fichier fourni par l'Insee]



## correction de bugs / optimisation du code

### août 2017

* NA pris en compte dans la fonction `changement_COG_typo`
* transformation automatique des variables de typologie de type "factor" dans `changement_COG_typo`. 

### février 2018

* création des variables globales annees_possibles et annee_ref, intégrées dans les différentes fonctions pour faciliter les mises à jours de COG notamment
* ajout de tests de vérification du contenu des paramètres (fonctions types match.arg ou if(){stop()}) dans l'ensemble des fonctions

# COGugaison 1.0.1

## nouveautés

### décembre 2019

* premier test de modification de version de COGugaison
* ajout d'une enquête de satisfaction à l'ouverture du package

# COGugaison 1.0.2

## nouveautés

### février 2020

* mise à jour totale du COG2020 

### mars 2021

* mise à jour totale du COG2021

# COGugaison 1.0.4

## nouveautés

### octobre 2021

* ajout de la fonction diag_COG

# COGugaison 1.0.5

## nouveautés

### juillet 2022

* mise à jour totale du COG2022 

## correction de bugs / optimisation du code

### juillet 2022

* voir issue 17 sur des coquilles repérées dans enlever_PLM et modification_Corse. Résolue dans le commit 1ec8a4b58ee9652d72f821220200ff0aee872df9


# COGugaison 1.0.6

## nouveautés

### février 2023

* mise à jour du COG2023 sauf la table nivsupra incomplète

### mai 2023

* ajout de la table nivsupra COG2023
* compression des fichiers de data grâce à `tools::resaveRdaFiles("data")`
