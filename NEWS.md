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
[penser plus tard à remodifier les fonctions nivsupra et modifications_communales en restaurant annee_ref au moment de la MaJ officielle du COG]

## correction de bugs / optimisation du code

### août 2017

* NA pris en compte dans la fonction `changement_COG_typo`
* transformation automatique des variables de typologie de type "factor" dans `changement_COG_typo`. 

### février 2018

* création des variables globales annees_possibles et annee_ref, intégrées dans les différentes fonctions pour faciliter les mises à jours de COG notamment
* ajout de tests de vérification du contenu des paramètres (fonctions types match.arg ou if(){stop()}) dans l'ensemble des fonctions
