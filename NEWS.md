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

## correction de bugs / optimisation du code

### août 2017

* NA pris en compte dans la fonction `changement_COG_typo`
* transformation automatique des variables de typologie de type "factor" dans `changement_COG_typo`. 

### février 2018

* création des variables globales annees_possibles et annee_ref, intégrées dans les différentes fonctions pour faciliter les mises à jours de COG notamment
* ajout de tests de vérification du contenu des paramètres (fonctions types match.arg ou if(){stop()}) dans l'ensemble des fonctions
