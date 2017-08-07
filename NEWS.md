# COGugaison 1.0.0

## nouveautés

### juillet 2017

* Création des fonctions `apparier_COG` et `modification_Oudon`.
* Ajout du paramètre *COG* dans la fonction `nivsupra` permettant d'agréger à des niveaux supra-communaux de plusieurs millésimes (et pas uniquement le dernier).
* Suppression du paramètre *liste_complete* de `COG_akinator` qui n'était pas très utile.

### août 2017

* ajout des options methode_fusion="methode_classe_absorbee" et methode_fusion="methode_classe_absorbee" dans `changement_COG_typo` et des paramètres associés *classe_absorbee* et *mot_fusion*
* ajout des niveaux supra-communaux des COG de 2008 à 2013


## correction de bugs

### août 2017

* NA pris en compte dans la fonction `changement_COG_typo`
* transformation automatique des variables de typologie de type "factor" dans `changement_COG_typo`. 
