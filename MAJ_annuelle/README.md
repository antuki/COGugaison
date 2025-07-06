**Mise à jour annuelle de COGugaison**

Remarque : bien regarder les fonctions de ce dossier avant de se lancer dans la procédure

1) télécharger les fichiers
https://www.insee.fr/fr/information/7671867 =>   table_passage_annuelle_2025.zip + table_passage_geo2003_geo2024.zip (non utilisé) 
https://www.insee.fr/fr/information/7671844 => table-appartenance-geo-communes-25.zip 
https://www.insee.fr/fr/information/8377162 => cog_ensemble_2025_csv.zip

2) A partir du zip table-appartenance-geo-communes-25.zip, créer les csv associés table-appartenance-geo-communes-2025_libelles.csv et table-appartenance-geo-communes-2025.csv 
(supprimer les lignes du haut et enregistrer en csv, séparateur ; et ANSI windows1252)
ranger tous les nouveaux fichiers dans un dossier à part. 

3) créer COG2025.csv et COG2025_insee.csv à partir de ce même fichier (3 colonnes : CODGEO, LIBGEO, POP, en renseignant des populations au hasard par exemple 100)
sep ;

4) Si ce n'a pas été fait mettre à jour les COG de l'année n-1 (COG2024 quand on màj le COG2025) avec les données de populations municipales sorties en janvier
Utiliser le code changer_pop_legales_en_janvier.R
Rq : c'est un fichier de pop légale mais il contient la pop municipale aussi. Pop légale 2022 pour le COG 2024 : https://www.insee.fr/fr/statistiques/8290591?sommaire=8290669
Fichier d'ensemble (France hors Mayotte) > France hors mayotte csv (ensemble.zip se télécharge) > 
ATTENTION à Paris lyon marseille : je l'ai mal géré au dernier COG donc pour la prochaine fois insérer dans le workflow PLM
ATTENTION :  dans ces fichiers les libellés sont mal foutus (Espaces après les L') ne prendre que les populations
On laisse Mayotte à population vide 
Remarque, en cas de scission (c'était le cas pour le COG 2024 avec deux scissions, ce sera également le cas en 2026 pour 2025), il faut mettre à jour le fichier passage_2023_2024. 

5) créer la table de passage passage_2024_2025 : Je mets désormais les date de modif au 1er janvier 2025 => code : creer_table_passage.R
Remarque : il faut gérer à la main les c et les défusions

6) créer la table de passage inverse passage_2025_2024 grâce au programme creer_table_passage_inverse.R
7) corriger les populations de COG2025 en faisant grâce au programme creer_pop_fictive_COGn.R


8) Aller sur developpement.R héberger les nouveaux fichiers dans historiq (non maintenu), COG et nivsupra.Rdata dans le dossier data du package (NE PAS OUBLIER) après avoir mis les new fichiers dans donnees 
Rq : sauvegarder les anciennes données .RData au cas où

9) changer les différentes informations : fichier A_GLOBAL VARIABLE, news.md, ctrl+maj+f de 2023 dans /Projets_R/COGugaison/COGugaison/R
10) Changer la version de COGugaison dans la description
11) faire un Clean and Install
12) tester toutes les fonctions de la vignette avec des paramètres 2025 dans les fonctions (dans developpement.R)
13) Générer les versions tar.gz et zip et les mettre sur github et changer la description de la page git en indiquant le dernier millésime dispo
Le tour est joué


A faire la prochaine fois : 
Dans passage 2024_2025 : changer le ratio de la défusion dans les chiffres de pop du RP (2024) en COG2025 seront sortis (Il y a une défusion)


