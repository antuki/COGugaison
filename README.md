# COGugaison

Le package `COGugaison` a pour objectif global de manipuler des données communales produites à différents millésimes. Il permet actuellement de : 
- détecter le millésime du code officiel géographique d'une table de données communales : fonction `COG_akinator`
- visualiser les modifications communales (fusions, défusions, changements de codes ou de noms) qui ont eu lieu entre deux dates : `modifications_communales`
- transformer des tables de données numériques en géographie au premier janvier d'une année souhaitée : `changement_COG_varNum`
- transformer des typologies de communes en géographie au premier janvier d'une année souhaitée en ayant le choix entre plusieurs hypothèses de classement en cas de fusion de communes de classes différentes (attribuer la classe qui contient le plus de population, définir une classe absorbante ou une classe spécifique aux regroupements de plusieurs communes de classes différentes) : `changement_COG_typo`.Il est également possible d'isoler dans une table les communes fusionnées appartenant à des classes différentes : `changement_COG_typo_details`.
- permettre d'agréger les tables de données communales à de nombreux échelons supra-communaux administratifs (EPCI, arrondissements, cantons-villes, départements, régions) ou d'étude (bassins de vie, zones d'emploi, unités urbaines, aires urbaines) : `nivsupra`.
- gérer des cas particuliers comme les codes Insee des communes corses (`modification_Corse`) ou des arrondissements municipaux de Paris, Lyon, et Marseille (`enlever_PLM`)

Il est à noter que la version actuellement en ligne du package correspond à un premier développement qui comporte  des imperfections. L'idée est donc à ce stade que les personnes qui utilisent régulièrement des bases de données communales produites à différents millésimes testent ses fonctionnalités, détectent ses bugs et ses manques pour l'améliorer.  

Il est également important de souligner que les données utilisées ici s'appuient sur des tables publiées par l'Insee :
- [l'historique des géographies communales](https://www.insee.fr/fr/information/2666684#titre-bloc-11)
- [les tables d'appartenance des communes aux différents niveaux géographiques](https://www.insee.fr/fr/information/2028028)

Tant que le package n'est pas stabilisé, il est évidemment conseillé de se référer aux données officielles de l'Insee en cas de doute sur un résultat.  

Pour installer le package `COGugaison` et le charger dans R :
 
    devtools::install_github("antuki/COGugaison")
    library(COGugaison)
