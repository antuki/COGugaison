#' \title{COGugaison}
#'Le découpage des territoires français, en particulier les communes, n’est pas un phénomène immuable. Chaque année certaines communes changent de codes, ou bien de nom, fusionnent ou encore se divisent. Certains périmètres supra-communaux changent également, comme celui des cantons qui a été récemment redéfini. C’est à l’Insee que revient le suivi de ces changements afin d’établir chaque année le code officiel géographique (COG).\cr
#'
#'Ce package R a alors pour objectif global de manipuler des données communales produites à différents millésimes et de les agréger à différents niveaux supra-communaux. Plus précisément, il permet actuellement de :
#' \itemize{
#' \item{détecter le millésime du code officiel géographique d’une table de données communales : fonction COG_akinator}
#' \item{visualiser les modifications communales (fusions, défusions, changements de codes ou de noms) qui ont eu lieu entre deux dates : modifications_communales}
#' \item{transformer des tables de données numériques en géographie au premier janvier d’une année souhaitée : changement_COG_varNum}
#' \item{transformer des typologies de communes en géographie au premier janvier d’une année souhaitée en ayant le choix entre plusieurs hypothèses de classement en cas de fusion de communes de classes différentes (attribuer la classe qui contient le plus de population, définir une classe absorbante ou une classe spécifique aux regroupements de plusieurs communes de classes différentes) : changement_COG_typo.Il est également possible d’isoler dans une table les communes fusionnées appartenant à des classes différentes : changement_COG_typo_details.}
#' \item{permettre d’agréger les tables de données communales à de nombreux échelons supra-communaux administratifs (EPCI, arrondissements, cantons-villes, départements, régions) ou d’étude (bassins de vie, zones d’emploi, unités urbaines, aires urbaines) : nivsupra.}
#' \item{gérer des cas particuliers comme les codes Insee des communes corses (modification_Corse) ou des arrondissements municipaux de Paris, Lyon, et Marseille (enlever_PLM)}} \cr
#' Il est à noter que la version actuellement en ligne du package correspond à un premier développement qui comporte des imperfections. L’idée est donc à ce stade que les personnes qui utilisent régulièrement des bases de données communales produites à différents millésimes testent ses fonctionnalités, détectent ses bugs et ses manques pour l’améliorer.
#' \title{détails}
#' Le code officiel géographique le plus récent du package est actuellement celui au 01/01/2017. \cr
#'
#' Les millésimes des COG qui peuvent être utilisés sont à ce stade les suivants : 1968, 1975, 1982, 1990, 1999, 2007 à 2017. \cr
#'
#' Les dates de référence des codes officiels géographiques utilisés dans COGugaison sont les suivantes :
#' \itemize{
#' \item{COG 1968 : à partir du 01/03/1968}
#' \item{COG 1975 : à partir du 20/02/1975}
#' \item{COG 1982 : à partir du 04/03/1982}
#' \item{COG 1990 : à partir du 05/03/1990}
#' \item{COG 1999 : à partir du 08/03/1999}
#' \item{Pour tous les autres COG : à partir du 01/01 de chaque année}} \cr
#'
#' Les différences entre les tables de passage Insee et non Insee sont les suivantes :\cr
#' \itemize{
#' \item{1982-03-03 (pris en compte par l'Insee seulement après le 04/03/1982): Flaignes-Havys (08169) est un rassemblement de Flaignes-Havys (08169), Havys (08221) [fusion simple].}
#' \item{2014-01-01 (pris en compte par l'Insee seulement au 01/01/2015) : Loisey (55298) s'est séparée en Loisey (55298), Culey (55138) [rétablissement].}
#' \item{1990-02-01 (pris en compte par l'Insee seulement après le 05/03/1990) : Le code commune de Oudon passe de 14624 à 14697 [changement de code dû à un changement de chef-lieu].}
#' \item{2014-01-07 (pris en compte par l'Insee  dès le 01/01/2016) : Tôtes est rattachée à Notre-Dame-de-Fresnay qui devient L'Oudon (changement de code de l'Oudon de 14697 à 14472) [transfert de chef-lieu].}
#' \item{1981-09-28 (pris en compte par l'Insee  dès le 20/02/1975) : Vaudreuil-Ex-Ensemble Urbain (27701) est créée à partir des parcelles d'Incarville (27351), de Léry (27365) , de Porte-Joie (27471) , de Poses  (27474) , de Saint-Étienne-du-Vauvray (27537), de Saint-Pierre-du-Vauvray (27598), de Tournedos-sur-Seine  (27651) et du Vaudreuil (27528) [création]. Cette situation étant complexe, nous avons pour le moment considéré que Vaudreuil-Ex-Ensemble Urbain (27701) est créée à partir de parcelles du Vaudreuil (27528) uniquement.}
#' \item{En 1968, les 4 communes qui auraient dû d'après le COG être codées 2B044,2B076,2B151 et 2A325 sont codées 20044,20076,20151 et 20325 dans les données Insee.}}
#' \title{references}
#' \itemize{
#' \item{\href{https://www.insee.fr/fr/information/2666684#titre-bloc-11}{historique des géographies communales (Insee)}}
#' \item{\href{https://www.insee.fr/fr/information/2028028}{tables d'appartenance des communes aux différents niveaux géographiques (Insee)}}}
#' \title{voir aussi}
#' \link{changement_COG_varNum},\link{changement_COG_typo}, \link{changement_COG_typo_details},  \link{COG_akinator}, \link{enlever_PLM}, \link{modification_Corse}, \link{modifications_communales},\link{nivsupra},\link{apparier_COG},\link{modification_Oudon}
