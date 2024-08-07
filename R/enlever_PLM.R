#' @title Remplacer les arrondissements municipaux par les codes communes
#' @name enlever_PLM
#' @description Remplacer les codes Insee des arrondissements municipaux de Paris, Lyon et Marseille par leurs codes communes respectifs.
#' @param table_entree correspond à la table (ou le vecteur, cf. paramètre vecteur_entree) à transformer de manière à remplacer les codes des arrondissements municipaux de Paris, Lyon et Marseille par leurs codes Insee communaux.
#' @param codgeo_entree est une chaîne de caractères qui indique le nom de la variable contenant les codes communes Insee et les arrondissements municipaux de Paris, Lyon et Marseille. Par défaut, il s'agit du nom de la première colonne de table_entree.
#' @param libgeo doit être conservé à NULL si la table souhaitée ne contient pas de libellés de communes, sinon il doit indiquer le nom de la variable renseignant sur le libellé de la commune (celui présent dans la table si il existe déjà ou tout autre nom s'il doit être ajouté).
#' @param agregation vaut TRUE si la table souhaitée doit sommer toutes les lignes qui concernent une même commune et FALSE si l'on souhaite volontairement conserver les doublons dans les codes commune (dans les tables de flux par exemple). Si agregation = F, les variables de type caractère sont alors conservées comme telles ou dupliquées en cas de défusion et les variables numériques sommées en cas de fusion ou réparties proportionnellement à la population de chaque commune en cas de défusion.
#' @param vecteur_entree vaut TRUE si table_entree est un simple vecteur.
#' @details
#' Le code officiel géographique le plus récent du package est actuellement celui au 01/01/2024. \cr
#'
#' Les millésimes des COG qui peuvent être utilisés sont à ce stade les suivants : 1968, 1975, 1982, 1990, 1999, annuel à partir de 2008. \cr
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
#' @references
#' \itemize{
#' \item{\href{https://www.insee.fr/fr/information/2666684#titre-bloc-11}{historique des géographies communales (Insee)}}
#' \item{\href{https://www.insee.fr/fr/information/2028028}{tables d'appartenance des communes aux différents niveaux géographiques (Insee)}}}
#' @seealso \link{changement_COG_varNum}, \link{changement_COG_typo},\link{changement_COG_typo_details}, \link{COG_akinator}, \link{modification_Corse}, \link{modifications_communales},\link{nivsupra},\link{apparier_COG},\link{modification_Oudon},\link{trajectoire_commune}
#' @export
#' @examples
#' ## Exemple 1
#' # Ici, nous allons remplacer les arrondissement municipaux par les codes communaux dans la table exemple_flux pour les variables COMMUNE puis DCLT.
#' exemple_flux_sansPLM <-enlever_PLM(table_entree = exemple_flux, codgeo_entree = "COMMUNE", libgeo = NULL, agregation = FALSE, vecteur_entree = FALSE)
#' exemple_flux_sansPLM <-enlever_PLM(table_entree = exemple_flux_sansPLM, codgeo_entree = "DCLT", libgeo = NULL, agregation = FALSE, vecteur_entree = FALSE)

enlever_PLM <- function(table_entree, codgeo_entree = colnames(table_entree)[1], libgeo = NULL, agregation = TRUE, vecteur_entree = is.vector(table_entree)){
  if(!vecteur_entree && !codgeo_entree%in%colnames(table_entree)){ #NEW
    stop(paste0("codgeo_entree doit être une colonne de table_entree."))
  }

  if(vecteur_entree == TRUE){
    table_entree[substr(table_entree,1,2)=="75"] <- "75056"
    table_entree[substr(table_entree,1,3)=="132"] <- "13055"
    table_entree[substr(table_entree,1,4)=="6938"] <- "69123"
    table_sortie <- unique(table_entree)
  } else{

  table_entree[substr(with(table_entree,get(codgeo_entree)),1,2)=="75", codgeo_entree] <- "75056"
  table_entree[substr(with(table_entree,get(codgeo_entree)),1,3)=="132", codgeo_entree] <- "13055"
  table_entree[substr(with(table_entree,get(codgeo_entree)),1,4)=="6938", codgeo_entree] <- "69123"

if(agregation == FALSE){
  table_sortie <- table_entree
} else{

  table_sortie <- aggregate(table_entree[,sapply(table_entree, function(x){is.numeric(x) | is.integer(x)})],by =list(with(table_entree,get(codgeo_entree))),FUN=sum)
  colnames(table_sortie)<- colnames(table_entree[,c(codgeo_entree,names(sapply(table_entree, function(x){is.numeric(x) | is.integer(x)})[sapply(table_entree, function(x){is.numeric(x) | is.integer(x)}) == TRUE]))])
  #table_sortie <- merge(table_sortie,table_entree[!duplicated(with(table_entree,get(codgeo_entree))),names(sapply(table_entree, function(x){is.numeric(x) | is.integer(x)})[sapply(table_entree, function(x){is.numeric(x) | is.integer(x)}) == FALSE])], by=codgeo_entree, all.x = TRUE, all.y = FALSE)
   }

if(!is.null(libgeo)){
  table_sortie[with(table_sortie,get(codgeo_entree))=="75056", libgeo] <- "Paris"
  table_sortie[with(table_sortie,get(codgeo_entree))=="13055", libgeo] <- "Marseille"
  table_sortie[with(table_sortie,get(codgeo_entree))=="69123", libgeo] <- "Lyon"
}

  }


return(table_sortie)
}
