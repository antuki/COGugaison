#' @title Modifier les codes Insee de l'ancienne commune de l'Oudon (Calvados, 14)
#' @name modification_Oudon
#' @description Modifier les codes Insee de l'ancienne commune de l'Oudon (Calvados, 14). En effet, la commune de l'Oudon a toujours été codée "14697" puis a changé de code devenant "14472" à partir du 07/01/2014 suite à un transfert de chef lieu. Cette commune a depuis disparu puisqu'elle est devenue commune déléguée au sein de Saint-Pierre-en-Auge (14654).
#' @param table_entree correspond à la table (ou le vecteur, cf. paramètre vecteur_entree) à transformer de manière à modifier le code de la commune de l'Oudon.
#' @param codgeo_entree est une chaîne de caractères qui indique le nom de la variable contenant les codes communes Insee. Par défaut, il s'agit du nom de la première colonne de table_entree.
#' @param vecteur_entree vaut TRUE si table_entree est un simple vecteur.
#' @param donnees_insee_entree vaut TRUE si les données en entrée sont des bases de données de l'Insee
#' @param donnees_insee_sortie vaut TRUE si les données souhaitées en sorties doivent respecter le même COG que les données Insee
#' @param COG indique l'année de COG de la table considérée. (exemple 1968). Par défaut renvoie l'année retournée par la fonction COG_akinator
#' @details
#' Le code officiel géographique le plus récent du package est actuellement celui au 01/01/2022. \cr
#'
#' Les millésimes des COG qui peuvent être utilisés sont à ce stade les suivants : 1968, 1975, 1982, 1990, 1999, 2008 à 2022. \cr
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
#' @seealso \link{changement_COG_varNum}, \link{changement_COG_typo},\link{changement_COG_typo_details}, \link{COG_akinator}, \link{enlever_PLM}, \link{modifications_communales},\link{nivsupra},\link{modification_Corse},\link{apparier_COG}
#' @export
#' @examples
#' ## Exemple 1
#' # Ici, nous allons remplacer les codes communes de l'Oudon dans une table de l'Insee (en COG 2014 mais que nous considérons en COG 2015 uniquement pour cet exemple).
#' head(exemple_popcom[which(exemple_popcom$CODGEO%in%c("14472","14697")),])
#' exemple_popcom_oudon <- modification_Oudon(table_entree=exemple_popcom,donnees_insee_entree=T,donnees_insee_sortie=F,COG=2015)
#' head(exemple_popcom_oudon[which(exemple_popcom$CODGEO%in%c("14472","14697")),])
#' @encoding UTF-8


modification_Oudon <- function(table_entree,codgeo_entree = colnames(table_entree)[1], vecteur_entree=is.vector(table_entree),donnees_insee_entree=TRUE,donnees_insee_sortie=TRUE,COG=as.numeric(substr(COG_akinator(vecteur_codgeo = table_entree[,codgeo_entree],donnees_insee = donnees_insee_entree),4,7))){
  if(!codgeo_entree%in%colnames(table_entree)){ #NEW
    stop(paste0("codgeo_entree doit être une colonne de table_entree."))
  }
  if(!COG%in%annees_possibles){ #NEW
    stop(paste0("COG doit être contenu dans ",paste0(annees_possibles,collapse = ", ")))
  }

  table_sortie <- table_entree

  if(vecteur_entree==T){
    vecteur <- table_sortie
  } else{
    vecteur <- table_sortie[,codgeo_entree]
  }
  if(donnees_insee_sortie){
    if(COG < 2016){
      vecteur[which(vecteur%in%c("14472"))] <-"14697"
    } else if (COG == 2016){
      vecteur[which(vecteur%in%c("14697"))] <-"14472"
    }
  } else{
    if(COG < 2015){
      vecteur[which(vecteur%in%c("14472"))] <-"14697"
    } else if (COG == 2015){
      vecteur[which(vecteur%in%c("14697"))] <-"14472"
    }
  }


  if(vecteur_entree==T){
    table_sortie <- vecteur
  } else{
    table_sortie[,codgeo_entree] <- vecteur
  }

  return(table_sortie)
}
