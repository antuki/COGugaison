#' @title Créer une table de communes fusionnées appartenant à des classes différentes selon une typologie
#' @name changement_COG_typo_details
#' @description Isoler dans une table les communes fusionnées entre deux géographies communales qui appartiennent à des classes différentes selon une typologie.
#' @param table_entree correspond à la table à transformer en une autre géographie
#' @param annees est un vecteur qui liste l'ensemble des années qui séparent le code officiel géographique de départ et d'arrivée. Par exemple c(1968:1985). Le package rend possible l'utilisation de tables de passages d'une année de COG vers une année antiérieure (par exemple c(2016:2014)).
#' @param codgeo_entree est une chaîne de caractères qui indique le nom de la variable contenant les codes Insee communaux. Par défaut, il s'agit du nom de la première colonne de table_entree.
#' @param typo est une chaîne de caractères qui indique le nom de la typologie à convertir.
#' @param donnees_insee vaut TRUE si les données manipulées sont produites par l'Insee. En effet, quelques rares modifications communales (la défusion des communes Loisey et Culey au 1er janvier 2014 par exemple) ont été prises en compte dans les bases de données communales de l'Insee plus tard que la date officielle.
#' @details
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
#' @references
#' \itemize{
#' \item{\href{https://www.insee.fr/fr/information/2666684#titre-bloc-11}{historique des géographies communales (Insee)}}
#' \item{\href{https://www.insee.fr/fr/information/2028028}{tables d'appartenance des communes aux différents niveaux géographiques (Insee)}}}
#' @seealso \link{changement_COG_varNum}, \link{changement_COG_typo}, \link{COG_akinator}, \link{enlever_PLM}, \link{modification_Corse}, \link{modifications_communales},\link{nivsupra},\link{apparier_COG},\link{modification_Oudon},\link{trajectoire_commune}
#' @export
#' @examples
#' ## Exemple 1
#' # Ici nous allons transformer les deux typologies (typoA et typoB) de la table exemple_pop en géographie communale au 1er janvier 2017 (au lieu de 2014).
#' # L'hypothèse de classement en cas de fusion de communes (*methode_fusion*) choisie est celle d'une classe spécifique (*methode_difference*, classe appelée *mot_difference*="differents") aux regroupements de plusieurs communes de classes différentes. Les autres hypothèses possibles auraient pu être l'hypothèse du maximum de population *methode_max_pop* ou de classe absorbante *methode_classe_absorbante*.
#' exemple_popcom_COG2017_typo <- changement_COG_typo(table_entree=exemple_popcom[,-2],annees=c(2014:2017),methode_fusion="methode_difference",typos=c("typoA","typoB"),mot_difference = "differents",libgeo=T,donnees_insee=T)
#' head(exemple_popcom_COG2017_typo)
#' # Nous allons maintenant isoler dans une table les communes fusionnées appartenant à des classes différentes, ici selon la typologie "typoA" entre 2014 et 2015, 2015 et 2016 et 2016 et 2017.
#' details_exemple_popcom_COG2017_typo <- changement_COG_typo_details(table_entree=exemple_popcom[,-2],annees=c(2014:2017),typo="typoA", donnees_insee=T)
#' head(details_exemple_popcom_COG2017_typo[["2014_2015"]])
#' head(details_exemple_popcom_COG2017_typo[["2015_2016"]])
#' head(details_exemple_popcom_COG2017_typo[["2016_2017"]])

changement_COG_typo_details <- function(table_entree,annees,codgeo_entree=colnames(table_entree)[1],typo, donnees_insee=T){

  inter <- intersect(c(1968,1975,1982,1990,1999,2008:2017),annees)
  if(annees[1]<=annees[length(annees)]){
    inter <- inter[order(inter)]
  } else{
    inter <- rev(inter[order(inter)])
  }
  annees <- unique(c(annees[1]:inter[1],inter,inter[length(inter)]:annees[length(annees)]))


  for (i in 1:(length(annees)-1)){

      if(annees[i] < annees[i + 1]){vecteur <- c(1968, 1975, 1982, 1990, 1999, 2013, 2014)} else{vecteur <-c(1975, 1982, 1990, 1999, 2008, 2014, 2015)}
      if(donnees_insee==T & annees[i]%in%vecteur){
      assign(paste0("PASSAGE_",annees[i],"_",annees[i+1]),get(paste0("PASSAGE_",annees[i],"_",annees[i+1],"_insee")))
      }

      provisoire <- merge(get(paste0("PASSAGE_",annees[i],"_",annees[i+1])), table_entree, by.x=paste0("cod",annees[i]),by.y=codgeo_entree, all.x=T, all.y=F)
      provisoire_court <- provisoire[,c(paste0("cod",annees[i]),paste0("cod",annees[i+1]),"annee","typemodif","ratio",typo)]

      table_f <- provisoire_court[(provisoire_court$typemodif=="f"),]
      table_f_liste <- lapply(unique(with(table_f,get(paste0("cod",annees[i+1])))),function(x){table_f[which(with(table_f,get(paste0("cod",annees[i+1])))==x),]})
      table_f_avecpb <-table_f_liste[which(lapply(table_f_liste, FUN=function(x){ifelse(length(unique(x[,6]))==1,T,F)})==F)]
      table_f_avecpb <- do.call("rbind", table_f_avecpb)

      if(!is.null(table_f_avecpb)){
      table_f_avecpb <- table_f_avecpb[order(table_f_avecpb[,paste0("cod",annees[i+1])]),c(paste0("cod",annees[i]),paste0("cod",annees[i+1]),typo)]

      if(donnees_insee==T & (annees[i]%in%c(1968,1975,1982,1990,1999,2014))){
        assign(paste0("COG",annees[i]),get(paste0("COG",annees[i],"_insee")))
      }
      table_f_avecpb <- merge(table_f_avecpb,get(paste0("COG",annees[i]))[,1:2],by.x=paste0("cod",annees[i]),by.y="CODGEO",all.x=T,all.y=F)
      colnames(table_f_avecpb)[ncol(table_f_avecpb)]<-paste0("lib",annees[i])

      if(donnees_insee==T & (annees[i+1]%in%c(1968,1975,1982,1990,1999,2014))){
        assign(paste0("COG",annees[i+1]),get(paste0("COG",annees[i+1],"_insee")))
      }
      table_f_avecpb <- merge(table_f_avecpb,get(paste0("COG",annees[i+1]))[,1:2],by.x=paste0("cod",annees[i+1]),by.y="CODGEO",all.x=T,all.y=F)
      colnames(table_f_avecpb)[ncol(table_f_avecpb)]<-paste0("lib",annees[i+1])

      table_f_avecpb <- table_f_avecpb[,c(paste0("cod",annees[i]),paste0("lib",annees[i]),paste0("cod",annees[i+1]),paste0("lib",annees[i+1]),typo)]

      }

      assign(paste0("resultats_",annees[i],"_",annees[i+1]),table_f_avecpb)
      table_entree <- changement_COG_typo(table_entree=table_entree,annees=c(annees[i]:annees[i+1]),codgeo_entree=codgeo_entree,typos=typo, methode_fusion="methode_difference",donnees_insee=donnees_insee)
  }

  liste <- mget(ls(pattern = "^resultats_",envir=))
  names(liste)<- gsub("^resultats_","",names(liste))
  return(liste)

}
