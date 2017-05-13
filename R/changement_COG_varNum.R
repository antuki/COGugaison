#' @title Changer les variables numériques de géographie communale
#' @name changement_COG_varNum
#' @description Transformer des tables de données numériques en géographie au premier janvier d'une année souhaitée.
#' @param table_entree correspond à la table à transformer en une autre géographie
#' @param annees est un vecteur qui liste l'ensemble des années qui séparent le code officiel géographique de départ et d'arrivée. Par exemple c(1968:1985). Le package rend possible l'utilisation de tables de passages d'une année de COG vers une année antiérieure (par exemple c(2016:2014)).
#' @param codgeo_entree est une chaîne de caractères qui indique le nom de la variable contenant les codes Insee communaux. Par défaut, il s'agit du nom de la première colonne de table_entree.
#' @param var_num est un vecteur de chaînes de caractères qui indique les noms des variables numériques à convertir. Par défaut, il s'agit de l'ensemble des variables de types numériques dans table_entree.
#' @param agregation vaut TRUE si la table souhaitée doit sommer toutes les lignes qui concernent une même commune et FALSE si l'on souhaite volontairement conserver les doublons dans les codes commune (dans les tables de flux par exemple). Si agregation = F, les variables de type caractère sont alors conservées comme telles ou dupliquées en cas de défusion et les variables numériques sommées en cas de fusion ou réparties proportionnellement à la population de chaque commune en cas de défusion.
#' @param libgeo doit être conservé à NULL si la table souhaitée ne contient pas de libellés de communes, sinon il doit indiquer le nom de la variable renseignant sur le libellé de la commune (celui présent dans la table si il existe déjà ou tout autre nom s'il doit être ajouté).
#' @param donnees_insee vaut TRUE si les données manipulées sont produites par l'Insee. En effet, quelques rares modifications communales (la défusion des communes Loisey et Culey au 1er janvier 2014 par exemple) ont été prises en compte dans les bases de données communales de l'Insee plus tard que la date officielle. Pour tous les COG officiels datant d'avant 2008, seules les tables de passage Insee sont disponibles dans ce package.
#' @details
#' Le code officiel géographique de référence du package est actuellement celui au 01/01/2017. Les données communales devront être dans ce COG pour être agrégées en niveaux supra-communaux (fonction nivsupra). \cr
#'
#' Les autres codes officiels géographiques utilisés dans COGugaison sont les suivants :
#' \itemize{
#' \item{COG 1968 : à partir du 01/03/1968 (donnees_insee=T obligatoirement)}
#' \item{COG 1975 : à partir du 20/02/1975 (donnees_insee=T obligatoirement)}
#' \item{COG 1982 : à partir du 04/03/1982 (donnees_insee=T obligatoirement)}
#' \item{COG 1990 : à partir du 05/03/1990 (donnees_insee=T obligatoirement)}
#' \item{COG 1999 : à partir du 08/03/1999 (donnees_insee=T obligatoirement)}
#' \item{COG 2008 à 2017 : à partir du 01/01 de chaque année (donnees_insee=T ou F)}} \cr
#'
#' Les différences entre les tables de passage Insee et non Insee sont les suivantes :\cr
#' \itemize{
#' \item{1982-03-03 (pris en compte par l'Insee seulement après le 04/03/1982): Flaignes-Havys (08169) est un rassemblement de Flaignes-Havys (08169), Havys (08221) [fusion simple].}
#' \item{2014-01-01 (pris en compte par l'Insee seulement au 01/01/2015) : Loisey (55298) s'est séparée en Loisey (55298), Culey (55138) [rétablissement].}
#' \item{1990-02-01 (pris en compte par l'Insee seulement après le 05/03/1990) : Le code commune de Oudon passe de 14624 à 14697 [changement de code dû à un changement de chef-lieu].}}
#' @references
#' \itemize{
#' \item{\href{https://www.insee.fr/fr/information/2666684#titre-bloc-11}{historique des géographies communales (Insee)}}
#' \item{\href{https://www.insee.fr/fr/information/2028028}{tables d'appartenance des communes aux différents niveaux géographiques (Insee)}}}
#' @seealso
#' \link{changement_COG_typo}, \link{changement_COG_typo_details},  \link{COG_akinator}, \link{enlever_PLM}, \link{modification_Corse}, \link{modifications_communales},\link{nivsupra}
#' @export
#' @examples
#' ## Exemple 1
#' # Ici, nous allons transformer les variables numériques de la table *exemple_pop* afin de récupérer les données de population et de superficie des communes au 1er janvier 2017 (au lieu de 2014).
#' exemple_popcom_COG2017_num <- changement_COG_varNum(table_entree=exemple_popcom,annees=c(2014:2017),agregation=T,libgeo=T,donnees_insee=T)
#' head(exemple_popcom_COG2017_num)
#' ## Exemple 2
#' # La fonction peut également s'appliquer à des tables de flux (codes communes pouvant comporter des doublons) grâce à l'option agregation = FALSE.
#' exemple_flux_COG2017 <- changement_COG_varNum(table_entree=exemple_flux,annees=c(2014:2017),codgeo_entree="COMMUNE",agregation=F,libgeo=F,donnees_insee=T)
#' exemple_flux_COG2017 <- changement_COG_varNum(table_entree=exemple_flux_COG2017,annees=c(2014:2017),codgeo_entree="DCLT",agregation=F,libgeo=F,donnees_insee=T)
#' @encoding UTF-8


changement_COG_varNum <- function(table_entree,annees,codgeo_entree=colnames(table_entree)[1],var_num=colnames(table_entree)[sapply(table_entree, is.numeric)],agregation=T,libgeo=F,donnees_insee=T){

  inter <- intersect(c(1968,1975,1982,1990,1999,2008:2017),annees)
  if(annees[1]<=annees[length(annees)]){
    inter <- inter[order(inter)]
  } else{
    inter <- rev(inter[order(inter)])
  }
  annees <- unique(c(annees[1]:inter[1],inter,inter[length(inter)]:annees[length(annees)]))


  for (i in 1:(length(annees)-1)){

    if(donnees_insee==T){
     assign(paste0("PASSAGE_",annees[i],"_",annees[i+1]),get(paste0("PASSAGE_",annees[i],"_",annees[i+1],"_insee")))
    }

    provisoire <- merge(table_entree,get(paste0("PASSAGE_",annees[i],"_",annees[i+1])),by.x=codgeo_entree,by.y=paste0("cod",annees[i]),all.x=T,all.y=F)
    provisoire[which(is.na(with(provisoire,get(paste0("cod",annees[i+1]))))),"ratio"] <- 1
    provisoire[which(is.na(with(provisoire,get(paste0("cod",annees[i+1]))))),paste0("cod",annees[i+1])] <- as.character(provisoire[which(is.na(with(provisoire,get(paste0("cod",annees[i+1]))))),codgeo_entree])
    provisoire[,c(var_num)] <- (provisoire[,c(var_num,"ratio")] * provisoire[,"ratio"])[,-(length(var_num)+1)]
    provisoire <- provisoire[,-which(colnames(provisoire)==codgeo_entree)]
    provisoire  <- provisoire[,-((ncol(provisoire)-2):ncol(provisoire))]
    names(provisoire )[which(names(provisoire)==paste0("cod",annees[i+1]))]<- codgeo_entree
    table_finale <- provisoire[,colnames(table_entree)]
    table_entree <- table_finale
  }

  if(libgeo==T){
    if(donnees_insee==T){
      assign(paste0("COG",annees[length(annees)]),get(paste0("COG",annees[length(annees)],"_insee")))
    }
    table_finale <- merge(table_finale,get(paste0("COG",annees[length(annees)]))[,c(1,2)],by.x=codgeo_entree,by.y="CODGEO",all.x=T,all.y=F)
    table_finale <- table_finale[,c(1,ncol(table_finale),2:(ncol(table_finale)-1))]
  }

  if(agregation==T){
    if(libgeo==T){
    table_libgeo <- table_finale[!duplicated(table_finale[,c(1,2)]),c(1,2)]
    }
    table_finale <- aggregate(table_finale[,c(var_num)],by =list(with(table_finale,get(codgeo_entree))),FUN=sum)
    colnames(table_finale)<- c(codgeo_entree,var_num)
    if(libgeo==T){
    table_finale <- merge(table_finale, table_libgeo,by=codgeo_entree,all.x=T,all.y=F)
    table_finale <- table_finale[,c(1,ncol(table_finale),var_num)]
    }
  }

  table_finale <- table_finale[order(table_finale[,codgeo_entree]),]

  return(table_finale)

}


