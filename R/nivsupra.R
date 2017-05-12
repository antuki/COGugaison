#' @title Agréger des données à des échelons supra-communaux
#' @name nivsupra
#' @description Permettre d'agréger les tables de données communales à de nombreux échelons supra-communaux administratifs (EPCI, arrondissements, cantons-villes, départements, régions) ou d'étude (bassins de vie, zones d'emploi, unités urbaines, aires urbaines). La fonction `nivsupra` peut également s'appliquer à des tables de flux (non agrégées par codes communes) grâce à l'option agregation = FALSE. La table en entrée est alors conservée comme telle avec une nouvelle colonne qui correspond au niveau supracommunal du code commune considéré.
#' @param table_entree correspond à la table (ou le vecteur, cf. paramètre vecteur_entree) à transformer de manière à modifier les codes des communes corses.
#' @param codgeo_entree est une chaîne de caractères qui indique le nom de la variable contenant les codes communes Insee. Par défaut, il s'agit du nom de la première colonne de table_entree.
#' @param var_num est un vecteur de chaînes de caractères qui indique les noms des variables numériques à convertir. Par défaut, il s'agit de l'ensemble des variables de types numériques dans table_entree.
#' @param nivsupra est une chaîne de caractères qui indique le nom du niveau supra-communale souhaité :
#' - "DEP" : départements
#' - "REG" : régions
#' - "EPCI" : EPCI au 01/01/2017
#' - "ARR" : arrondissements au 01/01/2017
#' - "CV" : cantons-villes au 01/01/2017
#' - "ZE2010" : zones d'emploi 2010
#' - "UU2010" : unités urbaines 2010
#' - "AU2010" : aires urbaines 2010
#' - "BV2012" : bassins de vie 2012
#' @param nivsupra_nom indique le nom à donner au niveau supra-communal dans la table de sortie. Il faut par défaut la chaîne de caractère contenue dans nivsupra si agregation = T et la concaténation de nivsupra et codgeo_entree séparée d'un "_" si agregation = F.
#' @param na.nivgeo.rm
#' @param agregation vaut TRUE si la table souhaitée doit sommer toutes les lignes qui concernent une même commune et FALSE si l'on souhaite volontairement conserver les doublons dans les codes commune (dans les tables de flux par exemple). Si agregation = F, les variables de type caractère sont alors conservées comme telles ou dupliquées en cas de défusion et les variables numériques sommées en cas de fusion ou réparties proportionnellement à la population de chaque commune en cas de défusion.
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
#' @seealso \link{changement_COG_varNum}, \link{changement_COG_typo},\link{changement_COG_typo_details}, \link{COG_akinator}, \link{enlever_PLM}, \link{modification_Corse},\link{modifications_communales}
#' @export
#' @examples
#' ## Exemple 1
#' # Ici, après avoir transformé les données en géographie communale au 01/01/2017, nous agrégeons la population et la superficie des communes à l'échelon géographique des zones d'emploi afin d'obtenir une table des densités de population par zone d'emploi.
#' exemple_popcom_COG2017_num <- changement_COG_varNum(table_entree=exemple_popcom,annees=c(2014:2017),agregation=T,libgeo="LIBGEO",donnees_insee=T)
#' exemple_popcom_ZE2010 <- nivsupra(table_entree=exemple_popcom_COG2017_num,nivsupra="ZE2010",na.nivgeo.rm=F,agregation=T)
#' exemple_popcom_ZE2010$densite <- exemple_popcom_ZE2010$P12_POP / exemple_popcom_ZE2010$SUPERF
#' head(exemple_popcom_ZE2010)
#' ## Exemple 2
#' # Ici, on ajoute les colonnes ZE2010_COMMUNE et ZE2010_DCLT à la table exemple de flux domicile-travail.
#' exemple_flux_COG2017 <- changement_COG_varNum(table_entree=exemple_flux,annees=c(2014:2017),codgeo_entree="COMMUNE",agregation=F,libgeo=NULL,donnees_insee=T)
#' exemple_flux_COG2017 <- changement_COG_varNum(table_entree=exemple_flux_COG2017,annees=c(2014:2017),codgeo_entree="DCLT",agregation=F,libgeo=NULL,donnees_insee=T)
#' exemple_flux_COG2017_etZE <- nivsupra(table_entree=exemple_flux_COG2017,codgeo_entree="COMMUNE",nivsupra="ZE2010",na.nivgeo.rm=F,agregation=F)
#' exemple_flux_COG2017_etZE <- nivsupra(table_entree=exemple_flux_COG2017_etZE,codgeo_entree="DCLT",nivsupra="ZE2010",na.nivgeo.rm=F,agregation=F)
#' head(exemple_flux_COG2017_etZE)

nivsupra <- function(table_entree,codgeo_entree=colnames(table_entree)[1],var_num=colnames(table_entree)[sapply(table_entree, is.numeric)]
                     ,nivsupra, nivsupra_nom=ifelse(agregation==T,nivsupra,paste0(nivsupra,"_",codgeo_entree)),na.nivgeo.rm=F,agregation=T){

  table_entree <- merge(table_entree,table_supracom[,c("CODGEO",nivsupra)],by.x=codgeo_entree,by.y="CODGEO",all.x=T,all.y=F)
  colnames(table_entree)[which(colnames(table_entree)==nivsupra)]<- nivsupra_nom

  if(agregation==F){
    if(na.nivgeo.rm==F){
      table_sortie <- table_entree
    } else{
      table_sortie <- table_entree[which(is.na(table_entree[,nivsupra_nom])),]
    }

  } else{
    table_sortie <- aggregate(table_entree[,c(var_num)],by=list(table_entree[,nivsupra_nom]),FUN=sum)
    colnames(table_sortie) <- c(nivsupra_nom,var_num)
    table_sortie <- merge(table_sortie,libelles_supracom[which(libelles_supracom$NIVGEO==nivsupra),c(2,3)],by.x=nivsupra_nom,by.y="CODGEO",all.x=T,all.y=F)
    table_sortie <- table_sortie[,c(nivsupra_nom,"LIBGEO",var_num)]


    if(na.nivgeo.rm==F & !(is.null(nrow(table_entree[is.na(table_entree[,nivsupra_nom]),var_num])) | nrow(table_entree[is.na(table_entree[,nivsupra_nom]),var_num])==0)){
      if(length(var_num)>1){
        vect_NA <- c(NA,NA,colSums(table_entree[is.na(table_entree[,nivsupra_nom]),var_num]))
      } else {
        vect_NA <- c(NA,NA,sum(table_entree[is.na(table_entree[,nivsupra_nom]),var_num]))
      }
      names(vect_NA)[1:2] <- c(nivsupra_nom,"LIBGEO")
      table_sortie<- rbind.data.frame(table_sortie,vect_NA)
    }
  }

  return(table_sortie)
}




