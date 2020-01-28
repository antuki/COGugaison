#' @title Changer les variables numériques de géographie communale
#' @name changement_COG_varNum
#' @description Transformer des tables de données numériques en géographie au premier janvier d'une année souhaitée.
#' @param table_entree correspond à la table à transformer en une autre géographie
#' @param annees est un vecteur qui liste l'ensemble des années qui séparent le code officiel géographique de départ et d'arrivée. Par exemple c(1968:1985). Le package rend possible l'utilisation de tables de passages d'une année de COG vers une année antiérieure (par exemple c(2016:2014)).
#' @param codgeo_entree est une chaîne de caractères qui indique le nom de la variable contenant les codes Insee communaux. Par défaut, il s'agit du nom de la première colonne de table_entree.
#' @param var_num est un vecteur de chaînes de caractères qui indique les noms des variables numériques à convertir. Par défaut, il s'agit de l'ensemble des variables de types numériques dans table_entree.
#' @param agregation vaut TRUE si la table souhaitée doit sommer toutes les lignes qui concernent une même commune et FALSE si l'on souhaite volontairement conserver les doublons dans les codes commune (dans les tables de flux par exemple). Si agregation = F, les variables de type caractère sont alors conservées comme telles ou dupliquées en cas de défusion et les variables numériques sommées en cas de fusion ou réparties proportionnellement à la population de chaque commune en cas de défusion.
#' @param libgeo vaut TRUE si l'on veut rajouter dans la table une colonne nommée "nom_commune" qui indique le nom de la commune issu du code officiel géographique et FALSE sinon.
#' @param donnees_insee vaut TRUE si les données manipulées sont produites par l'Insee. En effet, quelques rares modifications communales (la défusion des communes Loisey et Culey au 1er janvier 2014 par exemple) ont été prises en compte dans les bases de données communales de l'Insee plus tard que la date officielle.
#' @details
#' Le code officiel géographique le plus récent du package est actuellement celui au 01/01/2019. \cr
#'
#' Les millésimes des COG qui peuvent être utilisés sont à ce stade les suivants : 1968, 1975, 1982, 1990, 1999, 2008 à 2019. \cr
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
#' @seealso
#' \link{changement_COG_typo}, \link{changement_COG_typo_details},  \link{COG_akinator}, \link{enlever_PLM}, \link{modification_Corse}, \link{modifications_communales},\link{nivsupra},\link{apparier_COG},\link{modification_Oudon},\link{trajectoire_commune}
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


changement_COG_varNum <- function(table_entree,annees,codgeo_entree=colnames(table_entree)[1],var_num=colnames(table_entree)[sapply(table_entree, is.numeric)],agregation=TRUE,libgeo=FALSE,donnees_insee=TRUE){

  if(!codgeo_entree%in%colnames(table_entree)){ #NEW
    stop(paste0("codgeo_entree doit être une colonne de table_entree."))
  }
  if(any(annees>annee_ref) | any(annees<1968)){ #NEW
    stop(paste0("annees ne doit contenir que des années comprises entre 1968 et ",annee_ref,"."))
  }
  if(any(!var_num%in%colnames(table_entree))){ #NEW
    stop(paste0("var_num doit être un vecteur de colonne(s) de type numérique de table_entree."))
  }
  inter <- intersect(annees_possibles,annees)

  if(annees[1]<=annees[length(annees)]){
    inter <- inter[order(inter)]
  } else{
    inter <- rev(inter[order(inter)])
  }
  annees <- unique(c(annees[1]:inter[1],inter,inter[length(inter)]:annees[length(annees)]))


  for (i in 1:(length(annees)-1)){

    if(donnees_insee & length(annees)!=1){
      assign(paste0("PASSAGE_",annees[i],"_",annees[i+1]),get(paste0("PASSAGE_",annees[i],"_",annees[i+1],"_insee")))
    }

    if(length(annees)==1 || nrow(get(paste0("PASSAGE_",annees[i],"_",annees[i+1])))==0){
      table_finale <- table_entree
    } else {
      provisoire <- merge(table_entree,get(paste0("PASSAGE_",annees[i],"_",annees[i+1])),by.x=codgeo_entree,by.y=paste0("cod",annees[i]),all.x=T,all.y=F)
      provisoire[which(is.na(with(provisoire,get(paste0("cod",annees[i+1]))))),"ratio"] <- 1
      provisoire[which(is.na(with(provisoire,get(paste0("cod",annees[i+1]))))),paste0("cod",annees[i+1])] <- as.character(provisoire[which(is.na(with(provisoire,get(paste0("cod",annees[i+1]))))),codgeo_entree])
      provisoire[,c(var_num)] <- (provisoire[,c(var_num,"ratio")] * provisoire[,"ratio"])[,-(length(var_num)+1)]
      provisoire <- provisoire[,-which(colnames(provisoire)==codgeo_entree)]
      provisoire  <- provisoire[,-((ncol(provisoire)-2):ncol(provisoire))]
      names(provisoire )[which(names(provisoire)==paste0("cod",annees[i+1]))]<- codgeo_entree
      table_finale <- provisoire[,colnames(table_entree)]
    }
    table_entree <- table_finale
  }

  if(agregation){
    table_finale <- aggregate(table_finale[,c(var_num)],by =list(with(table_finale,get(codgeo_entree))),FUN=sum)
    colnames(table_finale)<- c(codgeo_entree,var_num)
  }

  if(libgeo){
    if(donnees_insee){
      assign(paste0("COG",annees[length(annees)]),get(paste0("COG",annees[length(annees)],"_insee")))
    }
    table_finale <- merge(table_finale,get(paste0("COG",annees[length(annees)]))[,c(1,2)],by.x=codgeo_entree,by.y="CODGEO",all.x=T,all.y=F)
    table_finale <- table_finale[,c(1,ncol(table_finale),2:(ncol(table_finale)-1))]
    colnames(table_finale)[2]<-"nom_commune"
  }

  table_finale <- table_finale[order(table_finale[,codgeo_entree]),]

  return(table_finale)

}


