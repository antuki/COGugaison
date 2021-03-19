#' @title Modifier les codes Insee des communes corses
#' @name modification_Corse
#' @description Modifier les codes Insee des communes corses. En effet, en 1976, les codes de toutes les communes corses sont modifiés : alors qu'ils commençaient tous par "20", leurs deux premiers caractères sont remplacés par "2A" (Corse du Sud) ou "2B" (Corse du Nord). Les codes postaux conservent d'ailleurs aujourd'hui le préfixe historique "20". Cette fonction est notamment utile pour la raison suivante :  dans certaines tables issues des recensements Insee de 1968 ou 1975, les communes sont déjà codées avec les préfixes 2A et 2B.
#' @param table_entree correspond à la table (ou le vecteur, cf. paramètre vecteur_entree) à transformer de manière à modifier les codes des communes corses.
#' @param codgeo_entree est une chaîne de caractères qui indique le nom de la variable contenant les codes communes Insee. Par défaut, il s'agit du nom de la première colonne de table_entree.
#' @param vecteur_entree vaut TRUE si table_entree est un simple vecteur.
#' @param sens vaut "20vers2A2B" pour changer tous les codes communes corses commençant par 20 par 2A ou 2B et vaut "2A2Bvers20" quand l'effet inverse est recherché.
#' @details
#' Le code officiel géographique le plus récent du package est actuellement celui au 01/01/2021. \cr
#'
#' Les millésimes des COG qui peuvent être utilisés sont à ce stade les suivants : 1968, 1975, 1982, 1990, 1999, 2008 à 2021. \cr
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
#' @seealso \link{changement_COG_varNum}, \link{changement_COG_typo},\link{changement_COG_typo_details}, \link{COG_akinator}, \link{enlever_PLM}, \link{modifications_communales},\link{nivsupra},\link{apparier_COG},\link{modification_Oudon},\link{trajectoire_commune}
#' @export
#' @examples
#' ## Exemple 1
#' # Ici, nous allons remplacer les codes communes corses commençant par 2A ou 2B par 20 (l'exemple actuel ne contient pas de commune Corse donc le code n'a pas d'impact).
#' exemple_flux_sansCorse <- modification_Corse(table_entree=exemple_flux,sens="2A2Bvers20")

modification_Corse <- function(table_entree,codgeo_entree = colnames(table_entree)[1],vecteur_entree=is.vector(table_entree),sens=c("20vers2A2B","2A2Bvers20")){

  sens = match.arg(sens) #NEW
  if(!vecteur_entree && !codgeo_entree%in%colnames(table_entree)){ #NEW
    stop(paste0("codgeo_entree doit être une colonne de table_entree."))
  }

  table_sortie <- table_entree

  if(vecteur_entree==T){
    vecteur <- table_sortie
  } else{
    vecteur <- table_sortie[,codgeo_entree]
  }

  if(sens=="2A2Bvers20"){
    substr(vecteur[which(substr(vecteur,1,2)%in%c("2A","2B"))],1,2)<-"20"
  }
  if(sens=="20vers2A2B"){
    substr(vecteur[which(vecteur%in%c("20001","20004","20006","20008","20011","20014","20017","20018","20019","20021","20022","20024","20026","20027","20028","20031","20032","20035","20038","20040","20041","20048","20056","20060","20061","20062","20064","20065","20066","20070","20071","20085","20089","20090","20091","20092","20094","20098","20099","20100","20103","20104","20108","20114","20115","20117","20118","20119","20127","20128","20129","20130","20131","20132","20133","20139","20141","20142","20144","20146","20154","20158","20160","20163","20174","20181","20186","20189","20191","20196","20197","20198","20200","20203","20204","20209","20211","20212","20215","20228","20232","20240","20247","20249","20253","20254","20258","20259","20262","20266","20268","20269","20270","20271","20272","20276","20278","20279","20282","20284","20285","20288","20295","20300","20308","20310","20312","20322","20323","20324","20325","20326","20330","20331","20336","20345","20348","20349","20351","20357","20358","20359","20360","20362","20363"))],1,2)<-"2A"
    substr(vecteur[which(vecteur%in%c("20002","20003","20005","20007","20009","20010","20012","20013","20015","20016","20020","20023","20025","20029","20030","20033","20034","20036","20037","20039","20042","20043","20044","20045","20046","20047","20049","20050","20051","20052","20053","20054","20055","20057","20058","20059","20063","20067","20068","20069","20072","20073","20074","20075","20076","20077","20078","20079","20080","20081","20082","20083","20084","20086","20087","20088","20093","20095","20096","20097","20101","20102","20105","20106","20107","20109","20110","20111","20112","20113","20116","20120","20121","20122","20123","20124","20125","20126","20134","20135","20136","20137","20138","20140","20143","20145","20147","20148","20149","20150","20151","20152","20153","20155","20156","20157","20159","20161","20162","20164","20165","20166","20167","20168","20169","20170","20171","20172","20173","20175","20176","20177","20178","20179","20180","20182","20183","20184","20185","20187","20188","20190","20192","20193","20194","20195","20199","20201","20202","20205","20206","20207","20208","20210","20213","20214","20216","20217","20218","20219","20220","20221","20222","20223","20224","20225","20226","20227","20229","20230","20231","20233","20234","20235","20236","20238","20239","20241","20242","20243","20244","20245","20246","20248","20250","20251","20252","20255","20256","20257","20260","20261","20263","20264","20265","20267","20273","20274","20275","20277","20280","20281","20283","20286","20287","20289","20290","20291","20292","20293","20296","20297","20298","20299","20301","20302","20303","20304","20305","20306","20307","20309","20311","20313","20314","20315","20316","20317","20318","20319","20320","20321","20327","20328","20329","20332","20333","20334","20335","20337","20338","20339","20340","20341","20342","20343","20344","20346","20347","20350","20352","20353","20354","20355","20356","20361","20364","20365","20366"))],1,2)<-"20"
  }

  if(vecteur_entree==T){
    table_sortie <- vecteur
  } else{
    table_sortie[,codgeo_entree] <- vecteur
  }

  return(table_sortie)
}

 # setwd("C:/Users/Kim Antunez/Desktop/PACKAGE_R/donnees/COG et tables de passage")
 #
 # COG1968 <- read.csv(paste0("COG",1968,"_insee_Corse2A2B.csv"),sep=";",stringsAsFactors = F,colClasses = c("character","character","numeric"))
 # COG1968_20 <- modification_Corse(table_entree=COG1968[,1],sens="2A2Bvers20")
 # write.table(COG1968_20,"COG1968_20.csv",sep=";",dec=".",row.names=F)
 #
 # COG1975 <- read.csv(paste0("COG",1975,"_insee_Corse2A2B.csv"),sep=";",stringsAsFactors = F,colClasses = c("character","character","numeric"))
 # COG1975_20 <- modification_Corse(table_entree=COG1975,sens="2A2Bvers20")
 # write.table(COG1975_20,"COG1975_20.csv",sep=";",dec=".",row.names=F)

# test <- PASSAGE_1975_1982_insee[-which(substr(PASSAGE_1975_1982_insee$cod1975,1,2)%in%c("2A","2B")),]
# test2 <- table_passage[which(substr(table_passage$cod1975,1,2)=="20"),]
# test2 <- test2[order(test2$cod1982),]
# test <- rbind.data.frame(test2,test)
#
# write.table(test,"PASSAGE_1975_1982_insee_new.csv",sep=";",dec=".",row.names=F)
#
#
# test <- PASSAGE_1968_1975_insee
# test <- modification_Corse(table_entree=test,codgeo_entree = "cod1968",sens="2A2Bvers20")
# test <- modification_Corse(table_entree=test,codgeo_entree = "cod1975",sens="2A2Bvers20")
#
# write.table(test,"PASSAGE_1968_1975_insee_new.csv",sep=";",dec=".",row.names=F)

