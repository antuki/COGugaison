#' @title Titre
#' @name creer_table_passage
#' @description Remplir
#' @return Remplir
#' @export
#'

creer_table_passage <- function(date_debut,date_fin){

  donnees <- historiq
  donnees <- donnees[which(donnees$EFF > as.Date(date_debut,"%d-%m-%Y") & donnees$EFF <= as.Date(date_fin,"%d-%m-%Y")),]
  donnees <- donnees[order(donnees$EFF),]

  donnees_fusions1 <- donnees[which(donnees$MOD%in%c("310","311","331","333","330")),c("DEPCOM","COMECH","EFF")]
  colnames(donnees_fusions1) <- c(paste0("cod",substr(date_debut,7,11)),paste0("cod",substr(date_fin,7,11)),"annee")
  donnees_fusions2 <- donnees[which(donnees$MOD%in%c("320")),c("DEPCOM","DEPCOM","EFF")]
  donnees_fusions2 <- donnees[which(donnees$MOD%in%c("320")),c("DEPCOM","DEPCOM","EFF")]
  colnames(donnees_fusions2) <- c(paste0("cod",substr(date_debut,7,11)),paste0("cod",substr(date_fin,7,11)),"annee")
  donnees_fusions3 <- donnees[which(donnees$MOD%in%c("330") &!duplicated(donnees)),c("COMECH","COMECH","EFF")]
  colnames(donnees_fusions3) <- c(paste0("cod",substr(date_debut,7,11)),paste0("cod",substr(date_fin,7,11)),"annee")
  donnees_fusions <- rbind(donnees_fusions1,donnees_fusions2,donnees_fusions3)
  donnees_fusions <- donnees_fusions[!duplicated(donnees_fusions[,c(1,2)]),]
  if(nrow(donnees_fusions)!=0){
    donnees_fusions$typemodif <- "f"
  }

  donnees_defusions1 <- donnees[which(donnees$MOD%in%c("200","210")),c("COMECH","DEPCOM","EFF")]
  colnames(donnees_defusions1) <- c(paste0("cod",substr(date_debut,7,11)),paste0("cod",substr(date_fin,7,11)),"annee")
  donnees_defusions2<- donnees_defusions1[!duplicated(donnees_defusions1[,c(paste0("cod",substr(date_debut,7,11)))]),]
  donnees_defusions2[,paste0("cod",substr(date_fin,7,11))]<- with(donnees_defusions2,get(paste0("cod",substr(date_debut,7,11))))
  donnees_defusions <- rbind(donnees_defusions1,donnees_defusions2)
  if(nrow(donnees_defusions)!=0){
    donnees_defusions$typemodif <- "d"
  }

  donnees_changements_dep1 <- donnees[which(donnees$MOD%in%c("410","411")),c("DEPANC","DEPCOM","EFF")]
  colnames(donnees_changements_dep1) <- c(paste0("cod",substr(date_debut,7,11)),paste0("cod",substr(date_fin,7,11)),"annee")
  donnees_changements_dep2 <- donnees[which(donnees$MOD%in%c("500")),c("C_LANC","C_LOFF","EFF")]
  colnames(donnees_changements_dep2) <- c(paste0("cod",substr(date_debut,7,11)),paste0("cod",substr(date_fin,7,11)),"annee")
  donnees_changements_dep2 <- donnees_changements_dep2[!duplicated(donnees_changements_dep2),]
  donnees_changements_dep <- rbind(donnees_changements_dep1,donnees_changements_dep2)

  if(nrow(donnees_changements_dep)!=0){
    donnees_changements_dep$typemodif <- "c"
  }

  table_passage <- rbind(donnees_fusions,donnees_defusions,donnees_changements_dep )
  table_passage$annee <- as.character(table_passage$annee)
  if(nrow(table_passage)==0){table_passage$typemodif <- as.character(table_passage$typemodif)}

  COG_fin <- get(paste0("COG",substr(date_fin,7,11)))

  #Si on veut enlever les communes qui ne sont pas dans le COG de départ, faire all.x=F
  COG_debut <- get(paste0("COG",substr(date_debut,7,11)))
   table_passage <- merge(table_passage,COG_debut[,c(1,2)],by.x=paste0("cod",substr(date_debut,7,11)),by.y="CODGEO",all.x=T,all.y=T)
  table_passage<- table_passage[,-ncol(table_passage)]

  table_passage[which(is.na(table_passage$typemodif)),paste0("cod",substr(date_fin,7,11))] <- table_passage[which(is.na(table_passage$typemodif)),paste0("cod",substr(date_debut,7,11))]
  table_passage[which(is.na(table_passage$typemodif)),"typemodif"] <- "n"
  table_passage <- merge(table_passage,COG_fin[,c(1,3)],by.x=paste0("cod",substr(date_fin,7,11)),by.y="CODGEO",all.x=T,all.y=F)
  table_passage$POP <- apply(table_passage,1,function(x){as.numeric(x["POP"])/sum(table_passage$POP[which(table_passage[,paste0("cod",substr(date_debut,7,11))]==x[paste0("cod",substr(date_debut,7,11))])])})
  table_passage$POP[which(table_passage$typemodif%in%c("f","n","c"))] <- 1
  colnames(table_passage)[ncol(table_passage)] <- "ratio"
  table_passage <- table_passage[order(table_passage$typemodif,table_passage[,paste0("cod",substr(date_fin,7,11))],table_passage[,paste0("cod",substr(date_debut,7,11))]),c(paste0("cod",substr(date_debut,7,11)),paste0("cod",substr(date_fin,7,11)),"annee","typemodif","ratio")]

}
