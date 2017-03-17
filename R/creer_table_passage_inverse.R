#' @title Titre
#' @name creer_table_passage_inverse
#' @description Remplir
#' @return Remplir
#' @export
#'

creer_table_passage_inverse <- function(table_passage,annee_debut=substr(table_passage,9,12),donnees_insee=ifelse(length(grep("insee",psg))==1,T,F)){

  if(donnees_insee==T & annee_debut%in%c("1968","1975","1982","1990","1999","2014")){
    assign(paste0("COG",annee_debut),get(paste0("COG",annee_debut,"_insee")))
  }

  table_passage <- get(table_passage)

  nom_colonnes <- colnames(table_passage)
  table_passage[which(table_passage$typemodif=="f"),"typemodif"] <- "g"
  table_passage[which(table_passage$typemodif=="d"),"typemodif"] <- "f"
  table_passage[which(table_passage$typemodif=="g"),"typemodif"] <- "d"
  table_passage[which(table_passage$typemodif=="f"),"ratio"] <- 1

  table_d <- table_passage[which(table_passage$typemodif=="d"),]
  table_d <- lapply(unique(table_d[,2]),function(x){table_d[which(table_d[,2]==x),]})
  table_d <- lapply(table_d, FUN=function(x){merge(x,get(paste0("COG",annee_debut))[,-2],by.x=colnames(table_passage)[1],by.y="CODGEO",all.x=T,all.y=F)})
  table_d <- lapply(table_d, FUN=function(x){x[,"ratio"]<-(x[,"POP"]/sum(x[,"POP"]));x <- x[,-6];return(x);})
  table_d  <- do.call("rbind", table_d)

  table_passage[which(table_passage$typemodif=="d"),] <- table_d

  table_passage <- table_passage[,c(2,1,3,4,5)]
  table_passage <- table_passage[order(table_passage[,4],table_passage[,2],table_passage[,1]),]

  return(table_passage)
}
