rm(list=ls())

creer_table_passage_inverse <- function(table_passage,annee_debut=substr(table_passage,9,12),donnees_insee=ifelse(length(grep("insee",table_passage))==1,T,F)){

  if(donnees_insee==T){
    assign(paste0("COG",annee_debut),get(paste0("COG",annee_debut,"_insee")))
  }

  table_passage <- get(table_passage)

  nom_colonnes <- colnames(table_passage)
  table_passage[which(table_passage$typemodif=="f"),"typemodif"] <- "g"
  table_passage[which(table_passage$typemodif=="d"),"typemodif"] <- "f"
  table_passage[which(table_passage$typemodif=="g"),"typemodif"] <- "d"
  table_passage[which(table_passage$typemodif=="f"),"ratio"] <- 1

  table_d <- table_passage[which(table_passage$typemodif=="d"),]
  #table_d <- lapply(unique(table_d[,2]),function(x){table_d[which(table_d[,2]==x),]}) #new 2023 bizarre sinon bug
  table_d <- lapply(unique(table_d[,1]),function(x){table_d[which(table_d[,1]==x),]})
  table_d <- lapply(table_d, FUN=function(x){merge(x,get(paste0("COG",annee_debut))[,-2],by.x=colnames(table_passage)[2],by.y="CODGEO",all.x=T,all.y=F)})
  table_d <- lapply(table_d, FUN=function(x){x[,"ratio"]<-(x[,"POP"]/sum(x[,"POP"]));x <- x[,-6];return(x);})
  table_d  <- do.call("rbind", table_d)
  table_d  <-table_d[,c(2,1,3,4,5)]

  table_passage[which(table_passage$typemodif=="d"),] <- table_d

  table_passage <- table_passage[,c(2,1,3,4,5)]
  table_passage <- table_passage[order(table_passage[,4],table_passage[,2],table_passage[,1]),]

  return(table_passage)
}

setwd("maj2025")


########### 2025_2024
annee1 <- 2022
annee2 <- annee1+1

assign(paste0("COG",annee1),read.csv(paste0("COG",annee1,".csv"), sep=";", dec=".", stringsAsFactors = F, colClasses = c("character","character","numeric")))

assign(paste0("PASSAGE_",annee1,"_",annee2),read.csv(paste0("PASSAGE_",annee1,"_",annee2,".csv"), sep=";", dec=".", stringsAsFactors = F, colClasses = c("character","character","character","character","numeric")))

assign(paste0("PASSAGE_",annee2,"_",annee1),creer_table_passage_inverse(paste0("PASSAGE_",annee1,"_",annee2),annee_debut=substr(paste0("PASSAGE_",annee1,"_",annee2),9,12)))

# Deux mêmes chiffres normalement
sum(get(paste0("PASSAGE_",annee2,"_",annee1))$ratio) #s'il y a des NA c'est peut-être qu'il manque des 0 devant les codes commune !
length(unique(with(get(paste0("PASSAGE_",annee2,"_",annee1)),get(paste0("cod",annee2)))))

# Deux mêmes chiffres normalement
sum(get(paste0("PASSAGE_",annee1,"_",annee2))$ratio)
length(unique(with(get(paste0("PASSAGE_",annee1,"_",annee2)),get(paste0("cod",annee1)))))

write.table(get(paste0("PASSAGE_",annee2,"_",annee1)),paste0("PASSAGE_",annee2,"_",annee1,"_nouv.csv"), sep=";", dec=".",row.names=F)
