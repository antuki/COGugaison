rm(list=ls())

setwd("maj2025")
library(COGugaison)

annee1 <- 2024
annee2 <- annee1+1

# ATTENTION A L'EMBROUILLE Tous les -1 qui suivent sont de la triche et servent à contourner l'erreur suivante
#Error in changement_COG_varNum(table_entree = get(paste0("COG", annee1)),  : annees ne doit contenir que des années comprises entre 1968 et 2019

assign(paste0("COG",annee1-1),read.csv(paste0("COG",annee1,".csv"), sep=";", dec=".", stringsAsFactors = F, colClasses = c("character","character","numeric")))

assign(paste0("PASSAGE_",annee1-1,"_",annee2-1),read.csv(paste0("PASSAGE_",annee1,"_",annee2,".csv"), sep=";", dec=".", stringsAsFactors = F, colClasses = c("character","character","character","character","numeric"),col.names=c(paste0("cod",annee2-1),paste0("cod",annee1-1),"annee","typemodif","ratio")))


assign(paste0("COG",annee2-1),changement_COG_varNum(table_entree=get(paste0("COG",annee1-1)),codgeo_entree = "CODGEO",annees = c(annee1-1,annee2-1),libgeo=FALSE, donnees_insee = FALSE))

assign(paste0("COG",annee2-1,"_old"),read.csv(paste0("COG",annee2,".csv"), sep=";", dec=".", stringsAsFactors = F, colClasses = c("character","character","numeric")))

#### Verification
# Changer à la main les mill de COG
verification <- merge(COG2024,COG2024_old,by="CODGEO",all=TRUE) ## Normalement le merge se fait bien

#### Export

export <- merge(COG2024_old, COG2024,by="CODGEO",all.x=TRUE)[-3]
colnames(export) <- c("CODGEO","LIBGEO","POP")
head(export)
write.table(export,paste0("COG",annee2,"_new.csv"), sep=";", dec=".",row.names=F)

