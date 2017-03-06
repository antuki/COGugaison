# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' @title Convertir un toto
#' @name toto
#' @description décrire
#' @return un chiffre transformé
#' @export

toto <- function(x){return(x+1)}


#' @title Convertir une table avec des variables numériques
#' @name convertisseur_COG_numerique
#' @description décrire
#' @return une table transformée
#' @export

convertisseur_COG_numerique <- function(table_entree,annees,codgeo_entree=colnames(table_entree)[1],var_num,clef_commune_unique=T,ajout_libelle_com=T){


  for (i in 1:(length(annees)-1)){

    #assign(paste0("PASSAGE_",annees[i],"_",annees[i+1]), read.csv(paste0("N:/DST/SDOAS/BOT/Espace de travail/Géographies/Tables de passage/CSV/PASSAGE_",annees[i],"_",annees[i+1],".csv"), header = TRUE, sep = ";", dec = ",",stringsAsFactors = F ))
    assign(paste0("PASSAGE_",annees[i],"_",annees[i+1]), read.csv(paste0("C:/Users/kantunez/Desktop/TD_R/PASSAGE_",annees[i],"_",annees[i+1],".csv"), header = TRUE, sep = ";", dec = ",",stringsAsFactors = F ))

    provisoire <- merge(table_entree,get(paste0("PASSAGE_",annees[i],"_",annees[i+1])),by.x=codgeo_entree,by.y=paste0("cod",annees[i]),all.x=T,all.y=F)

    #On laisse telles quelles les lignes non connues de notre table de passage (Suisse, 97 hors DOM...)
    provisoire[which(is.na(with(provisoire,get(paste0("cod",annees[i+1]))))),"ratio"] <- 1
    provisoire[which(is.na(with(provisoire,get(paste0("cod",annees[i+1]))))),paste0("cod",annees[i+1])] <- as.character(provisoire[which(is.na(with(provisoire,get(paste0("cod",annees[i+1]))))),codgeo_entree])
    provisoire[,c(var_num)] <- (provisoire[,c(var_num,"ratio")] * provisoire[,"ratio"])[,-(length(var_num)+1)]
    provisoire <- provisoire[,-which(colnames(provisoire)==codgeo_entree)]
    provisoire  <- provisoire[,-((ncol(provisoire)-2):ncol(provisoire))]
    names(provisoire )[which(names(provisoire)==paste0("cod",annees[i+1]))]<- codgeo_entree
    table_finale <- provisoire[,colnames(table_entree)]
    table_entree <- table_finale
  }

  if(clef_commune_unique==T){
    table_finale <- aggregate(table_finale[,c(var_num)],by =list(with(table_finale,get(codgeo_entree))),FUN=sum)
    colnames(table_finale)<- c(codgeo_entree,var_num)
    if(ajout_libelle_com==T){
      COG <- read.csv(paste0("N:/DST/SDOAS/BOT/Espace de travail/Géographies/COG/CSV/COG",annees[length(annees)],".csv"),sep=";",stringsAsFactors = F)
      table_finale <- merge(table_finale,COG[,c(1,2)],by.x=codgeo_entree,by.y="CODGEO",all.x=T,all.y=F)
    }
  }

  return(table_finale)


}

##hello

#packagedown
