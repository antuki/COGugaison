#' @title Titre
#' @name changement_COG_varNum
#' @description Remplir
#' @return Remplir
#' @export

changement_COG_varNum <- function(table_entree,annees,codgeo_entree=colnames(table_entree)[1],var_num=colnames(table_entree)[-1],clef_commune_unique=T,ajout_libelle_com=F){


  for (i in 1:(length(annees)-1)){

     provisoire <- merge(table_entree,get(paste0("PASSAGE_",annees[i],"_",annees[i+1])),by.x=codgeo_entree,by.y=paste0("cod",annees[i]),all.x=T,all.y=F)

    #On laisse telles quelles les lignes non connues de notre table de passage (97 hors DOM, pays étrangers...)
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
      table_finale <- merge(table_finale,get(paste0("COG",annees[length(annees)]))[,c(1,2)],by.x=codgeo_entree,by.y="CODGEO",all.x=T,all.y=F)
    }
  }

  return(table_finale)


}


