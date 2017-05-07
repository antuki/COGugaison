#' @title Titre
#' @name nivsupra
#' @description Remplir
#' @return Remplir
#' @export


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
    table_sortie <- table_sortie[,c(nivsupra_nom,"LIBGEO",var_num),]


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




