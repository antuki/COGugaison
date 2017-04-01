#' @title Titre
#' @name agregation_nivsupra
#' @description Remplir
#' @return Remplir
#' @export

agregation_nivsupra <- function(table_entree,code_com=colnames(table_entree)[1],var_num=colnames(table_entree[ ,sapply(table_entree, is.numeric) ]),nivsupra,na.nivgeo.rm=F){

  table_entree <- merge(table_entree,table_supracom[,c("CODGEO",nivsupra)],by.x=code_com,by.y="CODGEO",all.x=T,all.y=F)

  table_sortie <- aggregate(table_entree[,c(var_num)],by=list(table_entree[,nivsupra]),FUN=sum)
  colnames(table_sortie)[1] <- nivsupra
  table_sortie <- merge(table_sortie,libelles_supracom[which(libelles_supracom$NIVGEO==nivsupra),c(2,3)],by.x=nivsupra,by.y="CODGEO",all.x=T,all.y=F)
  table_sortie <- table_sortie[,c(nivsupra,"LIBGEO",var_num),]

  if(na.nivgeo.rm==F & sum(colSums(table_entree[is.na(table_entree[,nivsupra]),var_num]))!=0 ){
    vect_NA <- c(NA,NA,colSums(table_entree[is.na(table_entree[,nivsupra]),var_num]))
    names(vect_NA)[1:2] <- c(nivsupra,"LIBGEO")
    table_sortie<- rbind.data.frame(table_sortie,vect_NA)
  }

  return(table_sortie)
}
