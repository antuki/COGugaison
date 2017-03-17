#' @title Titre
#' @name agregation_nivsupra
#' @description Remplir
#' @return Remplir
#' @export

agregation_nivsupra <- function(table_entree,code_com=colnames(table_entree)[1],var_num=colnames(table_entree[ ,sapply(table_entree, is.numeric) ]),nivsupra){

  table_entree <- merge(table_entree,table_supracom[,c("CODGEO",nivsupra)],by.x=code_com,by.y="CODGEO",all.x=T,all.y=F)
  table_entree <- aggregate(table_entree[,c(var_num)],by=list(table_entree[,nivsupra]),FUN="sum")
  colnames(table_entree)[1] <- nivsupra
  table_entree <- merge(table_entree,libelles_supracom[which(libelles_supracom$NIVGEO==nivsupra),c(2,3)],by.x=nivsupra,by.y="CODGEO",all.x=T,all.y=F)
  table_entree <- table_entree[,c(nivsupra,"LIBGEO",var_num),]
  return(table_entree)
}

# tab <- COG2016
# tab$col2 <- tab$POP*2
# test<-  agregation_nivsupra(table_entree=tab,nivsupra="ZE2010")


