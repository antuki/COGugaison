#' @title Titre
#' @name changement_COG_typo
#' @description Remplir
#' @return Remplir
#' @export

changement_COG_typo <- function(table_entree,annees,codgeo_entree=colnames(table_entree)[1],var_car=colnames(table_entree)[-which(colnames(table_entree)==codgeo_entree)], methode_fusion="methode_difference",mot_different="différents",donnees_insee=T,libgeo=NULL){

  if(!is.null(libgeo) & libgeo%in%var_car){
    var_car <- var_car[-which(var_car==libgeo)]
  }

  for (i in 1:(length(annees)-1)){

    #tables de passage spéciales Insee
    if(donnees_insee==T & (annees[i]==2013 | annees[i]==2014)){
      assign(paste0("PASSAGE_",annees[i],"_",annees[i+1]),get(paste0("PASSAGE_",annees[i],"_",annees[i+1],"_insee")))
    }

    provisoire <- merge(get(paste0("PASSAGE_",annees[i],"_",annees[i+1])), table_entree, by.x=paste0("cod",annees[i]),by.y=codgeo_entree, all.x=T, all.y=F)
    #suppr : variables <- colnames(provisoire)[6:length(colnames(provisoire))]

    for (var in var_car){

      provisoire_court <- provisoire[,c(paste0("cod",annees[i]),paste0("cod",annees[i+1]),"annee","typemodif","ratio",var)]
      table_n_d <- provisoire_court[(provisoire_court$typemodif=="n") | (provisoire_court$typemodif=="d")| (provisoire_court$typemodif=="c"),]
      table_f <- provisoire_court[(provisoire_court$typemodif=="f"),]

      if(nrow(table_f)!=0){

        table_f_liste <- lapply(unique(with(table_f,get(paste0("cod",annees[i+1])))),function(x){table_f[which(with(table_f,get(paste0("cod",annees[i+1])))==x),]})
        table_f_sanspb <-table_f_liste[which(lapply(table_f_liste, FUN=function(x){ifelse(length(unique(x[,6]))==1,T,F)})==T)]
        table_f_sanspb <- lapply(table_f_sanspb, FUN=function(x){x[1,]})
        table_f_sanspb  <- do.call("rbind", table_f_sanspb)
        table_f_avecpb <-table_f_liste[which(lapply(table_f_liste, FUN=function(x){ifelse(length(unique(x[,6]))==1,T,F)})==F)]


        if (methode_fusion=="methode_difference"){
          table_f_avecpb <- lapply(table_f_avecpb, FUN=function(x){x[1,]})
          table_f_avecpb <- do.call("rbind", table_f_avecpb)

          if(!is.null(table_f_avecpb)){
            table_f_avecpb[,6] <- mot_different
          }

        }

        if (methode_fusion=="max_population"){
          #COG spécial Insee
          if(donnees_insee==T & (annees[i]==2014)){
            assign(paste0("COG",annees[i]),get(paste0("COG",annees[i],"_insee")))
          }
          table_f_avecpb <- lapply(table_f_avecpb, FUN=function(x){merge(x,get(paste0("COG",annees[i]))[,-2],by.x=paste0("cod",annees[i]),by.y="CODGEO",all.x=T,all.y=F)})
          table_f_avecpb  <- lapply(table_f_avecpb, FUN=function(x){x[which(x[,6]==(aggregate(POP ~ get(colnames(x)[6]),data =x, FUN=sum)[which.max(aggregate(POP ~ get(colnames(x)[6]),data = x, FUN=sum)$POP),1]))[1],-7]})
          table_f_avecpb <- do.call("rbind", table_f_avecpb)
        }

        table_finale_provisoire <- rbind(table_n_d,table_f_sanspb,table_f_avecpb)[,c(2,6)]
        names(table_finale_provisoire)[names(table_finale_provisoire)==paste0("cod",annees[i+1])] <- codgeo_entree


      } else{
        table_finale_provisoire <- table_n_d[,c(2,6)]
        names(table_finale_provisoire)[names(table_finale_provisoire)==paste0("cod",annees[i+1])] <- codgeo_entree
      }

      if(var==variables[1]){
        table_finale <- table_finale_provisoire
      } else{
        table_finale <- merge(table_finale,table_finale_provisoire,by=codgeo_entree,all.x=T,all.y=F)
      }
    }

    table_entree <- table_finale

  }

  if(!is.null(libgeo)){
    if(donnees_insee==T & (annees[length(annees)]==2014)){
      assign(paste0("COG",annees[length(annees)]),get(paste0("COG",annees[length(annees)],"_insee")))
    }
    table_finale <- merge(table_finale,get(paste0("COG",annees[length(annees)]))[,c(1,2)],by.x=codgeo_entree,by.y="CODGEO",all.x=T,all.y=F)
    colnames(table_finale)[ncol(table_finale)]<- libgeo
    table_finale <- table_finale[,c(1,ncol(table_finale),2:(ncol(table_finale)-1))]
  }

  return(table_finale)

}