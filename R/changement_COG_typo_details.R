#' @title Titre
#' @name changement_COG_typo_details
#' @description Remplir
#' @return Remplir
#' @export

changement_COG_typo_details <- function(table_entree,annees,codgeo_entree=colnames(table_entree)[1],typo,methode_fusion="methode_difference",mot_difference=NULL,classe_absorbante=NULL, donnees_insee=T){

  annees <- intersect(annees, c(1968, 1975, 1982, 1990, 1999, 2008:2017))

  for (i in 1:(length(annees)-1)){

      if(annees[i] < annees[i + 1]){vecteur <- c(1968, 1975, 1982, 1990, 1999, 2013, 2014)} else{vecteur <-c(1975, 1982, 1990, 1999, 2008, 2014, 2015)}
      if(donnees_insee==T & annees[i]%in%vecteur){
      assign(paste0("PASSAGE_",annees[i],"_",annees[i+1]),get(paste0("PASSAGE_",annees[i],"_",annees[i+1],"_insee")))
      }

      provisoire <- merge(get(paste0("PASSAGE_",annees[i],"_",annees[i+1])), table_entree, by.x=paste0("cod",annees[i]),by.y=codgeo_entree, all.x=T, all.y=F)
      provisoire_court <- provisoire[,c(paste0("cod",annees[i]),paste0("cod",annees[i+1]),"annee","typemodif","ratio",typo)]

      table_f <- provisoire_court[(provisoire_court$typemodif=="f"),]
      table_f_liste <- lapply(unique(with(table_f,get(paste0("cod",annees[i+1])))),function(x){table_f[which(with(table_f,get(paste0("cod",annees[i+1])))==x),]})
      table_f_avecpb <-table_f_liste[which(lapply(table_f_liste, FUN=function(x){ifelse(length(unique(x[,6]))==1,T,F)})==F)]
      table_f_avecpb <- do.call("rbind", table_f_avecpb)

      if(!is.null(table_f_avecpb)){
      table_f_avecpb <- table_f_avecpb[order(table_f_avecpb[,paste0("cod",annees[i+1])]),c(paste0("cod",annees[i]),paste0("cod",annees[i+1]),typo)]

      if(donnees_insee==T & (annees[i]%in%c(1968,1975,1982,1990,1999,2014))){
        assign(paste0("COG",annees[i]),get(paste0("COG",annees[i],"_insee")))
      }
      table_f_avecpb <- merge(table_f_avecpb,get(paste0("COG",annees[i]))[,1:2],by.x=paste0("cod",annees[i]),by.y="CODGEO",all.x=T,all.y=F)
      colnames(table_f_avecpb)[ncol(table_f_avecpb)]<-paste0("lib",annees[i])

      if(donnees_insee==T & (annees[i+1]%in%c(1968,1975,1982,1990,1999,2014))){
        assign(paste0("COG",annees[i+1]),get(paste0("COG",annees[i+1],"_insee")))
      }
      table_f_avecpb <- merge(table_f_avecpb,get(paste0("COG",annees[i+1]))[,1:2],by.x=paste0("cod",annees[i+1]),by.y="CODGEO",all.x=T,all.y=F)
      colnames(table_f_avecpb)[ncol(table_f_avecpb)]<-paste0("lib",annees[i+1])

      table_f_avecpb <- table_f_avecpb[,c(paste0("cod",annees[i]),paste0("lib",annees[i]),paste0("cod",annees[i+1]),paste0("lib",annees[i+1]),typo)]

      }

      assign(paste0("resultats_",annees[i],"_",annees[i+1]),table_f_avecpb)
      table_entree <- changement_COG_typo(table_entree=table_entree,annees=c(annees[i]:annees[i+1]),codgeo_entree=codgeo_entree,typos=typo, methode_fusion=methode_fusion,mot_difference=mot_difference,classe_absorbante=classe_absorbante,donnees_insee=donnees_insee,libgeo=NULL)

  }

  liste <- mget(ls(pattern = "^resultats_",envir=))
  names(liste)<- gsub("^resultats_","",names(liste))
  return(liste)

}
