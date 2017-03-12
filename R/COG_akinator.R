#' @title Titre
#' @name COG_akinator
#' @description Remplir
#' @return Remplir
#' @export

COG_akinator<- function(vecteur_codgeo,donnees_insee=T, liste_complete=T){


vecteur_codgeo <- vecteur_codgeo[-which(substr(vecteur_codgeo,1,2)%in%c("97","98","99"))]

if(donnees_insee==T){
  assign("PASSAGE_2013_2014",get("PASSAGE_2013_2014_insee"))
  assign("PASSAGE_2014_2015",get("PASSAGE_2014_2015_insee"))
  assign("COG2014",get("COG2014_insee"))
}

COG_possibles_partiel <- NULL
COG_possibles_complet <- NULL

for(annee in c(2008:2016)){
  vecteur_codgeo_COG <- get(paste0("COG",annee))[,1]
  vecteur_codgeo_COG <- vecteur_codgeo_COG[-which(substr(vecteur_codgeo_COG,1,2)==97)]


  if(length(setdiff(vecteur_codgeo,vecteur_codgeo_COG))==0){
    COG_possibles_partiel <- c(COG_possibles_partiel,paste0("COG",annee))
  }

  if(liste_complete==T){
    if(length(vecteur_codgeo)==length(vecteur_codgeo_COG)){
      COG_possibles_complet <- c(COG_possibles_complet,paste0("COG",annee))
    }
  }else{
      COG_possibles_complet <- c(paste0("COG",2008:2016))
  }


}

COG_possibles <- intersect(COG_possibles_complet,COG_possibles_partiel)

return(COG_possibles)

}


#COG_akinator(vecteur_codgeo = vect <- unique(PASSAGE_2015_2016[,2]),donnees_insee=F, liste_complete=F)
