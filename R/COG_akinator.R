#' @title Titre
#' @name COG_akinator
#' @description Remplir
#' @return Remplir
#' @export




COG_akinator<- function(vecteur_codgeo,donnees_insee=T, liste_complete=T){

#enleverPLM
vecteur_codgeo <- COGugaison::enlever_PLM(vecteur_codgeo,vecteur_entree=T)
vecteur_codgeo <- unique(vecteur_codgeo[which(substr(vecteur_codgeo,1,2)%in%c(paste0("0",1:9),10:96,"2A","2B") & nchar(vecteur_codgeo)==5)])

if(donnees_insee==T){
  for (a in c(1968,1975,1982,1990,1999,2014)){
  assign(paste0("COG",a),get(paste0("COG",a,"_insee")))
  }
}

COG_possibles_partiel <- NULL
COG_possibles_complet <- NULL

for(annee in c(1968, 1975, 1982, 1990, 1999, 2008:2017)){
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
      COG_possibles_complet <- c(paste0("COG",c(1968, 1975, 1982, 1990, 1999, 2008:2017)))
  }


}

COG_possibles <- intersect(COG_possibles_complet,COG_possibles_partiel)

return(COG_possibles)

}


#COG_akinator(vecteur_codgeo = vect <- unique(PASSAGE_2015_2016[,2]),donnees_insee=F, liste_complete=F)
