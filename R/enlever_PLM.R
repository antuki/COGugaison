#' @title Titre
#' @name enlever_PLM
#' @description Remplir
#' @return Remplir
#' @export


enlever_PLM <- function (table_entree, codgeo_entree = colnames(table_entree)[1],libgeo=NULL,clef_commune_unique=T,vecteur_entree=F) {

  if(vecteur_entree==T){
    table_entree[substr(table_entree,1,2)=="75"] <- "75056"
    table_entree[substr(table_entree,1,3)=="132"] <- "13055"
    table_entree[substr(table_entree,1,4)=="6938"] <- "69123"
    table_sortie <- unique(table_entree)
  } else{

  table_entree[substr(with(table_entree,get(codgeo_entree)),1,2)=="75", codgeo_entree] <- "75056"
  table_entree[substr(with(table_entree,get(codgeo_entree)),1,3)=="132", codgeo_entree] <- "13055"
  table_entree[substr(with(table_entree,get(codgeo_entree)),1,4)=="6938", codgeo_entree] <- "69123"

if(clef_commune_unique==T){
  table_sortie <- table_entree
} else{

  table_sortie <- aggregate(table_entree[,sapply(table_entree, function(x){is.numeric(x) | is.integer(x)})],by =list(with(table_entree,get(codgeo_entree))),FUN=sum)
  colnames(table_sortie)<- colnames(table_entree[,c(codgeo_entree,names(sapply(table_entree, function(x){is.numeric(x) | is.integer(x)})[sapply(table_entree, function(x){is.numeric(x) | is.integer(x)})==T]))])
  table_sortie <- merge(table_sortie,table_entree[!duplicated(with(table_entree,get(codgeo_entree))),names(sapply(table_entree, function(x){is.numeric(x) | is.integer(x)})[sapply(table_entree, function(x){is.numeric(x) | is.integer(x)})==F])], by=codgeo_entree,all.x=T,all.y=F)
}

if(!is.null(libgeo)){
  table_sortie[with(table_sortie,get(codgeo_entree))=="75056", libgeo] <- "Paris"
  table_sortie[with(table_sortie,get(codgeo_entree))=="13055", libgeo] <- "Lyon"
  table_sortie[with(table_sortie,get(codgeo_entree))=="69123", libgeo] <- "Marseille"
}

}

return(table_sortie)
}
