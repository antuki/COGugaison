#' @title Deviner la géographie communale
#' @name COG_akinator
#' @description Détecter le millésime du code officiel géographique d'une table de données communales.
#' @param vecteur_codgeo correspond à un vecteur de codes Insee communaux pour lequel on cherche à déterminer le millésime de la géographie.
#' @param donnees_insee vaut TRUE si les données manipulées sont produites par l'Insee. En effet, quelques rares modifications communales (la défusion des communes Loisey et Culey au 1er janvier 2014 par exemple) ont été prises en compte dans les bases de données communales de l'Insee plus tard que la date officielle. Pour tous les COG officiels datant d'avant 2008, seules les tables de passage Insee sont disponibles dans ce package.
#' @param liste_complete vaut TRUE si le vecteur de communes du paramètre *vecteur_codgeo* liste l'ensemble des communes françaises et FALSE s'il en liste seulement un extrait.
#' @details
#' Le code officiel géographique de référence du package est actuellement celui au 01/01/2017. Les données communales devront être dans ce COG pour être agrégées en niveaux supra-communaux (fonction nivsupra). \cr
#'
#' Les autres codes officiels géographiques utilisés dans COGugaison sont les suivants :
#' \itemize{
#' \item{COG 1968 : à partir du 01/03/1968 (donnees_insee=T obligatoirement)}
#' \item{COG 1975 : à partir du 20/02/1975 (donnees_insee=T obligatoirement)}
#' \item{COG 1982 : à partir du 04/03/1982 (donnees_insee=T obligatoirement)}
#' \item{COG 1990 : à partir du 05/03/1990 (donnees_insee=T obligatoirement)}
#' \item{COG 1999 : à partir du 08/03/1999 (donnees_insee=T obligatoirement)}
#' \item{COG 2008 à 2017 : à partir du 01/01 de chaque année (donnees_insee=T ou F)}} \cr
#'
#' Les différences entre les tables de passage Insee et non Insee sont les suivantes :\cr
#' \itemize{
#' \item{1982-03-03 (pris en compte par l'Insee seulement après le 04/03/1982): Flaignes-Havys (08169) est un rassemblement de Flaignes-Havys (08169), Havys (08221) [fusion simple].}
#' \item{2014-01-01 (pris en compte par l'Insee seulement au 01/01/2015) : Loisey (55298) s'est séparée en Loisey (55298), Culey (55138) [rétablissement].}
#' \item{1990-02-01 (pris en compte par l'Insee seulement après le 05/03/1990) : Le code commune de Oudon passe de 14624 à 14697 [changement de code dû à un changement de chef-lieu].}}
#' @references
#' \itemize{
#' \item{\href{https://www.insee.fr/fr/information/2666684#titre-bloc-11}{historique des géographies communales (Insee)}}
#' \item{\href{https://www.insee.fr/fr/information/2028028}{tables d'appartenance des communes aux différents niveaux géographiques (Insee)}}}
#' @seealso \link{changement_COG_varNum}, \link{changement_COG_typo},\link{changement_COG_typo_details}, \link{enlever_PLM}, \link{modification_Corse}, \link{modifications_communales},\link{nivsupra}
#' @export
#' @examples
#' ## Exemple 1
#' # Ici, nous cherchons le millésime du code officiel géographique (COG) utilisé dans la table de données communales exemple_popcom.
#' COG_akinator(vecteur_codgeo=exemple_popcom[,1],donnees_insee=T, liste_complete=T)

COG_akinator<- function(vecteur_codgeo,donnees_insee=T, liste_complete=T){

#enleverPLM
vecteur_codgeo <- COGugaison::enlever_PLM(vecteur_codgeo,vecteur_entree=T)
vecteur_codgeo <- unique(vecteur_codgeo[which(substr(vecteur_codgeo,1,2)%in%c(paste0("0",1:9),10:96,"2A","2B") & nchar(vecteur_codgeo)==5)])

#modificationCorse #new
vecteur_codgeo <- COGugaison::modification_Corse(vecteur_codgeo,sens="20vers2A2B")


if(donnees_insee==T){
  assign(paste0("COG",a),get(paste0("COG",a,"_insee")))
}

COG1968 <- COGugaison::modification_Corse(COG1968,sens="20vers2A2B") #new
COG1975 <- COGugaison::modification_Corse(COG1975,sens="20vers2A2B") #new


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
