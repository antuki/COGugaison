#' @title Deviner la géographie communale
#' @name COG_akinator
#' @description Détecter le millésime du code officiel géographique d'une table de données communales.
#' @param vecteur_codgeo correspond à un vecteur de codes Insee communaux pour lequel on cherche à déterminer le millésime de la géographie.
#' @param donnees_insee vaut TRUE si les données manipulées sont produites par l'Insee. En effet, quelques rares modifications communales (la défusion des communes Loisey et Culey au 1er janvier 2014 par exemple) ont été prises en compte dans les bases de données communales de l'Insee plus tard que la date officielle. Pour tous les COG officiels datant d'avant 2008, seules les tables de passage Insee sont disponibles dans ce package.
#' @details
#' Le code officiel géographique le plus récent du package est actuellement celui au 01/01/2024. \cr
#'
#' Les millésimes des COG qui peuvent être utilisés sont à ce stade les suivants : 1968, 1975, 1982, 1990, 1999, annuel à partir de 2008. \cr
#'
#' Les dates de référence des codes officiels géographiques utilisés dans COGugaison sont les suivantes :
#' \itemize{
#' \item{COG 1968 : à partir du 01/03/1968}
#' \item{COG 1975 : à partir du 20/02/1975}
#' \item{COG 1982 : à partir du 04/03/1982}
#' \item{COG 1990 : à partir du 05/03/1990}
#' \item{COG 1999 : à partir du 08/03/1999}
#' \item{Pour tous les autres COG : à partir du 01/01 de chaque année}} \cr
#'
#' Les différences entre les tables de passage Insee et non Insee sont les suivantes :\cr
#' \itemize{
#' \item{1982-03-03 (pris en compte par l'Insee seulement après le 04/03/1982): Flaignes-Havys (08169) est un rassemblement de Flaignes-Havys (08169), Havys (08221) [fusion simple].}
#' \item{2014-01-01 (pris en compte par l'Insee seulement au 01/01/2015) : Loisey (55298) s'est séparée en Loisey (55298), Culey (55138) [rétablissement].}
#' \item{1990-02-01 (pris en compte par l'Insee seulement après le 05/03/1990) : Le code commune de Oudon passe de 14624 à 14697 [changement de code dû à un changement de chef-lieu].}
#' \item{2014-01-07 (pris en compte par l'Insee  dès le 01/01/2016) : Tôtes est rattachée à Notre-Dame-de-Fresnay qui devient L'Oudon (changement de code de l'Oudon de 14697 à 14472) [transfert de chef-lieu].}
#' \item{1981-09-28 (pris en compte par l'Insee  dès le 20/02/1975) : Vaudreuil-Ex-Ensemble Urbain (27701) est créée à partir des parcelles d'Incarville (27351), de Léry (27365) , de Porte-Joie (27471) , de Poses  (27474) , de Saint-Étienne-du-Vauvray (27537), de Saint-Pierre-du-Vauvray (27598), de Tournedos-sur-Seine  (27651) et du Vaudreuil (27528) [création]. Cette situation étant complexe, nous avons pour le moment considéré que Vaudreuil-Ex-Ensemble Urbain (27701) est créée à partir de parcelles du Vaudreuil (27528) uniquement.}
#' \item{En 1968, les 4 communes qui auraient dû d'après le COG être codées 2B044,2B076,2B151 et 2A325 sont codées 20044,20076,20151 et 20325 dans les données Insee.}}
#' @references
#' \itemize{
#' \item{\href{https://www.insee.fr/fr/information/2666684#titre-bloc-11}{historique des géographies communales (Insee)}}
#' \item{\href{https://www.insee.fr/fr/information/2028028}{tables d'appartenance des communes aux différents niveaux géographiques (Insee)}}}
#' @seealso \link{changement_COG_varNum}, \link{changement_COG_typo},\link{changement_COG_typo_details}, \link{enlever_PLM}, \link{modification_Corse}, \link{modifications_communales},\link{nivsupra},\link{apparier_COG},\link{modification_Oudon}
#' @export
#' @examples
#' ## Exemple 1
#' # Ici, nous cherchons le millesime du code officiel geographique (COG) utilise dans la table de donnees communales exemple_popcom.
#' COG_akinator(vecteur_codgeo=exemple_popcom[,1], donnees_insee = TRUE)
#' @encoding UTF-8

COG_akinator <- function (vecteur_codgeo, donnees_insee = TRUE)
{
 if(!is.vector(vecteur_codgeo)){ #NEW
   stop("vecteur_codgeo doit être un vecteur")
 }
  annees <- annees_possibles
  vecteur_codgeo <- COGugaison::enlever_PLM(vecteur_codgeo,vecteur_entree = TRUE)
  vecteur_codgeo <- unique(vecteur_codgeo[which(substr(vecteur_codgeo, 1, 2) %in% c(paste0("0", 1:9), 10:96, "2A", "2B") & nchar(vecteur_codgeo) == 5)])
  vecteur_codgeo <- COGugaison::modification_Corse(vecteur_codgeo, sens = "20vers2A2B")
  vecteur_codgeo[which(vecteur_codgeo%in%c("14472"))] <-"14697" #cas l'Oudon qui est souvent présent dans les tables...

  if (donnees_insee) {
    for (a in annees) {
      assign(paste0("COG", a), get(paste0("COG", a, "_insee")))

    }
  }
  COG1968 <- COGugaison::modification_Corse(COG1968, sens = "20vers2A2B")
  COG1975 <- COGugaison::modification_Corse(COG1975, sens = "20vers2A2B")
 COG_possibles <- NULL
  for (annee in annees) {

    vecteur_codgeo_COG <- get(paste0("COG", annee))[, 1]
    vecteur_codgeo_COG <- vecteur_codgeo_COG[-which(substr(vecteur_codgeo_COG,1, 2) == 97)]
    vecteur_codgeo_COG[which(vecteur_codgeo_COG%in%c("14472"))] <-"14697" #cas l'Oudon
    if (length(setdiff(vecteur_codgeo, vecteur_codgeo_COG))==0) { # new : avant all(vecteur_codgeo %in% vecteur_codgeo_COG)
      COG_possibles <- c(COG_possibles, paste0("COG", annee))
    }
  }

  # ajustements et précisions sur les cas de fusions en cas de plusieurs années de COG possibles qui se suivent
  for (i in 1:(length(annees)-1)){
    if(all(c(paste0("COG",annees[i]),paste0("COG",annees[i+1]))%in%COG_possibles)){
      if(donnees_insee & length(annees)!=1){
        assign(paste0("PASSAGE_",annees[i],"_",annees[i+1]),get(paste0("PASSAGE_",annees[i],"_",annees[i+1],"_insee")))
      }

      fusions_disparues <- get(paste0("PASSAGE_",annees[i],"_",annees[i+1]))[which(get(paste0("PASSAGE_",annees[i],"_",annees[i+1]))[,"typemodif"]=="f" & get(paste0("PASSAGE_",annees[i],"_",annees[i+1]))[,paste0("cod",annees[i])]!=get(paste0("PASSAGE_",annees[i],"_",annees[i+1]))[,paste0("cod",annees[i+1])]),paste0("cod",annees[i])]
      if(length(intersect(fusions_disparues,vecteur_codgeo))==0){
        COG_possibles <- COG_possibles[!COG_possibles==paste0("COG",annees[i])]
      } else {
        COG_possibles <- COG_possibles[!COG_possibles==paste0("COG",annees[i+1])]

      }
    }

  }


  return(COG_possibles)
}
