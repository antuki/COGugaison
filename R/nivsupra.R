#' @title Agréger des données à des échelons supra-communaux
#' @name nivsupra
#' @description Permettre d'agréger les tables de données communales à de nombreux échelons supra-communaux administratifs (EPCI, arrondissements, cantons-villes, départements, régions) ou d'étude (bassins de vie, zones d'emploi, unités urbaines, aires urbaines). La fonction `nivsupra` peut également s'appliquer à des tables de flux (non agrégées par codes communes) grâce à l'option agregation = FALSE. La table en entrée est alors conservée comme telle avec une nouvelle colonne qui correspond au niveau supracommunal du code commune considéré.
#' @param table_entree correspond à la table (ou le vecteur, cf. paramètre vecteur_entree) à transformer de manière à modifier les codes des communes corses.
#' @param codgeo_entree est une chaîne de caractères qui indique le nom de la variable contenant les codes communes Insee. Par défaut, il s'agit du nom de la première colonne de table_entree.
#' @param COG indique l'année de COG de la table communale considérée. (exemple 2014). Années possibles : de 2008 à 2020. Par défaut, vaut 2020.
#' @param var_num est un vecteur de chaînes de caractères qui indique les noms des variables numériques à convertir. Par défaut, il s'agit de l'ensemble des variables de types numériques dans table_entree.
#' @param nivsupra est une chaîne de caractères qui indique le nom du niveau supra-communale souhaité. Pour les années récentes :
#' - "DEP" : départements
#' - "REG" : régions
#' - "EPCI" : EPCI au 01/01/20XX
#' - "ARR" : arrondissements au 01/01/20XX
#' - "CV" : cantons-villes au 01/01/20XX
#' - "ZE2010" : zones d'emploi 2010 (avant 2020)
#' - "ZE2020" : zones d'emploi 2020 (après 2020)
#' - "UU2010" : unités urbaines 2010 (avant 2020)
#' - "UU2020" : unités urbaines 2020 (après 2020)
#' - "AU2010" : aires urbaines 2010 (avant 2020)
#' - "AAV2020" : aires d'attraction des villes 2020 (après 2020)
#' - "BV2012" : bassins de vie 2012
#' Pour les COG plus anciens, regarder au cas par cas selon les années. Par exemple, str(table_supracom_2008)
#' @param nivsupra_nom indique le nom à donner au niveau supra-communal dans la table de sortie. Il faut par défaut la chaîne de caractère contenue dans nivsupra si agregation = T et la concaténation de nivsupra et codgeo_entree séparée d'un "_" si agregation = F.
#' @param agregation vaut TRUE si la table souhaitée doit sommer toutes les lignes qui concernent une même commune et FALSE si l'on souhaite volontairement conserver les doublons dans les codes commune (dans les tables de flux par exemple). Si agregation = F, les variables de type caractère sont alors conservées comme telles ou dupliquées en cas de défusion et les variables numériques sommées en cas de fusion ou réparties proportionnellement à la population de chaque commune en cas de défusion.
#' @details
#' Le code officiel géographique le plus récent du package est actuellement celui au 01/01/2024. \cr
#'
#' Les millésimes des COG qui peuvent être utilisés sont à ce stade les suivants : 1968, 1975, 1982, 1990, 1999, annuel à partir de 2008. \cr
#'
#' A partir de 2020, de nombreux zonages ont été modifiés suite à la refonte des zonages de l'Insee. Les ZE2010 deviennent les ZE2020, les UU2010 deviennent les UU2020, les AU2010 deviennent les AAV2020.
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
#' @seealso \link{changement_COG_varNum}, \link{changement_COG_typo},\link{changement_COG_typo_details}, \link{COG_akinator}, \link{enlever_PLM}, \link{modification_Corse},\link{modifications_communales},\link{apparier_COG},\link{modification_Oudon},\link{trajectoire_commune}
#' @export
#' @examples
#' ## Exemple 1
#' # Ici, apres avoir transforme les donnees en geographie communale au 01/01/2017, nous agregeons la population et la superficie des communes a l'echelon geographique des zones d'emploi afin d'obtenir une table des densites de population par zone d'emploi.
#' exemple_popcom_COG2017_num <- changement_COG_varNum(table_entree = exemple_popcom,annees=c(2014:2017), agregation = TRUE, libgeo = TRUE, donnees_insee = TRUE)
#' exemple_popcom_ZE2010 <- nivsupra(table_entree=exemple_popcom_COG2017_num,COG=2017,nivsupra="ZE2010", agregation = TRUE)
#' exemple_popcom_ZE2010$densite <- exemple_popcom_ZE2010$P12_POP / exemple_popcom_ZE2010$SUPERF
#' head(exemple_popcom_ZE2010)
#' ## Exemple 2
#' # Ici, on ajoute les colonnes ZE2010_COMMUNE et ZE2010_DCLT a la table exemple de flux domicile-travail.
#' exemple_flux_COG2017 <- changement_COG_varNum(table_entree = exemple_flux, annees=c(2014:2017), codgeo_entree="COMMUNE",agregation = FALSE, libgeo = FALSE, donnees_insee = TRUE)
#' exemple_flux_COG2017 <- changement_COG_varNum(table_entree = exemple_flux_COG2017, annees=c(2014:2017),codgeo_entree="DCLT", agregation = FALSE, libgeo = FALSE, donnees_insee = TRUE)
#' exemple_flux_COG2017_etZE <- nivsupra(table_entree = exemple_flux_COG2017, codgeo_entree="COMMUNE",nivsupra="ZE2010",COG=2017, agregation = FALSE)
#' exemple_flux_COG2017_etZE <- nivsupra(table_entree = exemple_flux_COG2017_etZE, codgeo_entree="DCLT",nivsupra="ZE2010",COG=2017, agregation = FALSE)
#' head(exemple_flux_COG2017_etZE)

nivsupra <- function(table_entree,codgeo_entree = colnames(table_entree)[1],
                     COG = annee_ref,
                     var_num = colnames(table_entree)[sapply(table_entree, is.numeric)],
                     nivsupra,
                     nivsupra_nom = ifelse(agregation == TRUE,nivsupra,paste0(nivsupra,"_",codgeo_entree)),
                     agregation = TRUE){

  if(!codgeo_entree%in%colnames(table_entree)){ #NEW
    stop(paste0("codgeo_entree doit être une colonne de table_entree."))
  }
  if(!COG%in%c(2008:annee_ref)){ #NEW
    stop(paste0("COG doit être compris entre 2008 et ",annee_ref," pour cette fonction."))
  }
  if(any(!var_num%in%colnames(table_entree))){ #NEW
    stop(paste0("var_num doit être une colonne de table_entree."))
  }
  if(!nivsupra%in%colnames(get(paste0("table_supracom_",COG)))[-c(1,2)]){ #NEW
    stop(paste0("nivsupra doit être un des niveaux géo suivants : ",paste0(colnames(get(paste0("table_supracom_",COG)))[-c(1,2)],collapse=", ")))
  }

  table_sortie <- enlever_PLM(table_entree = table_entree, codgeo_entree = codgeo_entree, agregation = FALSE) #nouveau
  table_sortie <- merge(table_sortie,get(paste0("table_supracom_",COG))[,c("CODGEO",nivsupra)],by.x=codgeo_entree,by.y="CODGEO", all.x = TRUE, all.y = FALSE)
  colnames(table_sortie)[which(colnames(table_sortie)==nivsupra)]<- nivsupra_nom

  if(agregation == FALSE){
    table_sortie <- merge(table_sortie,get(paste0("libelles_supracom_",COG))[which(get(paste0("libelles_supracom_",COG))[,"NIVGEO"]==nivsupra),c(2,3)],by.x=nivsupra_nom,by.y="CODGEO", all.x = TRUE, all.y = FALSE)
    colnames(table_sortie)[ncol(table_sortie)]<- paste0(nivsupra,"_nom_",codgeo_entree)
    table_sortie <- table_sortie[,c(colnames(table_entree),nivsupra_nom,paste0(nivsupra,"_nom_",codgeo_entree))]
  } else{
    table_sortie <- aggregate(table_sortie[,c(var_num)],by=list(table_sortie[,nivsupra_nom]),FUN=sum)
    colnames(table_sortie) <- c(nivsupra_nom,var_num)
    table_sortie <- merge(table_sortie,get(paste0("libelles_supracom_",COG))[which(get(paste0("libelles_supracom_",COG))[,"NIVGEO"]==nivsupra),c(2,3)],by.x=nivsupra_nom,by.y="CODGEO", all.x = TRUE, all.y = FALSE)
    table_sortie <- table_sortie[,c(nivsupra_nom,"LIBGEO",var_num)]
  }

  return(table_sortie)
}
