# Auteure : Constance Lecomte, Observatoire des territoires, ANCT
#' @title Effectuer un diagnostic sur le COG présent dans une base de données
#' @name diag_COG
#' @description Effectuer un diagnostic sur le COG présent dans une base de données
#' @param table_entree correspond à la table à diagnostiquer (ajout de la colonne de diagnostic au tableau en entrée, nouveaux paramètres optionnels ign_na et id_doubl, simplification du diagnostic)
#' @param codgeo_entree est une chaîne de caractères qui indique le nom de la variable contenant les codes Insee communaux. Par défaut, il s'agit du nom de la première colonne de table_entree.
#' @param ign_na vaut TRUE si on souhaite ignorer les codes manquants. Valeur FALSE par défaut.
#' @param id_doubl vaut TRUE si on souhaite ajouter une colonne d'identification des codes en double à l'export. Valeur FALSE par défaut.
#' @details
#' Le code officiel géographique le plus récent du package est actuellement celui au 01/01/2021. \cr
#'
#' Les millésimes des COG qui peuvent être utilisés sont à ce stade les suivants : 1968, 1975, 1982, 1990, 1999, 2008 à 2021. \cr
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
#' @seealso
#' \link{COG_akinator}, \link{enlever_PLM}, \link{trajectoire_commune}
#' @export
#' @examples
#' ## Exemple 1
#' # Exemple d'une table dont le COG est au carré
#' sortie <- diag_COG(COG2010)
#' ## Exemple 2
#' # Exemple d'une table qui mix plusieurs COG
#' table_fictive <- rbind(COG2021,COG2019) %>%
#' distinct(CODGEO, .keep_all = TRUE) %>%
#'   add_row(CODGEO = c(rep("01001",5),"75101",NA,"98756","ZZZZZ"))
#' sortie <- diag_COG(table_fictive)
#' @encoding UTF-8
#' @import dplyr

diag_COG <- function(table_entree, codgeo_entree = colnames(table_entree)[1], ign_na = FALSE, id_doubl = FALSE){


  table_sortie <- table_entree

  if(!codgeo_entree%in%colnames(table_sortie)){ #NEW
    stop(paste0("codgeo_entree doit être une colonne de table_entree."))
  }


  temp <- table_sortie %>% select(!!as.name(codgeo_entree)) %>% setNames("codgeo_init")

  # ****************************************************************************
  # Détection d'erreurs de codes

  # Codes manquants
  temp.na <- temp %>%
    filter(is.na(codgeo_init))

  # Codes arrondissements
  temp.plm <- temp %>%
    filter((substr(codgeo_init, 1, 3) %in% c("751", "132") | substr(codgeo_init, 1, 4) == "6938") & nchar(codgeo_init) == 5)

  # Communes des collectivités d'outre-mer
  temp.com <- temp %>%
    filter((substr(codgeo_init, 1, 2) == "98" | substr(codgeo_init, 1, 3) %in% c("975", "977", "978", "979")) & nchar(codgeo_init) == 5)

  # ****************************************************************************
  # Diagnostic de COG
  diacog.exp <- table_sortie

  # Préparation des COG
  #list_data <- data(package = "COGugaison")$results[, "Item"]
  # list_an_COG <- list_data[substr(list_data, 1, 3) == "COG"] %>%
  #   substr(., 4, 7) %>%
  #   unique() %>%
  #   as.numeric() %>%
  #   sort(decreasing = T)
  list_an_COG <- rev(annees_possibles)

  # Algorithme de détection de COG
  i <- 1
  cog_propre <- FALSE

  while(cog_propre == FALSE){
    an <- list_an_COG[i]
    nom_COG <- paste0("COG", as.character(an))

    if(!is.na(an)){

      df_COG <- eval(parse(text = nom_COG))

      if(ign_na == F){
        df_to_test <- temp %>%
          filter(!(codgeo_init %in% temp.plm$codgeo_init) & !(codgeo_init %in% temp.com$codgeo_init))
      } else if(ign_na == T) {
        df_to_test <- temp %>%
          filter(!is.na(codgeo_init)) %>%
          filter(!(codgeo_init %in% temp.plm$codgeo_init) & !(codgeo_init %in% temp.com$codgeo_init))
      }

      nb_obs_abs <- nrow(filter(df_to_test, !(codgeo_init %in% df_COG$CODGEO)))

      if(nb_obs_abs == 0){
        cog_propre <- TRUE
        result <- nom_COG

        # Compléter le fichier d'export
        diacog.exp <- diacog.exp %>%
          mutate(diag_cog = case_when(is.na(!!as.name(codgeo_entree)) ~ "code manquant",
                                      !!as.name(codgeo_entree) %in% unique(temp.plm$codgeo_init) ~ "arrondissement municipal",
                                      !!as.name(codgeo_entree) %in% unique(temp.com$codgeo_init) ~ "collectivité d'outre-mer",
                                      !!as.name(codgeo_entree) %in% unique(df_COG$CODGEO) ~ nom_COG,
                                      TRUE ~ "code indéterminé"))
      } else {
        i <- i+1
      }
    } else{
      cog_propre <- "non identifiable"
      result <- NULL
    }
  }

  # Si COG indétectable
  if(cog_propre == "non identifiable"){
    for(code in diacog.exp[[codgeo_entree]]){
      i <- 1
      code_id <- FALSE

      while(code_id == FALSE){
        an <- list_an_COG[i]
        nom_COG <- paste0("COG", as.character(an))

        if(!is.na(an)){
          df_COG <- eval(parse(text = nom_COG))
          if(code %in% df_COG[["CODGEO"]]){
            diacog.exp[["diag_cog"]][diacog.exp[[codgeo_entree]]==code] <- nom_COG
            code_id <- TRUE
          } else{
            i <- i+1
          }
        } else{
          if(code %in% temp.com$codgeo_init & code_id == FALSE){
            diacog.exp[["diag_cog"]][diacog.exp[[codgeo_entree]]==code] <- "collectivité d'outre-mer"
            code_id <- TRUE
          }
          if(code %in% temp.plm$codgeo_init & code_id == FALSE){
            diacog.exp[["diag_cog"]][diacog.exp[[codgeo_entree]]==code] <- "arrondissement municipal"
            code_id <- TRUE
          } else if(code_id == FALSE){
            diacog.exp[["diag_cog"]][diacog.exp[[codgeo_entree]]==code] <- "code indéterminé"
            code_id <- TRUE
          }
        }
      }
    }

  }
  diacog.exp[["diag_cog"]][is.na(diacog.exp[[codgeo_entree]])] <- "code manquant"

  # Export
  if(id_doubl == F){
    diacog.exp <- diacog.exp %>%
      select(!!as.name(codgeo_entree), diag_cog, everything())
  }
  else if(id_doubl == T){
    diacog.exp <- diacog.exp %>%
      group_by(!!as.name(codgeo_entree)) %>%
      mutate("code_doubl" = case_when(n()>1 ~ "code doublonné", TRUE ~ "code unique")) %>%
      ungroup() %>%
      select(!!as.name(codgeo_entree), diag_cog, code_doubl, everything())
  }

  #assign(deparse(substitute(table_sortie)), diacog.exp,  envir = parent.frame())

  # ****************************************************************************
  # Export du diagnostic
  # Diagnostic console
  print("# ------------------------------")
  print("# DIAGNOSTIC DE COG")
  print("# ------------------------------")
  print("# Synthèse")

  recap.temp <<- diacog.exp %>%
    group_by(diag_cog) %>%
    summarise("NB_OBS" = n()) %>%
    ungroup() %>%
    arrange(desc(NB_OBS)) %>%
    rbind(data.frame("diag_cog" = "codes uniques",
                     "NB_OBS" = length(unique(diacog.exp[[codgeo_entree]])))) %>%
    setNames(c("", "Nombre d'observations"))

  # COG identifiable
  if(!is.null(result)){
    print(paste0("# ", result))
    print("# ------------------------------")
    print("# Diagnostic détaillé")
    print(paste0("# Le fichier compte ", nrow(diacog.exp), " codes communes."))
    print("# Le diagnostic de COG correspond au COG le plus récent dans lequel l'ensemble des codes communes du fichier en entrée sont présents.")

    print(knitr::kable(recap.temp,
                  format = "markdown"))

    # COG non identifiable
  } else {
    print("# COG non identifiable")

    print("# ------------------------------")
    print("# Diagnostic détaillé")
    print(paste0("# Le fichier compte ", nrow(diacog.exp), " codes communes."))
    print("# Dans la mesure où le COG n'est pas identifiable, le diagnostic de COG correspond au COG le plus récent d'apparition de chaque code commune du fichier en entrée.")

     print(knitr::kable(recap.temp,
                  format = "markdown"))

  }

  return(diacog.exp) #table_sortie

}
