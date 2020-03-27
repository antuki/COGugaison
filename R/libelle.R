#' @title Renvoyer le libellé à partir d'un code géographique
#' @name libelle
#' @description Renvoie le libellé d'une entité géographique à partir de son code. Il est nécessaire de préciser le niveau géographique du code : commune, département, région, etc...
#' @param niveau Niveau de code géographique: `COG` pour la commune et les valeurs définies dans la table `libelles_supracom` pour les niveaux supra.
#' @param code Code de l'entité géographique.
#' @details
#' Le code est vectorisé, il est donc possible de l'utiliser dans un data.frame pour créer une nouvelle colonne.\cr
#' Les données de 2020, les plus récentes, sont utilisées.
#' @export
#' @examples
#' # Renvoie les noms de département de l'Ain et l'Aisne
#' libelle("DEP", c("01", "02"))
#' # Renvoie les nom de commune de L'Abergement-Clémenciat et L'Abergement-de-Varey
#' libelle("COG", c("01001", "01002"))

libelle <- function(niveau, code) {

  dplyr::tibble(
    CODGEO = code
  ) %>%
    dplyr::left_join(
      dplyr::bind_rows(
        dplyr::mutate(COG2020, NIVGEO = "COG"),
        libelles_supracom_2020
      ) %>%
        dplyr::filter(NIVGEO == !!niveau),
      by = "CODGEO") %>%
    dplyr::pull(LIBGEO)

}
