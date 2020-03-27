#' @title Renvoyer le libellé à partir d'un code géographique
#' @name libelle
#' @description Renvoie le libellé d'une entité géographique à partir de son code. Il est nécessaire de préciser le niveau géographique du code : commune, département, région, etc...
#' @param niveau Niveau de code géographie: `COG` pour la commune et les valeurs définie dans la table `libelles_supracom` pour les niveau supra.
#' @param code Code de l'entité géographique.
#' @details
#' Le code est vectorisé, il est donc possible de l'utiliser dans un data.frame pour créer une nouvelle colonne.\cr
#' Les données de 2020, les plus récentes, sont utilisées.
#' @export
#' @examples
#' # Renvoie de le département de l'Ain
#' libelle("DEP", "01")
#' # Renvoie le nom de commune de L'Abergement-Clémenciat
#' libelle("COG", "01001")

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
