#' Données contenues dans le package
#'
#' Les données contenues dans le package sont nombreuses :
#' \tabular{cl}{
#' COGXXXX   \tab identifiant, nom et population dans la commune à l'année XXXX \cr
#' PASSAGE_XXXX_YYYY \tab tables de passages permettant de passer d’un COG à un autre (plus récent ou plus ancien) : identifiants des communes, type d'événement (fusion, défusion, changement de nom), ratio en cas de défusion, date de l'événement        \cr
#' table_supracom_XXXX      \tab tables d’appartenance des communes aux différents niveaux géographiques supra-communaux                   \cr
#' libelles_supracom_2017      \tab indique les libellés des différents niveaux supra-communaux.               \cr
#' }
#'
#' Les tables suffixées par "_insee" correspondent à des tables selon le COG pris en compte par l’Insee dans la diffusion de ses données (qui est presque identique au COG à quelques exceptions près listées dans la documentation).
#'
#' @docType data
#' @format \code{data.frame}
#' @rdname data
#' @name data
"COG2020"
#' @rdname data
#' @name data
"COG2019"
