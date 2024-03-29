% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{data}
\alias{data}
\alias{COG2020}
\alias{COG2019}
\title{Données contenues dans le package}
\format{
\code{data.frame}

An object of class \code{data.frame} with 34970 rows and 3 columns.
}
\usage{
COG2020

COG2019
}
\description{
Les données contenues dans le package sont nombreuses :
\tabular{cl}{
COGXXXX   \tab identifiant, nom et population dans la commune à l'année XXXX \cr
PASSAGE_XXXX_YYYY \tab tables de passages permettant de passer d’un COG à un autre (plus récent ou plus ancien) : identifiants des communes, type d'événement (fusion, défusion, changement de nom), ratio en cas de défusion, date de l'événement        \cr
table_supracom_XXXX      \tab tables d’appartenance des communes aux différents niveaux géographiques supra-communaux                   \cr
libelles_supracom_2017      \tab indique les libellés des différents niveaux supra-communaux.               \cr
}
}
\details{
Les tables suffixées par "_insee" correspondent à des tables selon le COG pris en compte par l’Insee dans la diffusion de ses données (qui est presque identique au COG à quelques exceptions près listées dans la documentation).
}
\keyword{datasets}
