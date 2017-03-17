#' @title Titre
#' @name modifications_communales
#' @description Remplir
#' @return Remplir
#' @export
#'


#### attention fusion 14624 > 14697 pris en compte entre 1990 et 1999 alors qu'il aurait dû entre 1982 et 1990 (marche si on change le début en : )
### attention fusion 08221 > 08169 pris en compte entre 1975 et 1982 alors qu'il aurait dû entre 1982 et 1990 (marche si on change le début en : 02-03-1982)
### attention changement de département de 1972 a été tenue compte par l'Insee dès 1968 pour la Corse

### new : 200 220 défusion
# 01/03/1968
# 20/02/1975
# 04/03/1982
# 05/03/1990
# 08/03/1999


modifications_communales <- function(date_debut,date_fin){

donnees <- historiq
donnees <- donnees[which(donnees$EFF > as.Date(date_debut,"%d-%m-%Y") & donnees$EFF <= as.Date(date_fin,"%d-%m-%Y")),]
donnees <- donnees[order(donnees$EFF),]

#### partie fusions
donnees_fusions <- donnees[which(donnees$MOD%in%c("310","320","311","321","330","340","331","341","333","332","110","111","120")),]
phrase_fusions <- NULL
for(codgeo in unique(donnees_fusions[which(donnees_fusions$MOD%in%c("320","321","340","341")),c("DEPCOM")])){
  phrase_fusions <- paste0(phrase_fusions,
                           unique(donnees_fusions[which(donnees_fusions$MOD%in%c("320","321","340","341") & donnees_fusions$DEPCOM==codgeo),c("EFF")])," : ",
                           paste0(unique(donnees_fusions[which(donnees_fusions$MOD%in%c("320","321","340","341") & donnees_fusions$DEPCOM==codgeo),"NCCOFF"])," (",codgeo, ") est un rassemblement de "),
                           paste(c(
                             if(length(donnees_fusions[which((donnees_fusions$MOD=="340") & donnees_fusions$DEPCOM==codgeo) ,"NCCOFF"])!=0){paste0(unique(donnees_fusions[which((donnees_fusions$MOD=="340") & donnees_fusions$DEPCOM==codgeo) ,"NCCOFF"])," (",unique(donnees_fusions[which((donnees_fusions$MOD=="340") & donnees_fusions$DEPCOM==codgeo) ,"DEPCOM"]),")",collapse=", ")},
                             paste0(donnees_fusions[which(donnees_fusions$MOD%in%c("310","311","330","331","332","333") & donnees_fusions$COMECH==codgeo) ,"NCCOFF"]," (",donnees_fusions[which(donnees_fusions$MOD%in%c("310","311","330","331","332","333") & donnees_fusions$COMECH==codgeo),"DEPCOM"],")"),
                             if(length(donnees_fusions[which(donnees_fusions$MOD%in%c("110") & donnees_fusions$DEPCOM==codgeo) ,"NCCANC"])!=0){paste0(unique(donnees_fusions[which(donnees_fusions$MOD%in%c("110") & donnees_fusions$DEPCOM==codgeo) ,"NCCANC"])," (",unique(donnees_fusions[which(donnees_fusions$MOD%in%c("110") & donnees_fusions$DEPCOM==codgeo) ,"DEPCOM"]),")")}
                           )
                           ,collapse=", "),
                           #paste(paste0(donnees_fusions[which((donnees_fusions$MOD=="110" | donnees_fusions$MOD=="111"| donnees_fusions$MOD=="120") & donnees_fusions$DEPCOM==codgeo) ,"NCCANC"]," (",codgeo,")",collapse=", "))),collapse=", "),
                           switch(unique(as.character(donnees_fusions[which((donnees_fusions$MOD=="320" | donnees_fusions$MOD=="321" | donnees_fusions$MOD=="340"| donnees_fusions$MOD=="341") & donnees_fusions$DEPCOM==codgeo),c("MOD")])),
                                  "320"={" [fusion simple]"},
                                  "321"={" [commune nouvelle sans commune(s) déléguée(s)]"},
                                  "340"={" [fusion-association]"},
                                  "341"={" [commune nouvelle avec commune(s) déléguée(s)]"}
                           ),
                           ".\n"
  )
}

##### partie défusions
donnees_defusions <- donnees[which(donnees$MOD%in%c("210","230")),]
phrase_defusions <- NULL
for(codgeo in unique(donnees_defusions[which(donnees_defusions$MOD=="230"),c("DEPCOM")])){

  phrase_defusions <- paste0(phrase_defusions,
                             unique(donnees_defusions[which(donnees_defusions$MOD=="230" & donnees_defusions$DEPCOM==codgeo ),c("EFF")])," : ",
                             paste0(unique(donnees_defusions[which((donnees_defusions$MOD=="230") & donnees_defusions$DEPCOM==codgeo),"NCCOFF"])," (",codgeo, ") s'est séparée en "),
                             paste(c(paste0(unique(donnees_defusions[which(donnees_defusions$MOD=="230" & donnees_defusions$DEPCOM==codgeo) ,"NCCOFF"])," (",codgeo,")"),paste0(unique(donnees_defusions[which(donnees_defusions$MOD=="210" & donnees_defusions$COMECH==codgeo) ,"NCCOFF"])," (",donnees_defusions[which(donnees_defusions$MOD=="210" & donnees_defusions$COMECH==codgeo) ,"DEPCOM"],")")),collapse = ", "),
                             switch(unique(as.character(donnees_defusions[which((donnees_defusions$MOD=="230") & donnees_defusions$DEPCOM==codgeo),c("MOD")])),
                                    "230"={" [rétablissement]"}
                             ),
                             ".\n"
  )
}

#rajouter les exceptions Insee...
if(as.Date("2014-01-01") > as.Date(date_debut,"%d-%m-%Y") & as.Date("2014-01-01")  <= as.Date(date_fin,"%d-%m-%Y")){
  phrase_defusions <- paste0(phrase_defusions,"\n ATTENTION, la défusion de Loisey-Culey n'a été prise en compte dans les données de l'Insee seulement au 01/01/2015.")
}

if(!(as.Date("2014-01-01") > as.Date(date_debut,"%d-%m-%Y") & as.Date("2014-01-01")  <= as.Date(date_fin,"%d-%m-%Y")) & (as.Date("2015-01-01") > as.Date(date_debut,"%d-%m-%Y") & as.Date("2015-01-01")  <= as.Date(date_fin,"%d-%m-%Y"))){
  phrase_defusions <- paste0(phrase_defusions,"\n ATTENTION, l'évènement suivant a été pris en compte dans les données de l'Insee seulement au 01/01/2015 : 2014-01-01 : Loisey (55298) s'est séparée en Loisey (55298), Culey (55138) [rétablissement]/")
}

#### partie changements de codes
donnees_changements_codes <- donnees[which(donnees$MOD%in%c("410","411")),]
phrase_changements_codes <- NULL
for(codgeo in unique(donnees_changements_codes[,c("DEPCOM")])){
  phrase_changements_codes <- paste0(
    donnees_changements_codes[which(donnees_changements_codes$DEPCOM==codgeo),"EFF"]," : ",
    "Le code commune de ",donnees_changements_codes[which(donnees_changements_codes$DEPCOM==codgeo),"NCCOFF"], " passe de ",
    donnees_changements_codes[which(donnees_changements_codes$DEPCOM==codgeo),"DEPANC"], " à ", donnees_changements_codes[which(donnees_changements_codes$DEPCOM==codgeo),"DEPCOM"],
    switch(as.character(unique(donnees_changements_codes[which(donnees_changements_codes$DEPCOM==codgeo),"MOD"])),
           "410"={" [changement de département]"},
           "411"={" [changement de département dû à la création d'une commune nouvelle]"}
    ),
    ".\n"
  )
}



liste <- list(fusions=phrase_fusions, defusions=phrase_defusions, changements_codes=phrase_changements_codes)
return(liste)

}


# modifications_2008_2009 <- modifications_communales(date_debut="01-01-2008",date_fin="01-01-2009")
# cat(modifications_2008_2009$fusions)
# cat(modifications_2008_2009$defusions)
# cat(modifications_2008_2009$changements_codes)
# do.call(cat,modifications_2008_2009)
