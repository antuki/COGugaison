#' @title Titre
#' @name modifications_communales
#' @description Remplir
#' @return Remplir
#' @export

modifications_communales <- function(){
}

# modifications_communales <- function(date_debut,date_fin){
#
# donnees <- historiq
# donnees <- donnees[which(donnees$EFF > as.Date(date_debut,"%d-%m-%Y") & donnees$EFF <= as.Date(date_fin,"%d-%m-%Y")),]
# donnees <- donnees[order(donnees$EFF),]
#
# #### partie fusions
# donnees_fusions <- donnees[which(donnees$MOD%in%c("310","320","311","321","330","340","331","341","333","332","110","111","120","130","140","150","312")),]
# phrase_fusions <- NULL
# for(codgeo in unique(donnees_fusions[which(donnees_fusions$MOD%in%c("320","321","340","341")),c("DEPCOM")])){
#   for(date in   unique(as.character(donnees_fusions[which(donnees_fusions$MOD%in%c("320","321","340","341") & donnees_fusions$DEPCOM==codgeo),c("EFF")])))    {
#
#       phrase_fusions <- paste0( phrase_fusions,
#                           if(length(intersect(unique(donnees_fusions[which(donnees_fusions$DEPCOM==codgeo & donnees_fusions$EFF==date),"MOD"]),c("110","111","120","130","140","150")))!=0){
#                           paste(c(date," : ",
#                           "La commune de ",donnees_fusions[which(donnees_fusions$DEPCOM==codgeo & donnees_fusions$EFF==date),"NCCANC"]," (",codgeo,")",
#                           " change de nom en ",unique(donnees_fusions[which(donnees_fusions$DEPCOM==codgeo & donnees_fusions$EFF==date),"NCCOFF"]),
#                           sapply(as.character(unique(donnees_fusions[which(donnees_fusions$DEPCOM==codgeo & donnees_fusions$EFF==date & donnees_fusions$MOD%in%c("110","111","120","130","140","150")),"MOD"])),FUN=function(x){switch(x,
#                                                                                                                                                                        "110"={" [changement de nom dû à une fusion (simple ou association)]"},
#                                                                                                                                                                        "120"={" [changement de nom dû à un rétablissement]"},
#                                                                                                                                                                        "130"={" [changement de nom dû au changement de chef-lieu]"},
#                                                                                                                                                                        "140"={" [changement de nom dû au transfert du bureau centralisateur de canton]"},
#                                                                                                                                                                        "150"={" [changement de nom dû au transfert du chef-lieu d’arrondissement]"})}),
#                           ".\n"),collapse="")
#                           }
#                           ,
#                           date," : ",
#                           paste0(unique(donnees_fusions[which(donnees_fusions$MOD%in%c("320","321","340","341") & donnees_fusions$DEPCOM==codgeo & donnees_fusions$EFF==date),"NCCOFF"])," (",codgeo, ") est un rassemblement de "),
#                           paste(c(
#                             if(length(donnees_fusions[which(donnees_fusions$MOD%in%c("340","320") & donnees_fusions$DEPCOM==codgeo & donnees_fusions$EFF==date) ,"NCCOFF"])!=0){paste0(unique(donnees_fusions[which(donnees_fusions$MOD%in%c("320","340") & donnees_fusions$DEPCOM==codgeo & donnees_fusions$EFF==date) ,"NCCOFF"])," (",unique(donnees_fusions[which(donnees_fusions$MOD%in%c("320","340") & donnees_fusions$DEPCOM==codgeo & donnees_fusions$EFF==date) ,"DEPCOM"]),")",collapse=", ")},
#                             paste0(donnees_fusions[which(donnees_fusions$MOD%in%c("310","311","312","330","331","332","333") & donnees_fusions$COMECH==codgeo & donnees_fusions$EFF==date) ,"NCCOFF"]," (",donnees_fusions[which(donnees_fusions$MOD%in%c("310","311","312","330","331","332","333") & donnees_fusions$COMECH==codgeo & donnees_fusions$EFF==date),"DEPCOM"],")")
#                             #,if(length(donnees_fusions[which(donnees_fusions$MOD%in%c("110") & donnees_fusions$DEPCOM==codgeo & donnees_fusions$EFF==date) ,"NCCANC"])!=0){paste0(unique(donnees_fusions[which(donnees_fusions$MOD%in%c("110") & donnees_fusions$DEPCOM==codgeo & donnees_fusions$EFF==date) ,"NCCANC"])," (",unique(donnees_fusions[which(donnees_fusions$MOD%in%c("110") & donnees_fusions$DEPCOM==codgeo & donnees_fusions$EFF==date) ,"DEPCOM"]),")")}
#                           )
#                           ,collapse=", "),
#                           #paste(paste0(donnees_fusions[which((donnees_fusions$MOD=="110" | donnees_fusions$MOD=="111"| donnees_fusions$MOD=="120") & donnees_fusions$DEPCOM==codgeo) ,"NCCANC"]," (",codgeo,")",collapse=", "))),collapse=", "),
#                           sapply(unique(as.character(donnees_fusions[which(donnees_fusions$MOD%in%c("320","321","340","341") & donnees_fusions$DEPCOM==codgeo & donnees_fusions$EFF==date & donnees_fusions$MOD%in%c("310","320","311","321","330","340","331","341","333","332")),c("MOD")])),FUN=function(x){switch(x,
#                                                                                                                                                                                                                               "320"={" [fusion simple]"},
#                                                                                                                                                                                                                               "321"={" [commune nouvelle sans commune(s) déléguée(s)]"},
#                                                                                                                                                                                                                               "340"={" [fusion-association]"},
#                                                                                                                                                                                                                               "341"={" [commune nouvelle avec commune(s) déléguée(s)]"}
#                                                                                                                                                                                                                               )}),
#                           ".\n"
#   )
#   }
# }
#
# if((as.Date("1982-03-03") >= as.Date(date_debut,"%d-%m-%Y") & as.Date("1982-03-03") <= as.Date(date_fin,"%d-%m-%Y"))  | (as.Date("1982-04-03")  <= as.Date(date_fin,"%d-%m-%Y") & as.Date("1982-04-03")  >= as.Date(date_debut,"%d-%m-%Y")) ){
#   phrase_fusions <- paste0(phrase_fusions,"ATTENTION, l'évènement suivant a été pris en compte dans les données de l'Insee seulement au 04/03/1982 : 1982-03-03 : Flaignes-Havys (08169) est un rassemblement de Flaignes-Havys (08169), Havys (08221) [fusion simple].\n")
#   }
#
# ##### partie défusions
# donnees_defusions <- donnees[which(donnees$MOD%in%c("200","210","220","230")),]
# phrase_defusions <- NULL
# for(codgeo in unique(donnees_defusions[which(donnees_defusions$MOD%in%c("220","230")),c("DEPCOM")])){
#   for(date in   unique(as.character(donnees_defusions[which(donnees_defusions$MOD%in%c("220","230") & donnees_defusions$DEPCOM==codgeo),c("EFF")]))){
#
#   phrase_defusions <- paste0(phrase_defusions,
#                              date," : ",
#                              paste0(unique(donnees_defusions[which((donnees_defusions$MOD%in%c("220","230")) & donnees_defusions$DEPCOM==codgeo & donnees_defusions$EFF==date),"NCCOFF"])," (",codgeo, ") s'est séparée en "),
#                              paste(c(paste0(unique(donnees_defusions[which(donnees_defusions$MOD%in%c("220","230") & donnees_defusions$DEPCOM==codgeo & donnees_defusions$EFF==date) ,"NCCOFF"])," (",codgeo,")"),paste0(unique(donnees_defusions[which(donnees_defusions$MOD%in%c("200","210") & donnees_defusions$COMECH==codgeo & donnees_defusions$EFF==date) ,"NCCOFF"])," (",donnees_defusions[which(donnees_defusions$MOD%in%c("200","210") & donnees_defusions$COMECH==codgeo & donnees_defusions$EFF==date) ,"DEPCOM"],")")),collapse = ", "),
#                              sapply(unique(as.character(donnees_defusions[which((donnees_defusions$MOD%in%c("220","230")) & donnees_defusions$DEPCOM==codgeo & donnees_defusions$EFF==date),c("MOD")])),FUN=function(x){switch(x,
#                                                                                                                                                                                                                     "230"={" [rétablissement]"},
#                                                                                                                                                                                                                     "220"={" [création]"})}),
#                           ".\n"
#   )
# }
# }
#
# #rajouter les exceptions Insee...
# if((as.Date("2014-01-01") >= as.Date(date_debut,"%d-%m-%Y") & as.Date("2014-01-01") <= as.Date(date_fin,"%d-%m-%Y"))  | (as.Date("2015-01-01")  <= as.Date(date_fin,"%d-%m-%Y") & as.Date("2015-01-01")  >= as.Date(date_debut,"%d-%m-%Y")) ){
#   phrase_defusions <- paste0(phrase_defusions,"ATTENTION, l'évènement suivant a été pris en compte dans les données de l'Insee seulement au 01/01/2015 : 2014-01-01 : Loisey (55298) s'est séparée en Loisey (55298), Culey (55138) [rétablissement].\n")
#   }
#
# #### partie changements de codes
# donnees_changements_codes <- donnees[which(donnees$MOD%in%c("410","411","130","500")),]
# phrase_changements_codes <- NULL
# for(codgeo in unique(donnees_changements_codes[which(donnees$MOD%in%c("410","411")),c("DEPCOM")])){
#   for(date in   unique(as.character(donnees_changements_codes[which(donnees_changements_codes$DEPCOM==codgeo & donnees$MOD%in%c("410","411")),c("EFF")]))){
#
#   phrase_changements_codes <- paste0(phrase_changements_codes,
#     date," : ",
#     "Le code commune de ",donnees_changements_codes[which(donnees_changements_codes$DEPCOM==codgeo & donnees_changements_codes$EFF==date & donnees$MOD%in%c("410","411")),"NCCOFF"], " passe de ",
#     donnees_changements_codes[which(donnees_changements_codes$DEPCOM==codgeo & donnees_changements_codes$EFF==date & donnees$MOD%in%c("410","411")),"DEPANC"], " à ", donnees_changements_codes[which(donnees_changements_codes$DEPCOM==codgeo & donnees_changements_codes$EFF==date & donnees$MOD%in%c("410","411")),"DEPCOM"],
#     sapply(as.character(unique(donnees_changements_codes[which(donnees_changements_codes$DEPCOM==codgeo & donnees_changements_codes$EFF==date & donnees$MOD%in%c("410","411")),"MOD"])),FUN=function(x){switch(x,
#                                                                                                                                          "410"={" [changement de département]"},
#                                                                                                                                          "411"={" [changement de département dû à la création d'une commune nouvelle]"})}),
#
#     ".\n"
#   )
# }
# }
#
# for(codgeo in unique(donnees_changements_codes[which(donnees_changements_codes$MOD%in%c("500")),c("C_LOFF")])){
#   for(date in   unique(as.character(donnees_changements_codes[which(donnees_changements_codes$C_LOFF==codgeo & donnees_changements_codes$MOD%in%c("500")),c("EFF")]))){
#
#     phrase_changements_codes <- paste0(phrase_changements_codes,
#
#                                        date," : ",
#                                        "Le code commune de ",unique(donnees_changements_codes[which(donnees_changements_codes$DEPCOM==codgeo & donnees_changements_codes$EFF==date & donnees_changements_codes$MOD%in%c("130")),"NCCOFF"]), " passe de ", unique(donnees_changements_codes[which(donnees_changements_codes$C_LOFF==codgeo & donnees_changements_codes$EFF==date & donnees_changements_codes$MOD%in%c("500")),"C_LANC"]), " à ",codgeo,
#                                        sapply(as.character(unique(donnees_changements_codes[which(donnees_changements_codes$C_LOFF==codgeo & donnees_changements_codes$EFF==date & donnees_changements_codes$MOD%in%c("500")),"MOD"])),FUN=function(x){switch(x,
#                                                                                                                                                                                                                                                               "500"={" [changement de code dû à un changement de chef-lieu]"})}),
#
#                                        ".\n"
#     )
#   }
# }
#
# if((as.Date("1990-02-01") >= as.Date(date_debut,"%d-%m-%Y") & as.Date("1990-02-01") <= as.Date(date_fin,"%d-%m-%Y"))  | (as.Date("1990-03-05")  <= as.Date(date_fin,"%d-%m-%Y") & as.Date("1990-03-05")  >= as.Date(date_debut,"%d-%m-%Y")) ){
#   phrase_changements_codes <- paste0(phrase_changements_codes,"ATTENTION, l'évènement suivant a été pris en compte dans les données de l'Insee seulement au 05/03/1990 : 1990-02-01 : Le code commune de Oudon passe de 14624 à 14697 [changement de code dû à un changement de chef-lieu].\n")
# }
#
# # if((as.Date("1976-01-01") >= as.Date(date_debut,"%d-%m-%Y") & as.Date("1976-01-01") <= as.Date(date_fin,"%d-%m-%Y")) | (substr(date_debut,7,10)=="1968") ){
# #   phrase_changements_codes <- paste0(phrase_changements_codes,"ATTENTION, les changements de codes des communes de Corse ont eu lieu pour les données Insee dès le recensement de 1968.\n")
# # }
#
# #### partie changement de noms
# donnees_changements_noms <- donnees[which(donnees$MOD%in%c("100","140","150")),]
# phrase_changements_noms <- NULL
# for(codgeo in unique(donnees_changements_noms[,c("DEPCOM")])){
#   for(date in   unique(as.character(donnees_changements_noms[which(donnees_changements_noms$DEPCOM==codgeo),c("EFF")]))){
#
#     phrase_changements_noms <- paste0(phrase_changements_noms,
#       date," : ",
#       "La commune de ",donnees_changements_noms[which(donnees_changements_noms$DEPCOM==codgeo & donnees_changements_noms$EFF==date),"NCCANC"]," (",codgeo,")",
#       " change de nom en ",donnees_changements_noms[which(donnees_changements_noms$DEPCOM==codgeo & donnees_changements_noms$EFF==date),"NCCOFF"],
#       sapply(as.character(unique(donnees_changements_noms[which(donnees_changements_noms$DEPCOM==codgeo & donnees_changements_noms$EFF==date),"MOD"])),FUN=function(x){switch(x,
#                                                                                                                                            "100"={" [changement de nom]"},
#                                                                                                                                            "140"={" [changement de nom dû au transfert du bureau centralisateur de canton]"},
#                                                                                                                                            "150"={" [changement de nom dû au transfert du chef-lieu d’arrondissement]"}
#                                                                                                                                            )}),
#       ".\n"
#     )
#   }
# }
#
# liste <- list(fusions=phrase_fusions, defusions=phrase_defusions, changements_codes=phrase_changements_codes, changements_noms=phrase_changements_noms)
#
# return(liste)
#
# }


