## ---- fig.show='hold'---------------------------------------------------------
#devtools::install_github("antuki/COGugaison")
library(COGugaison)

## ---- fig.show='hold'---------------------------------------------------------
COGugaison:::annee_ref

## ---- fig.show='hold'---------------------------------------------------------
head(COG2017)

## ---- fig.show='hold'---------------------------------------------------------
head(PASSAGE_2015_2016_insee)

## ---- fig.show='hold'---------------------------------------------------------
head(table_supracom_2017)
head(libelles_supracom_2017)

## ---- fig.show='hold'---------------------------------------------------------
head(exemple_popcom)
str(exemple_popcom)

## ---- fig.show='hold'---------------------------------------------------------
COG_akinator(vecteur_codgeo = exemple_popcom[,1], donnees_insee = TRUE)

## ---- fig.show='hold'---------------------------------------------------------
COG_akinator(exemple_popcom$CODGEO, donnees_insee = TRUE)
apparier_COG(vecteur_codgeo = c(exemple_popcom[which(exemple_popcom$CODGEO!="01001"),1],"XXXXX"),
             donnees_insee = TRUE, COG = 2014)
appariement <- apparier_COG(vecteur_codgeo = c(exemple_popcom[which(exemple_popcom$CODGEO!="01001"),1],"XXXXX"),
                            donnees_insee = TRUE, COG = 2014)
cat(appariement$absent_de_bdd)
cat(appariement$absent_de_COG)
# regarder le libellé des communes présentes dans le COG mais pas dans la base de données
COG2014_insee[which(COG2014_insee$CODGEO %in% appariement$absent_de_bdd),c(1,2)]

## ---- fig.show='hold'---------------------------------------------------------
diagnostic <- diag_COG(exemple_popcom)
head(diagnostic)

## ---- fig.show='hold', include=FALSE------------------------------------------
library(dplyr)

## ---- fig.show='hold', eval=FALSE---------------------------------------------
#  library(dplyr)

## ---- fig.show='hold'---------------------------------------------------------
exemple_popcom_deforme <- exemple_popcom %>% 
  add_row(CODGEO = c(rep("01001",5),"76601","75101",NA,"98756","ZZZZZ"))
diagnostic2 <- diag_COG(exemple_popcom_deforme)

## ---- fig.show='hold'---------------------------------------------------------
COG_akinator(exemple_popcom_deforme$CODGEO)
diagnostic3 <- diag_COG(exemple_popcom_deforme, hypothese_COG = 2014)

## ---- fig.show='hold',warning=F,error=F, message=FALSE------------------------
# Ici, nous allons observer sur un graphique interactif la trajectoire de la commune ayant pour code 01003 en 1968
# (fusion avec 2 autres communes le 1er janvier 1974)
trajectoire_commune("01003", 1968, donnees_insee = FALSE)
# Cette fonction lance une application shiny permettant d'observer toutes les trajectoires des communes.
#trajectoire_commune_shiny()

## ---- fig.show='hold'---------------------------------------------------------
modifs <- modifications_communales(date_debut = "01-01-2014", date_fin = "01-01-2015")
cat(modifs$fusions)
cat(modifs$defusions)
cat(modifs$changements_codes)
cat(modifs$changements_noms)

## ---- fig.show='hold'---------------------------------------------------------
exemple_popcom_COG2017_num <- changement_COG_varNum(table_entree = exemple_popcom, annees = c(2014:2017),
                                                    agregation = TRUE, libgeo = TRUE, donnees_insee = TRUE)
head(exemple_popcom_COG2017_num)

## ---- fig.show='hold'---------------------------------------------------------
exemple_popcom_COG2017_typo <- changement_COG_typo(table_entree = exemple_popcom[,-2], annees = c(2014:2017),
                                                   methode_fusion = "methode_difference", typos = c("typoA","typoB"),
                                                   mot_difference = "differents", libgeo=TRUE,
                                                   donnees_insee=TRUE)
head(exemple_popcom_COG2017_typo)

## ---- fig.show='hold'---------------------------------------------------------
details_exemple_popcom_COG2017_typo <- changement_COG_typo_details(table_entree = exemple_popcom[,-2],
                                                                   annees = c(2014:2017), typo = "typoA",
                                                                   donnees_insee = TRUE)
head(details_exemple_popcom_COG2017_typo[["2014_2015"]])
head(details_exemple_popcom_COG2017_typo[["2015_2016"]])
head(details_exemple_popcom_COG2017_typo[["2016_2017"]])

## ---- fig.show='hold'---------------------------------------------------------
exemple_popcom_COG2017 <- merge(exemple_popcom_COG2017_num, exemple_popcom_COG2017_typo[,-2], by = "CODGEO",
                                all = TRUE)
exemple_popcom_ZE2010 <- nivsupra(table_entree = exemple_popcom_COG2017, nivsupra="ZE2010", agregation = TRUE,
                                  COG = 2017)
exemple_popcom_ZE2010$densite <- exemple_popcom_ZE2010$P12_POP / exemple_popcom_ZE2010$SUPERF
head(exemple_popcom_ZE2010)

## ---- fig.show='hold'---------------------------------------------------------
head(exemple_flux)

## ---- fig.show='hold'---------------------------------------------------------
exemple_flux_sansPLM <- enlever_PLM(table_entree = exemple_flux, codgeo_entree = "COMMUNE", libgeo = NULL,
                                    agregation = FALSE, vecteur_entree = FALSE)
exemple_flux_sansPLM <- enlever_PLM(table_entree = exemple_flux_sansPLM, codgeo_entree = "DCLT", libgeo = NULL,
                                    agregation = FALSE, vecteur_entree = FALSE)

## ---- fig.show='hold'---------------------------------------------------------
exemple_flux_sansPLMsansCorse <- modification_Corse(table_entree = exemple_flux_sansPLM, sens = "2A2Bvers20")

## ---- fig.show='hold'---------------------------------------------------------
# Ici, nous allons remplacer les codes communes de l'Oudon dans une table de l'Insee
# (en COG 2014 mais que nous considérons en COG 2015 uniquement pour cet exemple).
head(exemple_popcom[which(exemple_popcom$CODGEO %in% c("14472","14697")),])
exemple_popcom_oudon <- modification_Oudon(table_entree = exemple_popcom,
                                           donnees_insee_entree = TRUE, donnees_insee_sortie = FALSE, COG = 2015)
head(exemple_popcom_oudon[which(exemple_popcom$CODGEO%in%c("14472","14697")),])

## ---- fig.show='hold'---------------------------------------------------------
exemple_flux_COG2017 <- changement_COG_varNum(table_entree = exemple_flux_sansPLMsansCorse,
                                              annees = c(2014:2017), codgeo_entree = "COMMUNE", agregation = FALSE,
                                              libgeo = FALSE, donnees_insee = TRUE)
exemple_flux_COG2017 <- changement_COG_varNum(table_entree = exemple_flux_COG2017,
                                              annees = c(2014:2017), codgeo_entree = "DCLT",
                                              agregation = FALSE, libgeo = FALSE, donnees_insee = TRUE)

## ---- fig.show='hold'---------------------------------------------------------
exemple_flux_COG2017_etZE <- nivsupra(table_entree = exemple_flux_COG2017, codgeo_entree = "COMMUNE",
                                      nivsupra = "ZE2010", agregation = FALSE, COG = 2017)
exemple_flux_COG2017_etZE <- nivsupra(table_entree = exemple_flux_COG2017_etZE, codgeo_entree = "DCLT",
                                      nivsupra = "ZE2010", agregation = FALSE, COG = 2017)
head(exemple_flux_COG2017_etZE)

