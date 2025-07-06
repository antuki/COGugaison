rm(list=ls())


#### créer les RData

#liste_psg <- ls(pattern = "^PASSAGE_")
# for(psg in liste_psg){
#   donnees <- creer_table_passage_inverse(table_passage = psg)
  #assign(paste0(substr(psg,1,8),substr(psg,14,17),substr(psg,13,13),substr(psg,9,12),substr(psg,18,100)),donnees)
  # write.table(donnees,paste0(substr(psg,1,8),substr(psg,14,17),substr(psg,13,13),substr(psg,9,12),substr(psg,18,100),".csv"),row.names=F,sep=";",dec=".")
  # }

annee_new <- 2025

### COGXXXX
library(tidyverse)
annees <- c(1968,1975,1982,1990,1999,2008:annee_new)
for(annee in annees){
  #donnees <- read.csv(paste0("COG",annee,".csv"),sep=";",stringsAsFactors = F,colClasses = c("character","character","numeric"))
  donnees <- read_delim(paste0("COG",annee,".csv"),locale = locale(encoding = "Windows-1252"), delim=";",col_types = list(col_character(), col_character(),col_double())) %>% as.data.frame()
  assign(paste0("COG",annee),donnees)
}



annees <- c(1968,1975,1982,1990,1999,2008:annee_new)
for(annee in annees){
  #donnees <- read.csv(paste0("COG",annee,"_insee.csv"),sep=";",stringsAsFactors = F,colClasses = c("character","character","numeric"))
  donnees <- read_delim(paste0("COG",annee,"_insee.csv"),locale = locale(encoding = "Windows-1252"), delim=";",col_types = list(col_character(), col_character(),col_double())) %>% as.data.frame()
  assign(paste0("COG",annee,"_insee"),donnees)
}
save(list=ls(pattern = "^COG"), file = "COG.RData")

### PASSAGE_XXXX_XXXX
annees <- c(1968,1975,1982,1990,1999,2008:annee_new)
for(i in 1:(length(annees)-1)){
  #donnees <- read.csv(paste0("PASSAGE_",annees[i],"_",annees[i+1],".csv"),sep=";",dec=".",stringsAsFactors = F,colClasses = c(rep("character",4),"numeric"))
  donnees <- read_delim(paste0("PASSAGE_",annees[i],"_",annees[i+1],".csv"),locale = locale(encoding = "Windows-1252"), delim=";",col_types = list(col_character(), col_character(),col_character(),col_character(),col_double())) %>% as.data.frame()
  assign(paste0("PASSAGE_",annees[i],"_",annees[i+1]),donnees)
}

annees <- c(1968,1975,1982,1990,1999,2008:annee_new)
for(i in 1:(length(annees)-1)){
  #donnees <- read.csv(paste0("PASSAGE_",annees[i],"_",annees[i+1],"_insee.csv"),sep=";",dec=".",stringsAsFactors = F,colClasses = c(rep("character",4),"numeric"))
  donnees <- read_delim(paste0("PASSAGE_",annees[i],"_",annees[i+1],"_insee.csv"),locale = locale(encoding = "Windows-1252"), delim=";",col_types = list(col_character(), col_character(),col_character(),col_character(),col_double())) %>% as.data.frame()
  assign(paste0("PASSAGE_",annees[i],"_",annees[i+1],"_insee"),donnees)
}

annees <- rev(c(1968,1975,1982,1990,1999,2008:annee_new))
for(i in 1:(length(annees)-1)){
  #donnees <- read.csv(paste0("PASSAGE_",annees[i],"_",annees[i+1],".csv"),sep=";",dec=".",stringsAsFactors = F,colClasses = c(rep("character",4),"numeric"))
  donnees <- read_delim(paste0("PASSAGE_",annees[i],"_",annees[i+1],".csv"),locale = locale(encoding = "Windows-1252"), delim=";",col_types = list(col_character(), col_character(),col_character(),col_character(),col_double())) %>% as.data.frame()
  assign(paste0("PASSAGE_",annees[i],"_",annees[i+1]),donnees)
}
annees <- rev(c(1968,1975,1982,1990,1999,2008:annee_new))
for(i in 1:(length(annees)-1)){
  #donnees <- read.csv(paste0("PASSAGE_",annees[i],"_",annees[i+1],"_insee.csv"),sep=";",dec=".",stringsAsFactors = F,colClasses = c(rep("character",4),"numeric"))
  donnees <- read_delim(paste0("PASSAGE_",annees[i],"_",annees[i+1],"_insee.csv"),locale = locale(encoding = "Windows-1252"), delim=";",col_types = list(col_character(), col_character(),col_character(),col_character(),col_double())) %>% as.data.frame()
  assign(paste0("PASSAGE_",annees[i],"_",annees[i+1],"_insee"),donnees)
}
save(list=ls(pattern = "^PASSAGE_"), file = "TABLES_PASSAGE.RData")

####### nivsupra
annees <- c(2008:annee_new)
for(annee in annees){
  #donnees <-  read.csv(paste0("table-appartenance-geo-communes-",annee,".csv"),sep=";",stringsAsFactors = F,colClasses="character")
  donnees <- read_delim(paste0("table-appartenance-geo-communes-",annee,".csv"),locale = locale(encoding = "Windows-1252"), delim=";",col_types = cols(.default = "c")) %>% as.data.frame()
  assign(paste0("table_supracom_",annee),donnees)
 # donnees <- read.csv(paste0("table-appartenance-geo-communes-",annee,"_libelles.csv"),sep=";",stringsAsFactors = F,colClasses="character")[,c("NIVGEO","CODGEO","LIBGEO")]
  donnees <- read_delim(paste0("table-appartenance-geo-communes-",annee,"_libelles.csv"),locale = locale(encoding = "Windows-1252"), delim=";",col_types = cols(.default = "c")) %>% as.data.frame()
  assign(paste0("libelles_supracom_",annee),donnees)
}
save(list=c(ls(pattern = "^table_supracom"),ls(pattern = "^libelles_supracom")), file = "DATA_SUPRACOM.RData")


##### Les copier au bon endroit puis faire une compression des fichiers
tools::resaveRdaFiles("data")


annee_new <-2025
COG_akinator(vecteur_codgeo = exemple_popcom[,1], donnees_insee = TRUE)
#"COG2014"

modifs <- modifications_communales(date_debut = "01-01-2017", date_fin = "01-01-2018")
cat(modifs$fusions)
cat(modifs$defusions)
cat(modifs$changements_codes)
cat(modifs$changements_noms)

exemple_popcom_COG2020_num <- changement_COG_varNum(table_entree = exemple_popcom, annees = c(2014:annee_new), agregation = TRUE, libgeo = TRUE, donnees_insee = TRUE)
head(exemple_popcom_COG2020_num)

exemple_popcom_COG2020_typo <- changement_COG_typo(table_entree = exemple_popcom,annees = c(2014:annee_new), methode_fusion = "methode_difference",typos = c("typoA","typoB"), mot_difference = "differents", libgeo = TRUE, donnees_insee=TRUE)
head(exemple_popcom_COG2020_typo)

details_exemple_popcom_COG2020_typo <- changement_COG_typo_details(table_entree = exemple_popcom[,-2], annees = c(2014:annee_new), typo = "typoA", donnees_insee = TRUE)
head(details_exemple_popcom_COG2020_typo[["2021_2022"]])
head(details_exemple_popcom_COG2020_typo[["2023_2024"]])

exemple_popcom_COG2020 <- merge(exemple_popcom_COG2020_num, exemple_popcom_COG2020_typo[,-2], by = "CODGEO", all = TRUE)
exemple_popcom_ZE2010 <- nivsupra(table_entree = exemple_popcom_COG2020, nivsupra = "DEP", agregation = TRUE)
exemple_popcom_ZE2010$densite <- exemple_popcom_ZE2010$P12_POP / exemple_popcom_ZE2010$SUPERF
head(exemple_popcom_ZE2010)

exemple_flux_sansPLM <-enlever_PLM(table_entree = exemple_flux,codgeo_entree = "COMMUNE",libgeo=NULL,agregation = F,vecteur_entree=F)
exemple_flux_sansPLM <-enlever_PLM(table_entree = exemple_flux_sansPLM, codgeo_entree = "DCLT", libgeo = NULL, agregation = FALSE, vecteur_entree = FALSE)
exemple_flux_sansPLMsansCorse <- modification_Corse(table_entree = exemple_flux_sansPLM, sens = "2A2Bvers20")
exemple_flux_COG2020 <- changement_COG_varNum(table_entree = exemple_flux_sansPLMsansCorse, annees=c(2014:annee_new), codgeo_entree = "COMMUNE", agregation = FALSE,libgeo = FALSE, donnees_insee = TRUE)
exemple_flux_COG2020 <- changement_COG_varNum(table_entree = exemple_flux_COG2020, annees = c(2014:annee_new), codgeo_entree = "DCLT", agregation = FALSE,libgeo = FALSE, donnees_insee = TRUE)

exemple_flux_COG2020_etZE <- nivsupra(table_entree=exemple_flux_COG2020,codgeo_entree="COMMUNE",nivsupra="DEP",agregation = FALSE)
exemple_flux_COG2020_etZE <- nivsupra(table_entree = exemple_flux_COG2020_etZE, codgeo_entree="DCLT", nivsupra = "DEP", agregation = FALSE)

trajectoire_commune("14697", 1968, donnees_insee = FALSE)
trajectoire_commune("76108", annee_new, donnees_insee = FALSE)
trajectoire_commune_shiny()
