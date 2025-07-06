rm(list=ls())
# POP DE REF 2022 en COG2024
#https://www.insee.fr/fr/statistiques/8290591?sommaire=8290669

library(tidyverse)
library(COGugaison)
setwd("maj2025")
library(tidyverse)
annee <- 2024
COGA <- COG2024

# poplegales <- read_delim("donnees_communes.csv",
#                          locale = locale(encoding = "Windows-1252"), delim=";")


poplegales <- read_delim("donnees_communes.csv",
                         locale = locale(encoding = "Windows-1252"),
                         delim=";",
                         #col_types = list(CODDEP=col_character(), CODCOM=col_character(),PMUN=col_double())
                         col_types = list(COM=col_character(),PMUN=col_double())
                         ) %>%
  #mutate(CODGEO = paste0(substr(CODDEP,1,2),CODCOM)) %>%
  mutate(CODGEO = COM) %>%
  select(CODGEO,PMUN) %>%
  as.data.frame() %>%
  enlever_PLM %>%
  right_join(COGA) %>%
  select(CODGEO,LIBGEO,PMUN) %>%
  setNames(c("CODGEO","LIBGEO","POP"))

#Uniquement Mayotte !
poplegales[which(is.na(poplegales$POP)),]

write.table(poplegales,paste0("COG",annee,"_2.csv"),sep=";",row.names=FALSE)

