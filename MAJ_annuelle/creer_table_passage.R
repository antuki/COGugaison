rm(list=ls())

setwd("maj2025")

library(openxlsx)
annee = 2025
PASSAGE <- read.xlsx(paste0("table_passage_annuelle_", annee, ".xlsx"),sheet="Liste des fusions", startRow = 6) %>%
  filter(ANNEE_MODIF==annee) %>% select(COM_FIN, COM_INI) %>%
  setNames(paste0("cod",c(annee,annee-1))) %>%
  mutate(annee=paste0("01/01/",annee),
         typemodif="f",
         ratio=1)

write.table(PASSAGE,paste0("PASSAGE_",annee-1,"_",annee,".csv"),sep=";",row.names=FALSE)

### défusions non gérées car inexistantes en 2020, 2021, 2022, 2023. Mais scissions à partir de 2024
# Il existe c au 1er janvier 2023, à ajouter dans la main dansle fichier
# 1 Scission à coder à la main en 2025 et 4 c !
# "15031";"15141";"01/01/2025";"d";0.2
# "15035";"15141";"01/01/2025";"d";0.2
# "15047";"15141";"01/01/2025";"d";0.2
# "15141";"15141";"01/01/2025";"d";0.2
# "15171";"15141";"01/01/2025";"d";0.2
# "12218";"12076";"01/01/2025";"c";1
# "14581";"14011";"01/01/2025";"c";1
# "49126";"49069";"01/01/2025";"c";1
# "69114";"69159";"01/01/2025";"c";1

# COG2023 <- read.csv("COG2023.csv", sep=";")
# setdiff(COG2023$CODGEO, c(COG2022$CODGEO))  #27676
# setdiff(COG2022$CODGEO, c(COG2023$CODGEO, PASSAGE$cod2022))  #27058

