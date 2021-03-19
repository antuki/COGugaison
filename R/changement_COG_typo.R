#' @title Changer les typologies de géographie communale
#' @name changement_COG_typo
#' @description Transformer des typologies en géographie au premier janvier d'une année souhaitée en ayant le choix entre plusieurs hypothèses de classement en cas de fusion de communes de classes différentes (attribuer la classe qui contient le plus de population, définir une classe absorbante ou une classe spécifique aux regroupements de plusieurs communes de classes différentes).
#' @param table_entree correspond à la table à transformer en une autre géographie
#' @param annees est un vecteur qui liste l'ensemble des années qui séparent le code officiel géographique de départ et d'arrivée. Par exemple c(1968:1985). Le package rend possible l'utilisation de tables de passages d'une année de COG vers une année antiérieure (par exemple c(2016:2014)).
#' @param codgeo_entree est une chaîne de caractères qui indique le nom de la variable contenant les codes Insee communaux. Par défaut, il s'agit du nom de la première colonne de table_entree.
#' @param typos est un vecteur de chaînes de caractères qui indique les noms des typologies à convertir . Elles peuvent être de types numeric, character ou factor. Par défaut, il s'agit de l'ensemble des variables de table_entree sauf codgeo_entree.
#' @param methode_fusion méthode choisie en cas de fusion de communes de classes différentes :
#' - "methode_classe_fusion" : pour toutes les communes qui ont fusionné entre 2 dates indiquer comme classe la valeur inscrite dans "mot_fusion" y compris pour les fusions de communes de mêmes classes (sinon utiliser la méthode : "methode_difference"). Remarque : La fonction changement_COG_typo peut très bien s'utiliser avec des variables numériques (populations...) si l'on choisit cette hypothèse en cas de fusions de communes.
#' - "methode_difference" : créer une classe spécifique dont le nom est contenu dans mot_difference
#' - "methode_max_pop" : attribuer la classe contenant le maximum de population des communes fusionnées
#' - "methode_classe_absorbante" : attribuer la classe dite absorbante à toute commune fusionnée contenant au moins une ancienne commune appartenant à cette classe absorbante
#' - "methode_classe_absorbee" : ne pas tenir compte de cette classe dite "absorbée" pour toute commune fusionnée contenant au moins une ancienne commune appartenant à cette classe absorbée
#' @param mot_difference n'est à définir que si methode_fusion = "methode_difference". Si ce paramètre est laissé à NULL alors la commune fusionnée possède comme libellé de classe l'ensemble des libellés présents dans ses communes fusionnées séparés d'un "et". Sinon, il indique un nom de classe à attribuer aux communes fusionnées de classes différentes.
#' @param mot_fusion n'est à définir que si methode_fusion = "methode_classe_fusion". Sa valeur par défaut vaut "commune fusionnée".
#' @param classe_absorbante n'est à définir que si methode_fusion = "methode_classe_absorbante". Si ce paramètre est laissé à NULL alors la commune fusionnée possède comme libellé de classe l'ensemble des libellés présents dans ses communes fusionnées séparés d'un "et". Sinon, il indique le nom de la classe dite absorbante à attribuer à toute commune fusionnée contenant au moins une ancienne commune appartenant à cette classe absorbante.
#' @param classe_absorbee n'est à définir que si methode_fusion = "methode_classe_absorbee". Si ce paramètre est laissé à NULL alors la commune fusionnée possède comme libellé de classe l'ensemble des libellés présents dans ses communes fusionnées séparés d'un "et". Sinon, il indique le nom de la classe dite absorbée à attribuer à toute commune fusionnée contenant au moins une ancienne commune appartenant à cette classe absorbante.
#' @param libgeo vaut TRUE si l'on veut rajouter dans la table une colonne nommée "nom_commune" qui indique le nom de la commune issu du code officiel géographique et FALSE sinon.
#' @param donnees_insee vaut TRUE si les données manipulées sont produites par l'Insee. En effet, quelques rares modifications communales (la défusion des communes Loisey et Culey au 1er janvier 2014 par exemple) ont été prises en compte dans les bases de données communales de l'Insee plus tard que la date officielle.
#' @details
#' Le code officiel géographique le plus récent du package est actuellement celui au 01/01/2021. \cr
#'
#' Les millésimes des COG qui peuvent être utilisés sont à ce stade les suivants : 1968, 1975, 1982, 1990, 1999, 2008 à 2021. \cr
#'
#' Les dates de référence des codes officiels géographiques utilisés dans COGugaison sont les suivantes :
#' \itemize{
#' \item{COG 1968 : à partir du 01/03/1968}
#' \item{COG 1975 : à partir du 20/02/1975}
#' \item{COG 1982 : à partir du 04/03/1982}
#' \item{COG 1990 : à partir du 05/03/1990}
#' \item{COG 1999 : à partir du 08/03/1999}
#' \item{Pour tous les autres COG : à partir du 01/01 de chaque année}} \cr
#'
#' Les différences entre les tables de passage Insee et non Insee sont les suivantes :\cr
#' \itemize{
#' \item{1982-03-03 (pris en compte par l'Insee seulement après le 04/03/1982): Flaignes-Havys (08169) est un rassemblement de Flaignes-Havys (08169), Havys (08221) [fusion simple].}
#' \item{2014-01-01 (pris en compte par l'Insee seulement au 01/01/2015) : Loisey (55298) s'est séparée en Loisey (55298), Culey (55138) [rétablissement].}
#' \item{1990-02-01 (pris en compte par l'Insee seulement après le 05/03/1990) : Le code commune de Oudon passe de 14624 à 14697 [changement de code dû à un changement de chef-lieu].}
#' \item{2014-01-07 (pris en compte par l'Insee  dès le 01/01/2016) : Tôtes est rattachée à Notre-Dame-de-Fresnay qui devient L'Oudon (changement de code de l'Oudon de 14697 à 14472) [transfert de chef-lieu].}
#' \item{1981-09-28 (pris en compte par l'Insee  dès le 20/02/1975) : Vaudreuil-Ex-Ensemble Urbain (27701) est créée à partir des parcelles d'Incarville (27351), de Léry (27365) , de Porte-Joie (27471) , de Poses  (27474) , de Saint-Étienne-du-Vauvray (27537), de Saint-Pierre-du-Vauvray (27598), de Tournedos-sur-Seine  (27651) et du Vaudreuil (27528) [création]. Cette situation étant complexe, nous avons pour le moment considéré que Vaudreuil-Ex-Ensemble Urbain (27701) est créée à partir de parcelles du Vaudreuil (27528) uniquement.}
#' \item{En 1968, les 4 communes qui auraient dû d'après le COG être codées 2B044,2B076,2B151 et 2A325 sont codées 20044,20076,20151 et 20325 dans les données Insee.}}
#' @references
#' \itemize{
#' \item{\href{https://www.insee.fr/fr/information/2666684#titre-bloc-11}{historique des géographies communales (Insee)}}
#' \item{\href{https://www.insee.fr/fr/information/2028028}{tables d'appartenance des communes aux différents niveaux géographiques (Insee)}}}
#' @seealso \link{changement_COG_varNum}, \link{changement_COG_typo_details},  \link{COG_akinator}, \link{enlever_PLM}, \link{modification_Corse}, \link{modifications_communales},\link{nivsupra},\link{apparier_COG},\link{modification_Oudon},\link{trajectoire_commune}
#' @export
#' @examples
#' ## Exemple 1
#' # Ici nous allons transformer les deux typologies (typoA et typoB) de la table exemple_pop en géographie communale au 1er janvier 2017 (au lieu de 2014).
#' # L'hypothèse de classement en cas de fusion de communes (*methode_fusion*) choisie est celle d'une classe spécifique (*methode_difference*, classe appelée *mot_difference*="differents") aux regroupements de plusieurs communes de classes différentes. Les autres hypothèses possibles auraient pu être l'hypothèse du maximum de population *methode_max_pop* ou de classe absorbante *methode_classe_absorbante*.
#' exemple_popcom_COG2017_typo <- changement_COG_typo(table_entree=exemple_popcom[,-2],annees=c(2014:2017),methode_fusion="methode_difference",typos=c("typoA","typoB"),mot_difference = "differents",libgeo=T,donnees_insee=T)
#' head(exemple_popcom_COG2017_typo)
#' # Nous allons maintenant isoler dans une table les communes fusionnées appartenant à des classes différentes, ici selon la typologie "typoA" entre 2014 et 2015, 2015 et 2016 et 2016 et 2017.
#' details_exemple_popcom_COG2017_typo <- changement_COG_typo_details(table_entree=exemple_popcom[,-2],annees=c(2014:2017),typo="typoA", donnees_insee=T)
#' head(details_exemple_popcom_COG2017_typo[["2014_2015"]])
#' head(details_exemple_popcom_COG2017_typo[["2015_2016"]])
#' head(details_exemple_popcom_COG2017_typo[["2016_2017"]])
#' @encoding UTF-8

changement_COG_typo <- function(table_entree,annees,codgeo_entree=colnames(table_entree)[1],typos=colnames(table_entree)[-which(colnames(table_entree)==codgeo_entree)], methode_fusion=c("methode_difference","methode_classe_fusion","methode_max_pop","methode_classe_absorbante","methode_classe_absorbee"),mot_difference=NULL,mot_fusion="commune fusionnée",classe_absorbante=NULL,classe_absorbee=NULL,donnees_insee=TRUE,libgeo=FALSE){
  if(!codgeo_entree%in%colnames(table_entree)){ #NEW
    stop(paste0("codgeo_entree doit être une colonne de table_entree."))
  }
  if(any(!typos%in%colnames(table_entree))){ #NEW
      stop(paste0("typos doit être un vecteur de colonne(s) de table_entree."))
  }
  methode_fusion <- match.arg(methode_fusion) #NEW
  if(any(annees>annee_ref) | any(annees<1968)){ #NEW
    stop(paste0("annees ne doit contenir que des années comprises entre 1968 et ",annee_ref,"."))
  }
  inter <- intersect(annees_possibles,annees)


  if(annees[1]<=annees[length(annees)]){
    inter <- inter[order(inter)]
  } else{
    inter <- rev(inter[order(inter)])
  }
  annees <- unique(c(annees[1]:inter[1],inter,inter[length(inter)]:annees[length(annees)]))

  for (i in 1:(length(annees)-1)){

    if(donnees_insee & length(annees)!=1){
      assign(paste0("PASSAGE_",annees[i],"_",annees[i+1]),get(paste0("PASSAGE_",annees[i],"_",annees[i+1],"_insee")))
    }

    if(length(annees)==1 || nrow(get(paste0("PASSAGE_",annees[i],"_",annees[i+1])))==0){
      table_finale <- table_entree
    } else {

      provisoire <- merge(table_entree,get(paste0("PASSAGE_",annees[i],"_",annees[i+1])),by.x=codgeo_entree,by.y=paste0("cod",annees[i]),all.x=T,all.y=T) #all.y=T nouveau
      colnames(provisoire)[1]<- paste0("cod",annees[i])
      provisoire[which(is.na(with(provisoire,get(paste0("cod",annees[i+1]))))),"ratio"] <- 1
      provisoire[which(is.na(with(provisoire,get(paste0("cod",annees[i+1]))))),paste0("cod",annees[i+1])] <- as.character(provisoire[which(is.na(with(provisoire,get(paste0("cod",annees[i+1]))))),paste0("cod",annees[i])])
      for (var in typos){
        provisoire_court <- provisoire[,c(paste0("cod",annees[i]),paste0("cod",annees[i+1]),"annee","typemodif","ratio",var)]
        if(is.factor(provisoire_court[,var])){ ## nouveau : possibilité de mettre des factor en typologie (transformés en caractère)
          provisoire_court[,var] <- as.character(provisoire_court[,var])
        }
        table_n_d <- provisoire_court[which(is.na(provisoire_court$typemodif) | (provisoire_court$typemodif=="d")| (provisoire_court$typemodif=="c")),]
        table_f <- provisoire_court[which(provisoire_court$typemodif=="f"),]

        if(nrow(table_f)!=0){

          table_f_liste <- lapply(unique(with(table_f,get(paste0("cod",annees[i+1])))),function(x){table_f[which(with(table_f,get(paste0("cod",annees[i+1])))==x),]})
          table_f_sanspb <-table_f_liste[which(lapply(table_f_liste, FUN=function(x){ifelse(length(unique(x[,6]))==1,T,F)})==T)]
          table_f_sanspb <- lapply(table_f_sanspb, FUN=function(x){x[1,]})
          table_f_sanspb  <- do.call("rbind", table_f_sanspb)
          table_f_avecpb <-table_f_liste[which(lapply(table_f_liste, FUN=function(x){ifelse(length(unique(x[,6]))==1,T,F)})==F)]

          if (methode_fusion=="methode_classe_fusion"){
            if(!is.null(table_f_sanspb)){ #NEW suite à bug
              table_f_sanspb[,ncol(table_f_sanspb)] <- mot_fusion
            }
            table_f_avecpb <- lapply(table_f_avecpb, FUN=function(x){
              x[1,var]<- mot_fusion
              return(x[1,])
            })
          }

          if (methode_fusion=="methode_difference"){
            table_f_avecpb <- lapply(table_f_avecpb, FUN=function(x){
              categories <- unique(x[,var])[order(unique(x[,var]))]
              mot_diff <- ifelse(is.null(mot_difference),F,T)
              x[1,var]<- ifelse(mot_diff==T,mot_difference,paste(categories,collapse = " et "))
              return(x[1,])
            })
          }

          if (methode_fusion=="methode_max_pop"){
            if(donnees_insee){
              assign(paste0("COG",annees[i]),get(paste0("COG",annees[i],"_insee")))
            }
            table_f_avecpb <- lapply(table_f_avecpb, FUN=function(x){merge(x,get(paste0("COG",annees[i]))[,-2],by.x=paste0("cod",annees[i]),by.y="CODGEO",all.x=T,all.y=F)})
            table_f_avecpb  <- lapply(table_f_avecpb, FUN=function(x){x[which(x[,6]==(aggregate(POP ~ get(colnames(x)[6]),data =x, FUN=sum)[which.max(aggregate(POP ~ get(colnames(x)[6]),data = x, FUN=sum)$POP),1]))[1],-7]})
          }


          if (methode_fusion=="methode_classe_absorbante"){
           table_f_avecpb <- lapply(table_f_avecpb, FUN=function(x){
              categories <- unique(x[,var])[order(unique(x[,var]))]
              absorb <- ifelse(classe_absorbante%in%categories,T,F)
              x[1,var]<- ifelse(absorb==T,classe_absorbante,paste(categories,collapse = " et "))
              return(x[1,])
            })
          }

          if (methode_fusion=="methode_classe_absorbee"){
            table_f_avecpb <- lapply(table_f_avecpb, FUN=function(x){
              categories <- unique(x[,var])[order(unique(x[,var]))]
              absorb <- ifelse(classe_absorbee%in%categories,T,F)
              x[1,var]<- ifelse(absorb==T,paste(categories[-which(categories==classe_absorbee)],collapse = " et "),paste(categories,collapse = " et "))
              return(x[1,])
            })
          }

          table_f_avecpb <- do.call("rbind", table_f_avecpb)

          table_finale_provisoire <- rbind(table_n_d,table_f_sanspb,table_f_avecpb)[,c(2,6)]
          names(table_finale_provisoire)[names(table_finale_provisoire)==paste0("cod",annees[i+1])] <- codgeo_entree

        } else{
          table_finale_provisoire <- table_n_d[,c(2,6)]
          names(table_finale_provisoire)[names(table_finale_provisoire)==paste0("cod",annees[i+1])] <- codgeo_entree
        }

        if(var==typos[1]){
          table_finale <- table_finale_provisoire
        } else{
          table_finale <- merge(table_finale,table_finale_provisoire,by=codgeo_entree,all.x=T,all.y=F)
        }
      }
    }

    table_entree <- table_finale
  }

  if(libgeo){
    if(donnees_insee){
      assign(paste0("COG",annees[length(annees)]),get(paste0("COG",annees[length(annees)],"_insee")))
    }
    table_finale <- merge(table_finale,get(paste0("COG",annees[length(annees)]))[,c(1,2)],by.x=codgeo_entree,by.y="CODGEO",all.x=T,all.y=F)
    table_finale <- table_finale[,c(1,ncol(table_finale),2:(ncol(table_finale)-1))]
    colnames(table_finale)[2]<-"nom_commune"
  }


  table_finale <- table_finale[order(table_finale[,codgeo_entree]),]

  return(table_finale)

}
