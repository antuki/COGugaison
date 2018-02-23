#' @title Observer la trajectoire d'une commune
#' @name trajectoire_commune
#' @description Observer sur un diagramme interactif la trajectoire d'une commune depuis 1968 (aucun changement, fusions, défusions ou changements de codes) notamment via une application shiny.
#' @param codgeo est une chaîne de 5 caractères qui indique le code Insee de la commune considérée.
#' @param COG indique l'année de COG de la communes considérée (exemple 2014). Années possibles : de 1968 à 2017. Par défaut, vaut 2017.
#' @param donnees_insee vaut TRUE si l'on veut observer les dates de prise en compte du COG par l'Insee. En effet, quelques rares modifications communales (la défusion des communes Loisey et Culey au 1er janvier 2014 par exemple) ont été prises en compte dans les bases de données communales de l'Insee plus tard que la date officielle.
#' @details
#' Le code officiel géographique le plus récent du package est actuellement celui au 01/01/2017. \cr
#'
#' Les millésimes des COG qui peuvent être utilisés sont à ce stade les suivants : 1968, 1975, 1982, 1990, 1999, 2007 à 2017. \cr
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
#' @seealso
#' \link{changement_COG_typo}, \link{changement_COG_typo_details},  \link{COG_akinator}, \link{enlever_PLM}, \link{modification_Corse}, \link{modifications_communales},\link{nivsupra},\link{apparier_COG},\link{modification_Oudon},\link{trajectoire_commune}
#' @examples
#' ## Exemple 1
#' # Ici, nous allons observer sur un graphique interactif la trajectoire des communes ayant pour codes 01003 en 1968, 14697 en 1968, 76108 en 2014.
#' trajectoire_commune("01003", 1968,donnees_insee=F)
#' trajectoire_commune("14697", 1968,donnees_insee=F)
#' trajectoire_commune("76108", 2014,donnees_insee=F)
#' ## Exemple 2
#' # Cette fonction lance une application shiny permettant d'observer toutes les trajectoires des communes.
#' # trajectoire_commune_shiny()
#' @encoding UTF-8

#' @rdname trajectoire_commune
#' @export
trajectoire_commune <- function(codgeo,COG=annee_ref,donnees_insee=F){
  COG <- as.numeric(match.arg(as.character(COG),annees_possibles)) ## new
  base_exhaustive <-  creer_base_empilee(donnees_insee=donnees_insee)
  if(codgeo%in%get(paste0("COG",COG))[,"CODGEO"]){
    afficher_visNetwork(base_exhaustive,codgeo, COG,donnees_insee=donnees_insee)
  } else{
    print(paste0(codgeo," n'est pas un code Insee présent dans le code officiel géographique de ",COG))
  }
}


#' @rdname trajectoire_commune
#' @export
trajectoire_commune_shiny <- function(donnees_insee=FALSE){

    if (!requireNamespace("shiny", quietly = TRUE)) {
      stop("Package shiny needed for this function to work. Please install it.",
           call. = FALSE)
    }
  library(shiny)

  # créer la grande base
  annees <- annees_possibles

  base_exhaustive <-  creer_base_empilee(donnees_insee=donnees_insee)


  server <- function(input, output, session) {

    output$plot <- renderPlot({
      plot(cars, type=input$plotType)
    })

    output$text  <- renderText({
      paste0(input$commune," n'est pas un code Insee présent dans le code officiel géographique de ",input$COG)
    })

    library(visNetwork)
    output$network <- renderVisNetwork({
      afficher_visNetwork(base_exhaustive=base_exhaustive, codgeo=input$commune, COG=input$COG,donnees_insee=donnees_insee)
      })

    output$ui <- renderUI({
      if(input$commune==""){
      } else if(input$commune%in%get(paste0("COG",input$COG))[,"CODGEO"]){
        visNetworkOutput("network", height="500px")
      } else{
        verbatimTextOutput("text")
      }
    })

    # output$ui <- renderUI({
    #   switch(input$COG,
    #          "1968" = selectInput("select", label = "Commune", choices = paste0(COG1968$CODGEO," - ",COG1968$LIBGEO), selected = 1),
    #          "1975" = selectInput("select", label = "Commune", choices = paste0(COG1975$CODGEO," - ",COG1975$LIBGEO), selected = 1)
    #   )
    # })


  }

  ui <- fluidPage(
    fluidRow(
      column(2, selectInput("COG", label = "Année", choices = annees, selected = as.character(annee_ref))),
      column(10, textInput("commune", label = "Commune", placeholder = "Entrez le code Insee de la commune"))
      #column(10, uiOutput("ui"))
    ),
    #  visNetworkOutput("network")
    uiOutput("ui")
  )

 shinyApp(ui, server)

}



creer_base_empilee <- function(donnees_insee=F){
  library(dplyr)
  annees <- annees_possibles
  if (donnees_insee) {
    for (i in 1:(length(annees))) {
      assign(paste0("COG", annees[i]), get(paste0("COG", annees[i], "_insee")))
    }
    for (i in 1:(length(annees)-1)) {
      assign(paste0("PASSAGE_", annees[i], "_", annees[i+ 1]), get(paste0("PASSAGE_", annees[i], "_", annees[i+1], "_insee")))
    }
  }
  base_exhaustive <- NULL
  for(i in 1:(length(annees)-1)){
    base_exhaustive <- bind_rows(base_exhaustive ,
                                 left_join(get(paste0("COG",annees[i])) ,
                                           get(paste0("PASSAGE_",annees[i],"_",annees[i+1])),
                                           by = c("CODGEO" = paste0("cod",annees[i]))
                                 ) %>%
                                   rename(from="CODGEO",to=!!paste0("cod",annees[i+1])) %>%
                                   mutate(to=ifelse(is.na(to),from,to)
                                   ) %>%
                                   mutate(from = paste0(from,"_",annees[i]),
                                          to = paste0(to,"_",annees[i+1])
                                   ) %>%
                                   select(from,to,LIBGEO,POP,annee:ratio)
    )
  }
  return(base_exhaustive)
}


afficher_visNetwork <- function(base_exhaustive,codgeo, COG,donnees_insee=F){
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Package visNetwork needed for this function to work. Please install it.",
         call. = FALSE)
  }
  #library(dplyr)

  annees <- annees_possibles

  if (donnees_insee) {
    for (i in 1:(length(annees))) {
      assign(paste0("COG", annees[i]), get(paste0("COG", annees[i], "_insee")))
    }
  }

  if(codgeo%in%get(paste0("COG",COG))[,"CODGEO"]){
    # ne sélectionner que les lignes qui concernent la commune qui nous intéresse
    f1 <- function(data, codgeo, COG){
      indiv=paste0(codgeo,"_",COG)
      children_ancestors <- function(indiv){
        # Find children and ancestors of an indiv
        c(data[data[,"from"]==indiv,"to"],data[data[,"to"]==indiv,"from"])
      }
      family <- indiv
      new_people <- children_ancestors(indiv) # New people to inspect
      while(length(diff_new_p <- setdiff(new_people,family)) > 0){
        # if the new people aren't yet in the family :
        family <- c(family, diff_new_p)
        new_people <- unlist(sapply(diff_new_p, children_ancestors))
        new_people <- unique(new_people)
      }
      data[(data[,1] %in% family) | (data[,2] %in% family),]
    }
    base_simplifiee <- f1(base_exhaustive, codgeo=codgeo,COG=COG)
    # on enlève les années où il ne se passe rien...
    sous_base <- base_simplifiee %>% filter(substr(from,1,5)==substr(to,1,5) & is.na(typemodif))
    sous_base <- lapply(unique(substr(sous_base$from,1,5)),function(x){
      grouped_data <-  sous_base[which(substr(sous_base$from,1,5)==x),]
      vect_debut <- annees
      vect_debut[which(vect_debut%in%setdiff(annees,unique(substr(grouped_data$from,7,10))))]<- NA
      vect_debut = vect_debut[c(ifelse(annees[1]%in%substr(grouped_data$from,7,10),T,F),is.na(vect_debut)[-length(is.na(vect_debut))])  & !is.na(vect_debut)]
      vect_fin <- annees
      vect_fin[which(vect_fin%in%setdiff(annees,unique(substr(grouped_data$to,7,10))))]<- NA
      vect_fin = vect_fin[c(is.na(vect_fin)[-1],ifelse(annees[length(annees)]%in%substr(grouped_data$to,7,10),T,F)) & !is.na(vect_fin)]
      df <- data.frame(from=paste0(x,"_",vect_debut),to=paste0(x,"_",vect_fin),stringsAsFactors = F)
    })
    sous_base  <- do.call(rbind,sous_base)
    ###### ancienne version de sous_base
    # sous_base <- base_simplifiee %>% filter(substr(from,1,5)==substr(to,1,5) & is.na(typemodif)) %>%  group_by(substr(from,1,5)) %>% arrange(to) %>%
    #   mutate(from=paste0(substr(from,1,5),"_",min(substr(from,7,10))),
    #          to=paste0(substr(to,1,5),"_",max(substr(to,7,10)))
    #   ) %>% slice(1) %>% ungroup() %>% select(-8)
    base_simplifiee <- bind_rows(base_simplifiee %>% filter(substr(from,1,5)!=substr(to,1,5) | !is.na(typemodif)),
                                 sous_base) %>% as.data.frame() %>%
      mutate(ratio=ifelse(is.na(ratio),1,ratio))
    rm(sous_base)

    ### représentation graphique
    extraction_COG <- function(COG,comm){
      data <- data.frame(COG,comm)
      tab <- mget(paste0("COG",COG),envir=as.environment("package:COGugaison"))
      tab <- sapply(1:length(tab),function(i){tab[[i]] %>% filter(CODGEO==comm[i]) %>% unlist() })
      return(tab)
    }

    label_typemodif <- function(vect){
      vect[which(vect=="f")] <- "fusion"
      vect[which(vect=="d")] <- "défusion"
      vect[which(vect=="c")] <- "changement de code"
      return(vect)
    }


    edges <- base_simplifiee %>%
      mutate(arrows.middle.enabled=ifelse(is.na(typemodif),F,T),
             dashes=ifelse(is.na(typemodif),T,F),
             value= ifelse(is.na(typemodif),0.2,ratio),
             title= paste0(
               ifelse(is.na(typemodif),"",paste0("<b>",label_typemodif(typemodif),"</b>")),
               ifelse(ratio==1 | is.na(typemodif),"",paste0(" (ratio : ",round(ratio,2),")")),
               ifelse(is.na(typemodif),"","<br>"),
               ifelse(is.na(annee),"",paste0("Date : ",format(as.Date(annee), format="%d %b %Y") ,"<br>"))
             ),
             title=ifelse(title=="",NA,title)
      )

    nodes <-   data.frame(id = unique(c(base_simplifiee[,"from"],base_simplifiee[,"to"])), stringsAsFactors = F) %>%
      mutate(comm = substr(id,1,5),
             COG = substr(id,7,10),
             label = paste0(comm,"\n(",COG,")"),
             level = sapply(COG,function(x){order(unique(COG)[order(unique(COG))])[unique(COG)[order(unique(COG))]==x]}) %>% as.vector(),
             pop = extraction_COG(COG,comm)["POP",],
             libgeo = extraction_COG(COG,comm)["LIBGEO",],
             ratio = ifelse(COG!=as.character(annees[length(annees)]),left_join(.,base_simplifiee,by = c("id" = "from")) %>% select(ratio) %>% unlist() %>% as.vector(), 1),
             title = paste0("<b>",'<a target="_blank" href=https://www.insee.fr/fr/recherche/recherche-geographique?geo=COM-',comm,'&debut=0>',comm,'</a> ',libgeo,"</b><br>Population : ",pop,' hab.')
      )

    library(visNetwork)
    network <- visNetwork(nodes, edges,height="500px",width = "100%") %>%
      visNodes(color = list(border = "black", background="#000000", highlight = list(border="#000000",background="#000000")),
               font = list(face="verdana", color="#FFFFFF",size=20),
               fixed = list(allowedToMoveX=T,allowedToMoveY=T),
               shape= "ellipse"
      ) %>%
      visEdges(shadow = FALSE,
               arrows =list(middle = list(enabled = TRUE, scaleFactor = 3)),
               color = list(color = "#2AA198"),
               width=10
      ) %>%
      visHierarchicalLayout(direction = "LR", levelSeparation = 500) %>%
      visLayout(randomSeed = 12) %>%
      visInteraction(selectable=F,
                     navigationButtons=T,
                     selectConnectedEdges=F,
                     hoverConnectedEdges=F
      ) %>%
      visPhysics(hierarchicalRepulsion=list(centralGravity=0,springConstant=0,nodeDistance=0))  %>%
      visConfigure(enabled = F)



  } else{
    network=NULL
  }
  return(network)

}


