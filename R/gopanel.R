#' getGoPanel
#'
#' Creates go term analysis panel within the shiny
#' display.
#' 
#' @note \code{getGoPanel}
#' @return the panel for go term analysis;
#'
#' @examples  
#'     x <- getGoPanel()
#'
#' @export
#' 
getGoPanel <- function(){
       gopanel <- list(
           wellPanel(helpText( "Please select parameters and press the 
                    submit button in the left menu for the plots"),
            getHelpButton("method", 
            "http://debrowser.readthedocs.io/en/master/examples/examples.html#go-term-plots")),
            tabsetPanel(id = "gotabs", type = "tabs",
                tabPanel(title = "Plot", value = "gopanel1", id = "gopanel1",
                     column(12, wellPanel( plotOutput("GOPlots1")))),
                tabPanel(title = "Table", value = "gopanel2", id = "gopanel2",
                     column(12, wellPanel( DT::dataTableOutput("gotable"))))
            ),
            getKEGGModal(),
            getTableModal()
        )
       return(gopanel)
}

#' getGOPlots
#'
#' Go term analysis panel.  Generates appropriate GO plot
#' based on user selection.
#'
#' @param dataset, the dataset used
#' @param GSEARes, GSEA results
#' @param input, input params
#' @note \code{getGOPlots}
#' @return the panel for go plots;
#'
#' @examples
#'     x<- getGOPlots()
#' @export
#' 
getGOPlots <- function(dataset = NULL, GSEARes = NULL, input = NULL){
    if (is.null(dataset)) return(NULL)
    goplots <- NULL
    org <- input$organism
    if (input$goplot == "disease")
        org <- "org.Hs.eg.db"
    genelist <- getGeneList(rownames(dataset), org)
    genelist <- genelist$ENTREZID
    gopval <- as.numeric(input$gopvalue)
    if (input$goplot == "enrichGO"){
        res <- getEnrichGO(genelist, ont = input$ontology,
            pvalueCutoff = gopval, org = input$organism)
        goplots<-res
        if (input$goextplot == "Dotplot")
            goplots$p <- dotplot(res$enrich_p, showCategory=30)
    }
    else if (input$goplot == "enrichKEGG"){
        res <- getEnrichKEGG(genelist, pvalueCutoff=
            gopval, org = input$organism)
        goplots<-res
        if (input$goextplot == "Dotplot")
            goplots$p <- dotplot(res$enrich_p, showCategory=30)
    }
    else if (input$goplot == "compare"){
        cl <- clusterData(dataset)
        res <- compareClust(cl, fun=input$gofunc, input$ontology,
            org = input$organism)
        goplots<-res
    }
    else if (input$goplot == "disease"){
        res <- getEnrichDO(genelist, pvalueCutoff = gopval )
        goplots<-res
        if (input$goextplot == "Dotplot")
            goplots$p <- dotplot(res$enrich_p, showCategory=30)
    }
    else if (input$goplot == "GSEA") {
        res <- GSEARes
        if (nrow(res$enrich_p@result)>0) {
            res$p <-  gseaplot(res$enrich_p, by = "all", 
                title = res$enrich_p$Description[1], geneSetID = 1)
            goplots <- res
            
            if (input$goextplot == "Dotplot")
                goplots$p <- dotplot(res$enrich_p, showCategory=10, 
                    split=".sign") + facet_grid(.~.sign)
        } else {
            showNotification("no term enriched under specific pvalueCutoff...", 
                type = "error")
        }
    }
    return(goplots)
}



#' getOrganismBox
#'
#' Get the organism Box.
#"
#' @note \code{getOrganismBox}
#'
#' @export
#'
#' @note \code{getOrganismBox}
#' makes the organism box
#' @return selectInput
#'
#' @examples
#'     x <- getOrganismBox()
#'
getOrganismBox <- function(){
    organismBox <- list(
        conditionalPanel( ( condition <- "input.goplot!='disease' &&
                            input.gofunc != 'enrichDO'"),
        selectInput("organism", "Choose an organism:",
        choices =  c( "Human" = "org.Hs.eg.db", 
            "Mouse" = "org.Mm.eg.db", 
            "Rat" = "org.Rn.eg.db", 
            "Zebrafish" = "org.Dr.eg.db",
            "Fly" = "org.Dm.eg.db",
            "Worm" = "org.Ce.eg.db",
            "Yeast" = "org.Sc.sgd.db",
            "Arabidopsis" = "org.At.tair.db"
        ))))
    return(organismBox)
}

#' getOrganism
#'
#' @param org, organism
#' @note \code{getOrganism}
#'
#' @export
#' @return organism name for keg
#'
#' @examples
#'     x <- getOrganism()
#'
getOrganism <- function(org){
    organisms <-  list("hsa", "mmu", "rno", 
                      "dre", "dme", "cel", "sce", "At")
    names(organisms) <- c("org.Hs.eg.db", 
                          "org.Mm.eg.db", 
                          "org.Rn.eg.db", 
                          "org.Dr.eg.db",
                          "org.Dm.eg.db",
                          "org.Ce.eg.db",
                          "org.Sc.sgd.db",
                          "org.At.tair.db")
    organisms[org][[1]]
}

#' getOrganismPathway
#'
#' @param org, organism
#' @note \code{getOrganismPathway}
#'
#' @export
#' @return organism name for pathway
#'
#' @examples
#'     x <- getOrganismPathway()
#'
getOrganismPathway <- function(org){
    organisms <- list("human", "mouse", "rat", 
                      "zebrafish", "fly", "celegans", "yeast", "arabidopsis")
    names(organisms) <- c("org.Hs.eg.db", 
                          "org.Mm.eg.db", 
                          "org.Rn.eg.db", 
                          "org.Dr.eg.db",
                          "org.Dm.eg.db",
                          "org.Ce.eg.db",
                          "org.Sc.sgd.db",
                          "org.At.tair.db")
    organisms[org][[1]]
}
