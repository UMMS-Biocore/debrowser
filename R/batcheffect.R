#' debrowserbatcheffect
#'
#' Module to correct batch effect
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param ldata, loaded data
#' @return main plot
#'
#' @return panel
#' @export
#'
#' @examples
#'     x <- debrowserbatcheffect()
#'
debrowserbatcheffect <- function(input, output, session, ldata = NULL) {
    if(is.null(ldata)) return(NULL)
    batchdata <- reactiveValues(count=NULL, meta = NULL)
    observeEvent(input$submitBatchEffect, {
    if (is.null(ldata$count)) return (NULL)

    countData <- ldata$count
    withProgress(message = 'Normalization', detail = "Normalization", value = NULL, {
        if (input$norm_method != "none"){
            countData <- getNormalizedMatrix(ldata$count, method=input$norm_method)
        }
    })
    withProgress(message = 'Batch Effect Correction', detail = "Adjusting the Data", value = NULL, {
    if (input$batchmethod == "CombatSeq" |  input$batchmethod == "Combat"){
        batchdata$count <- correctCombat(input, countData, ldata$meta, method = input$batchmethod)
    }
    else if (input$batchmethod == "Harman"){
        batchdata$count <- correctHarman(input, countData, ldata$meta)
    }
    else{
        batchdata$count <-  countData
    }
    })
    if (is.null(batchdata$count)) return(NULL)
    batchdata$meta <- ldata$meta
  })
  
  output$batchfields <- renderUI({
    if (!is.null(ldata$meta))
        list( conditionalPanel(condition = paste0("input['", session$ns("batchmethod"),"']!='none'"),
             selectGroupInfo( ldata$meta, input, session$ns("treatment"), "Treatment"),
             selectGroupInfo( ldata$meta, input, session$ns("batch"), "Batch")))
  })
  
  batcheffectdata <- reactive({
    ret <- NULL
    if(!is.null(batchdata$count)){
      ret <- batchdata
    }
    return(ret)
  })
  
  observe({
    getSampleDetails(output, "uploadSummary", "sampleDetails", ldata)
    getSampleDetails(output, "filteredSummary", "filteredDetails", batcheffectdata())
    getTableDetails(output, session, "beforebatchtable", ldata$count, modal = TRUE)
    callModule(debrowserpcaplot, "beforeCorrectionPCA", ldata$count, ldata$meta)
    callModule(debrowserIQRplot, "beforeCorrectionIQR",  ldata$count)
    callModule(debrowserdensityplot, "beforeCorrectionDensity", ldata$count)
    if ( !is.null(batcheffectdata()$count ) && nrow(batcheffectdata()$count)>2 ){
      withProgress(message = 'Drawing the plot', detail = "Preparing!", value = NULL, {
       getTableDetails(output, session, "afterbatchtable", batcheffectdata()$count, modal = TRUE)
       callModule(debrowserpcaplot, "afterCorrectionPCA",  batcheffectdata()$count, batcheffectdata()$meta)
       callModule(debrowserIQRplot, "afterCorrectionIQR",  batcheffectdata()$count)
       callModule(debrowserdensityplot, "afterCorrectionDensity", batcheffectdata()$count)
      })
    }
  })
  
  list(BatchEffect=batcheffectdata)
}


#' batchEffectUI
#' Creates a panel to coorect batch effect
#'
#' @param id, namespace id
#' @return panel
#' @examples
#'     x <- batchEffectUI("batcheffect")
#'
#' @export
#'
batchEffectUI <- function (id) {
  ns <- NS(id)

  list(
    fluidRow(
        shinydashboard::box(title = "Batch Effect Correction and Normalization",
        solidHeader = TRUE, status = "info",  width = 12, 
        fluidRow(
            column(5,div(style = 'overflow: scroll',
                tableOutput(ns("uploadSummary")),
                DT::dataTableOutput(ns("sampleDetails"))),
                uiOutput(ns("beforebatchtable"))
            ),
            column(2,
            shinydashboard::box(title = "Options",
                solidHeader = TRUE, status = "info",
                width = 12, 
                normalizationMethods(id),
                batchMethod(id),
                uiOutput(ns("batchfields")),
                actionButtonDE(ns("submitBatchEffect"), label = "Submit", styleclass = "primary")
           )
          ),
          column(5,div(style = 'overflow: scroll', 
                tableOutput(ns("filteredSummary")),
                DT::dataTableOutput(ns("filteredDetails"))),
                uiOutput(ns("afterbatchtable"))
          )
        ),
        conditionalPanel(condition = paste0("input['", ns("submitBatchEffect"),"']"),
        actionButtonDE("goDE", "Go to DE Analysis", styleclass = "primary"),
        actionButtonDE("goQCplots", "Go to QC plots", styleclass = "primary"))),
    shinydashboard::box(title = "Plots",
        solidHeader = TRUE, status = "info",  width = 12, 
        fluidRow(column(1, div()),
            tabsetPanel( id = ns("batchTabs"),
                tabPanel(id = ns("PCA"), "PCA",
                    column(5,
                        getPCAPlotUI(ns("beforeCorrectionPCA"))),
                    column(2,  
                        shinydashboard::box(title = "PCA Controls",
                        solidHeader = T, status = "info",  width = 12, 
                        tabsetPanel( id = ns("pcacontrols"),
                        tabPanel ("Before",
                        pcaPlotControlsUI(ns("beforeCorrectionPCA"))),
                        tabPanel ( "After",
                        pcaPlotControlsUI(ns("afterCorrectionPCA")))))),
                    column(5,
                        getPCAPlotUI(ns("afterCorrectionPCA")))
                ),
                tabPanel(id = ns("IQR"), "IQR",
                    column(5,
                        getIQRPlotUI(ns("beforeCorrectionIQR"))),
                    column(2, div()),
                    column(5,
                        getIQRPlotUI(ns("afterCorrectionIQR")))
                ),
                tabPanel(id = ns("Density"), "Density",
                    column(5,
                        getDensityPlotUI(ns("beforeCorrectionDensity"))),
                    column(2, div()),
                    column(5,
                        getDensityPlotUI(ns("afterCorrectionDensity")))
                )
            )
        )
      )
    ), getPCAcontolUpdatesJS())
}
#' normalizationMethods
#'
#' Select box to select normalization method prior to batch effect correction
#'
#' @note \code{normalizationMethods}
#' @param id, namespace id
#' @return radio control
#'
#' @examples
#'    
#'     x <- normalizationMethods("batch")
#'
#' @export
#'
normalizationMethods <- function(id) {
    ns <- NS(id)
    selectInput(ns("norm_method"), "Normalization Method:",
        choices = c("none", "MRN", "TMM", "RLE", "upperquartile"))
}

#' batchMethod
#'
#' select batch effect method
#' @param id, namespace id
#' @note \code{batchMethod}
#' @return radio control
#'
#' @examples
#'    
#'     x <- batchMethod("batch")
#'
#' @export
#'
batchMethod <- function(id) {
  ns <- NS(id)
  selectInput(ns("batchmethod"), "Correction Method:",
              choices = c("none", "Combat", "CombatSeq", "Harman"),
               selected='none'
  )
}

#' Correct Batch Effect using Combat in sva package
#'
#' Batch effect correction
#' @param input, input values
#' @param idata, data
#' @param metadata, metadata
#' @param method, method: either Combat or CombatSeq
#' @return data
#' @export
#'
#' @examples
#'     x<-correctCombat ()
correctCombat <- function (input = NULL, idata = NULL, metadata = NULL, method = NULL) {
  if (is.null(idata)) return(NULL)
  
  if (input$batch == "None") {
      showNotification("Please select the batch field to use Combat!", type = "error")
      return(NULL)
  }
  
  batch <- metadata[, input$batch]
  
  columns <- colnames(idata)
  datacor <- data.frame(idata[, columns])
  datacor[, columns] <- apply(datacor[, columns], 2,
      function(x) as.integer(x) + runif(1, 0, 0.01))
  
  if (input$treatment != "None") {
      treatment <- metadata[, input$treatment]
      meta <- data.frame(cbind(columns, treatment, batch))
      modcombat = model.matrix(~as.factor(treatment), data = meta)
      if(method == "Combat"){
        combat_res = sva::ComBat(dat=as.matrix(datacor), mod=modcombat, batch=batch)
      } else {
        combat_res = sva::ComBat_seq(counts=as.matrix(datacor), covar_mod = modcombat, batch=batch)
      }
  } else {
      if(method == "Combat"){
        combat_res = sva::ComBat(dat=as.matrix(datacor), batch=batch)
      } else {
        combat_res = sva::ComBat_seq(counts=as.matrix(datacor), batch=batch)
      }
  }
  
  a <- cbind(idata[rownames(combat_res), 2], combat_res)
  a[, columns] <- apply(a[, columns], 2, function(x) ifelse(x<0, 0, x))
  a[, columns] <- apply(a[, columns], 2, function(x) as.integer(x))
  colnames(a[, 1]) <- colnames(idata[, 1])
  a[,columns]
}

#' Correct Batch Effect using Harman
#'
#' Batch effect correction
#' @param input, input values
#' @param idata, data
#' @param metadata, metadata
#' @return data
#' @export
#'
#' @examples
#'     x<-correctHarman ()
correctHarman <- function (input = NULL, idata = NULL, metadata = NULL) {
  if (is.null(idata)) return(NULL)
  if (input$treatment == "None" || input$batch == "None") {
      showNotification("Please select the batch and treatment fields to use Harman!", type = "error")
      return(NULL)
  }

  batch.info <- data.frame(metadata[, c(input$treatment, input$batch)])
  rownames(batch.info) <- rownames(metadata)
  colnames(batch.info) <- c("treatment", "batch") 
  
  harman.res <- harman(idata, expt= batch.info$treatment, batch= batch.info$batch, limit=0.95)
  harman.corrected <- reconstructData(harman.res)
  harman.corrected[harman.corrected<0] <- 0
  harman.corrected
}
