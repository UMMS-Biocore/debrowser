#' debrowserlowcountfilter
#'
#' Module to filter low count genes/regions
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
#'     x <- debrowserlowcountfilter()
#'
debrowserlowcountfilter <- function(input = NULL, output = NULL, session = NULL, ldata = NULL) {
    if (is.null(ldata)) return(NULL)
    fdata <- reactiveValues(count=NULL, meta = NULL)
    observeEvent(input$submitLCF, {
        if (is.null(ldata$count)) return (NULL)
        filtd <- ldata$count
        filtd[, colnames(filtd)] <- apply(filtd[, colnames(filtd)], 2, function(x) as.integer(x))
    
        if (input$lcfmethod == "Max"){
          filtd <- subset(filtd, apply(filtd, 1, max, na.rm = TRUE)  >=  as.numeric(input$maxCutoff))
        } else if (input$lcfmethod == "Mean") {
          filtd <- subset(filtd, rowMeans(filtd, na.rm = TRUE) >= as.numeric(input$meanCutoff))
        }
        else if (input$lcfmethod == "CPM") {
            cpmcount <- edgeR::cpm(filtd)
            filtd <- subset(filtd, rowSums(cpmcount > as.numeric(input$CPMCutoff), 
            na.rm = TRUE) >= as.numeric(input$numSample))
        }
        fdata$count <- filtd
        fdata$meta <- ldata$meta
    })
  
  output$cutoffLCFMet <- renderUI({
    ret<-textInput(session$ns("maxCutoff"), "Filter features where Max Value <", value = "10" )
    if (input$lcfmethod == "Mean"){
      ret<-textInput(session$ns("meanCutoff"), "Filter features where Row Means <", value = "10" )
    }
    else if (input$lcfmethod == "CPM"){
      ret <- list(textInput(session$ns("CPMCutoff"), "Filter features where CPM <", value = "1" ),
         textInput(session$ns("numSample"), "at least # of samples", value = toString(ncol(ldata$count)-1) ))
    }
    ret
  })

  filtereddata <- reactive({
    ret <- NULL
    if(!is.null(fdata$count)){
      ret <- fdata
    }
    return(ret)
  })
 
  observe({
    getSampleDetails(output, "uploadSummary", "sampleDetails", ldata)
    getSampleDetails(output, "filteredSummary", "filteredDetails", filtereddata())
    getTableDetails(output, session, "loadedtable",  data = ldata$count,  modal = TRUE)
    callModule(debrowserhistogram, "beforeFiltering", ldata$count)
    
    if ( !is.null(filtereddata()$count ) && nrow(filtereddata()$count)>2 ) {
        getTableDetails(output, session, "filteredtable",  data = filtereddata()$count, modal = TRUE)
        callModule(debrowserhistogram, "afterFiltering", filtereddata()$count)
    }
  })
  
  list(filter=filtereddata)
}

#' dataLCFUI
#' Creates a panel to filter low count genes and regions
#'
#' @param id, namespace id
#' @return panel
#' @examples
#'     x <- dataLCFUI("lcf")
#'
#' @export
#'
dataLCFUI<- function (id) {
  ns <- NS(id)
  list(
    fluidRow(
      shinydashboard::box(title = "Low Count Filtering",
          solidHeader = TRUE, status = "info",  width = 12, 
          fluidRow(
            column(5,div(style = 'overflow: scroll',
                tableOutput(ns("uploadSummary")),
                DT::dataTableOutput(ns("sampleDetails"))),
                uiOutput(ns("loadedtable"))
            ),
            column(2,
                shinydashboard::box(title = "Filtering Methods",
                    solidHeader = TRUE, status = "info",
                    width = 12, 
                    lcfMetRadio(id),
                    uiOutput(ns("cutoffLCFMet")),
                    actionButtonDE(ns("submitLCF"), label = "Filter", styleclass = "primary")
                )
            ),
            column(5,div(style = 'overflow: scroll',
                tableOutput(ns("filteredSummary")),
                DT::dataTableOutput(ns("filteredDetails"))),
                uiOutput(ns("filteredtable"))
            )
          ),
          conditionalPanel(condition = paste0("input['", ns("submitLCF"),"']"),
          actionButtonDE("Batch", label = "Batch Effect Correction", styleclass = "primary"),
          conditionalPanel(condition = "!(input.Batch)",
          actionButtonDE("goDEFromFilter", "Go to DE Analysis", styleclass = "primary"),
          actionButtonDE("goQCplotsFromFilter", "Go to QC plots", styleclass = "primary")))
      ),
      shinydashboard::box(title = "Histograms",
                          solidHeader = TRUE, status = "info",  width = 12, 
      fluidRow(
          column(6,histogramControlsUI(ns("beforeFiltering")),
                 getHistogramUI(ns("beforeFiltering"))),
          column(6,histogramControlsUI(ns("afterFiltering")),
                 getHistogramUI(ns("afterFiltering")))
      ))
    ))
}

#' lcfMetRadio
#'
#' Radio buttons for low count removal methods
#'
#' @param id, namespace id
#' @note \code{lcfMetRadio}
#' @return radio control
#'
#' @examples
#'    
#'     x <- lcfMetRadio("lcf")
#'
#' @export
#'
lcfMetRadio <- function(id) {
    ns <- NS(id)
    radioButtons(inputId=ns("lcfmethod"), 
    label="Low count filtering method:",
    choices=c(Max='Max',
        Mean='Mean',
        CPM='CPM'
    ),
    selected='Max'
    )
}

