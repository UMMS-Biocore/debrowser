#' debrowserIQRplot
#'
#' Module for an IQR plot that can be used in data prep and 
#' low count removal modules
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param data, a matrix that includes expression values
#' @return IQR 
#' @export
#'
#' @examples
#'     x <- debrowserIQRplot()
#'
debrowserIQRplot <- function(input = NULL, output = NULL, session = NULL, data = NULL) {
    if (is.null(data)) return(NULL)
    output$IQR <- renderPlotly({
      getIQRPlot(data, input)
    })
    output$IQRUI <- renderUI({
    shinydashboard::box(
        collapsible = TRUE, title = session$ns("plot"), status = "primary", 
        solidHeader = TRUE, width = NULL,
        draggable = TRUE,  plotlyOutput(session$ns("IQR"),
             width = input$width, height=input$height))
    })
}

#' getIQRPlotUI
#'
#' IQR plot UI.  
#' @param id, namespace id
#' @note \code{getIQRPlotUI}
#' @return the panel for IQR plots;
#'
#' @examples
#'     x <- getIQRPlotUI("IQR")
#'
#' @export
#'
getIQRPlotUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("IQRUI"))
}

#' IQRPlotControlsUI
#'
#' Generates the controls in the left menu for an IQR plot#' 
#' @param id, namespace id
#' @note \code{IQRPlotControlsUI}
#' @return returns the left menu
#' @examples
#'     x <- IQRPlotControlsUI("IQR")
#' @export
#'
IQRPlotControlsUI <- function(id) {
  ns <- NS(id)
  shinydashboard::menuItem(paste0(id, " - Options"),
      textInput(ns("breaks"), "Breaks", value = "100" )
  )
}


#' getIQRPlot
#'
#' Makes IQR boxplot plot
#'
#' @param data, count or normalized data
#' @param input, input
#' @param title, title
#'
#' @export
#'
#' @examples
#'     getIQRPlot()
#'
getIQRPlot <- function(data=NULL, input=NULL, title = ""){
  if (is.null(data)) return(NULL)
  data <- as.data.frame(data)
  cols <- colnames(data)
  data[, cols] <- apply(data[, cols], 2,
      function(x) log10(as.integer(x) + 1))
  
  data <- addID(data)
  mdata <- melt(as.data.frame(data[,c("ID", cols)]),"ID")
  colnames(mdata)<-c("ID", "samples", "logcount")

  p <- plot_ly(mdata, x = ~samples, y = ~logcount, type = "box",
               width = input$width, height=input$height,
               marker = list(color = 'rgb(8,81,156)',
                             outliercolor = 'rgba(219, 64, 82, 0.6)',
                             line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                                         outlierwidth = 2))) %>%
      plotly::layout(title = title,
           xaxis = list(title = "samples"),
           yaxis = list(title = "logcount")) 
    if (!is.null(input$left))
        p <- p %>% plotly::layout(margin = list(l = input$left,
                                 b = input$bottom,
                                 t = input$top,
                                 r = input$right
    ))
  p$elementId <- NULL
  if (!is.null(input$svg) && input$svg == TRUE)
      p <- p %>% config(toImageButtonOptions = list(format = "svg"))
  p
}