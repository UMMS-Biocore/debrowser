#' getPCAPlotUI
#'
#' PCA plots UI.  
#' @param id, namespace id
#' @note \code{getPCAPlotUI}
#' @return the panel for PCA plots;
#'
#' @examples
#'     x <- getPCAPlotUI("pca")
#'
#' @export
#'
getPCAPlotUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("pcaplot"))
}

#' debrowserpcaplot
#'
#' Module for a pca plot with its loadings 
#' as a mainplot in debrowser
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param pcadata, a matrix that includes expression values
#' @param metadata, metadata to color the plots
#' @return main plot
#'
#' @return panel
#' @export
#'
#' @examples
#'     x <- debrowserpcaplot()
#'
debrowserpcaplot <- function(input = NULL, output = NULL, session = NULL, pcadata = NULL, metadata = NULL) {
    if(is.null(pcadata)) return(NULL)
    qcplots <-  reactive({ 
        if(is.null(pcadata)) return(NULL)
        sc <- getShapeColor(input)
        plot_pca(pcadata, input$pcselx, input$pcsely,
            metadata = metadata, color = sc$color,
            size = 5, shape = sc$shape,
            textonoff = sc$textonoff,
            legendSelect = sc$legendSelect, input = input )
    })
    output$pcaplot <- renderUI({
        list(fluidRow(
        column(12,
        shinydashboard::box(
        collapsible = TRUE, title = "PCA Plot", status = "primary", 
        solidHeader = TRUE, width = NULL,
        draggable = TRUE, plotlyOutput(session$ns("pca1"), 
        height= input$height, width=input$width, inline=TRUE) 
        ),
        shinydashboard::box(
        collapsible = TRUE, title = "Loadings", status = "primary", 
        solidHeader = TRUE, width = NULL,
        draggable = TRUE,  plotlyOutput(session$ns("pca2"), 
        height= input$height, width=input$width, inline=TRUE) )) ) 
        )
    })
    output$pca1 <- renderPlotly({
        p <- qcplots()$plot1
        p$elementId <- NULL
        p
    })
    output$pca2 <- renderPlotly({
        p <- qcplots()$plot2
        p$elementId <- NULL
        p
    })
    output$colorShapeSelect <- renderUI({
        getColorShapeSelection(metadata, input, session)
    })
}

#' pcaPlotControlsUI
#'
#' Generates the PCA PLots Left menu to be displayed within the DEBrowser.
#'
#' @param id, namespace id
#' @note \code{pcaPlotControlsUI}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- pcaPlotControlsUI("pca")
#' @export
#'
pcaPlotControlsUI <- function(id  = "pca") {
    ns <- NS(id)
    list(fluidRow(column(12, getPCselection(id, 1, "x")), 
         column(12, getPCselection(id, 2, "y"))),
            fluidRow(
            column(12, getHideLegendOnOff(id)),
            column(12, getTextOnOff(id)),
            column(12, getLegendSelect(id))),
    uiOutput(ns("colorShapeSelect")))
}

#' run_pca
#'
#' Runs PCA on the selected dataset.
#'
#' @param x, dataframe with experiment data
#' @param retx, specifies if the data should be returned
#' @param center, center the PCA (Boolean)
#' @param scale, scale the PCA (Boolean)
#' @return pca list
#' @examples
#'     load(system.file("extdata", "demo", "demodata.Rda", 
#'         package="debrowser"))
#'     pca_data<-run_pca(getNormalizedMatrix(
#'         demodata[rowSums(demodata[,1:6])>10,1:6]))
#'
#' @export
#'
run_pca <- function(x=NULL, retx = TRUE,
    center = TRUE, scale = TRUE) {
    if ( is.null(x) || ncol(x) < 2 || nrow(x) < 1) return (NULL)
    x <- subset(x, apply(x, 1, var, na.rm = TRUE) >  0)
    pca <- prcomp(t(x), retx = retx,
         center = center, scale. = scale)
    variances <- pca$sdev ^ 2
    explained <- variances / sum(variances)
    
    return(list(PCs = pca$x, explained = explained, pca = pca))
}

#' plot_pca
#'
#' Plots the PCA results for the selected dataset.
#'
#' @param dat, data
#' @param pcx, x axis label
#' @param pcy, y axis label
#' @param metadata, additional data
#' @param color, color for plot
#' @param shape, shape for plot
#' @param size, size of the plot
#' @param textonoff, text on off
#' @param legendSelect, select legend
#' @param input, input param
#' @return pca list
#' @examples
#'     load(system.file("extdata", "demo", "demodata.Rda",
#'             package="debrowser"))
#'     metadata<-cbind(colnames(demodata[,1:6]), 
#'             colnames(demodata[,1:6]),
#'             c(rep("Cond1",3), rep("Cond2",3)))
#'     colnames(metadata)<-c("samples", "color", "shape")
#'     
#'     a <- plot_pca(getNormalizedMatrix(
#'             demodata[rowSums(demodata[,1:6])>10,1:6]),
#'             metadata = metadata, color = "samples",
#'             size = 5, shape = "shape")
#'
#' @export
#'
plot_pca <- function(dat = NULL, pcx = 1, pcy = 2,
    metadata = NULL, color = NULL, shape = NULL,
    size = NULL, textonoff = "Off", legendSelect = "samples", input = NULL) {
    if ( is.null(dat) || is.null(ncol(dat)) || ncol(dat) < 2 || nrow(dat)<1) return(NULL)

    pca_data <- run_pca(dat)
    p_data <- prepPCADat(pca_data, metadata, input, pcx, pcy)
    
    # Prepare axis labels
    xaxis <- sprintf("PC%d (%.2f%%)", pcx,
                     round(pca_data$explained[pcx] * 100, 2))
    yaxis <- sprintf("PC%d (%.2f%%)", pcy,
                     round(pca_data$explained[pcy] * 100, 2))

    plot1 <- ggplot(data=p_data, aes(x=x, y=y))
    
    if (legendSelect == "color") {
        plot1 <-  plot1 + geom_point(mapping=aes(shape=shape, color=color), size=3 )
    }else{
        plot1 <-  plot1 + geom_point(mapping=aes(shape=shape, color=shape), size=3 )
    }
    if (textonoff == "On")
        plot1 <- plot1 + geom_text(aes(label=samples), vjust = 0, nudge_y = 1)
    plot1 <- plot1 + theme(legend.title = element_blank())
    plot1 <- plot1 +  labs(x = xaxis, y = yaxis)
    if (!is.null(input$top))
        plot1 <- plot1 + theme( plot.margin = margin(t = input$top, r =input$right, b =input$bottom, l = input$left, "pt"))
    plot1 <- ggplotly(plot1, width = input$width, height = input$height)
    if (!is.null(input$legendonoff) && input$legendonoff=="Off") 
        plot1 <- plotly::hide_legend(plot1)

    plot1$elementId <- NULL
    if (!is.null(input$svg) && input$svg == TRUE)
      plot1 <- plot1 %>% config(toImageButtonOptions = list(format = "svg"))
    pcaExp <- getPCAexplained(dat, pca_data, input)
    plot2 <- drawPCAExplained(pcaExp$plotdata)
    plot2$elementId <- NULL
    if (!is.null(input$svg) && input$svg == TRUE)
      plot2 <- plot2 %>% config(toImageButtonOptions = list(format = "svg"))
    return (list(plot1 =  plot1, plot2 =  plot2, pcaset = pcaExp$pcaset))
}

#' prepPCADat
#'
#' prepares pca data with metadata. If metadata doesn't exists
#' it puts all the sampels into a signlge group; "Conds".
#' 
#' @param pca_data, pca run results
#' @param metadata, additional meta data
#' @param input, input
#' @param pcx, x axis label
#' @param pcy, y axis label
#' @return Color and shape from selection boxes or defaults
#' @examples
#'     x <- prepPCADat()
#' @export
#'
prepPCADat <- function(pca_data = NULL, metadata = NULL, input = NULL, pcx = 1, pcy = 2){
    if (is.null(pca_data)) return (NULL)
    rownames(metadata) <- metadata[,1]
    
    x <- pca_data$PCs
    plot_data <- data.frame(x)
    # Prepare data frame to pass to ggplot
    xaxis <- paste0("PC", pcx)
    yaxis <- paste0("PC", pcy)
    if (!is.null(metadata)) {
        samples <- rownames(plot_data)
        color  <- rownames(plot_data)
        shape <- "Conds"
        if (!is.null(input$color_pca) && input$color_pca != "None")
            color <- as.character(metadata[samples, input$color_pca])
        if (!is.null(input$shape_pca) && input$shape_pca != "None")
            shape <- as.character(metadata[samples, input$shape_pca])
        
        metadata <- cbind(samples, color, shape)
        plot_data <- cbind(plot_data, metadata)
        p_data <- plot_data[,c(xaxis, yaxis, "samples", "color", "shape")]
    } else {
        samples <- rownames(plot_data)
        color  <- rownames(plot_data)
        shape <- "Conds"
        p_data <- cbind( plot_data[,c(xaxis, yaxis)], samples, color, shape)
    }
    colnames(p_data) <- c("x", "y", "samples", "color", "shape")
    p_data
}
#' getPCAexplained
#'
#' Creates a more detailed plot using the PCA results from
#' the selected dataset.
#'
#' @param datasetInput, selected data
#' @param pca_data, from user
#' @param input, input params
#' @return explained plot
#' @examples
#' load(system.file("extdata", "demo", "demodata.Rda", package="debrowser"))
#' input<-c()
#' input$qcplot<-"pca"
#' input$col_list<-colnames(demodata[,1:6])
#' dat <- getNormalizedMatrix(demodata[,1:6])
#' pca_data <- run_pca(dat)
#' x <- getPCAexplained(dat, pca_data, input)
#'
#' @export
#'
getPCAexplained <- function(datasetInput = NULL, 
    pca_data = NULL, input = NULL) {
    if (is.null(datasetInput)) return(NULL)
    datexp <- NULL
    pcaset <- NULL
    size <- length(pca_data$explained)
    if (size>9)
        size <- 9
    datexp <- data.frame(cbind(unlist(lapply(
        c(1:size), 
        function(x){paste0("PC", x)})), 
        round(pca_data$explained * 100, 2)))
    colnames(datexp) <- c("PCs", "explained")
    datexp$explained <- as.numeric( as.character(datexp$explained) )
    datexp <- datexp[1:size,]
    var <- pca_data$pca$sdev^2/sum(pca_data$pca$sdev^2)
    
    ## Select the genes for PCA, removing the least variable 
    
    dThresh.pctile <- 1 - as.numeric(input$pctile)     # distance threshold
    gList.dThresh <- c()
    
    d <- pca_data$pca$rotation[,c(input$pcselx)]
    dThresh<-quantile(d, dThresh.pctile)
    gList.dThresh <- names(which(d>dThresh))
    pcaset <-  datasetInput[gList.dThresh, ]
    return (list(plotdata =  datexp, pcaset = pcaset))
}

#' getShapeColor
#'
#' Generates the fill and shape selection boxes for PCA plots.
#' metadata file has to be loaded in this case
#'
#' @param input, input values
#' @return Color and shape from selection boxes or defaults
#' @examples
#'     x <- getShapeColor()
#' @export
#'
getShapeColor <- function(input = NULL) {
    if (is.null(input)) return (NULL)
    sc <-  c()
    if (!is.null(input$color_pca))
        sc$color <- input$color_pca
    if (!is.null(input$shape_pca))
        sc$shape <- input$shape_pca
    
    sc$textonoff <- input$textonoff
    sc$legendSelect <- input$legendSelect
    return(sc)
}

#' Creates a more detailed plot using the PCA results from
#' the selected dataset.
#'
#' @param explainedData, selected data
#' @return explained plot
#' @examples
#'     x <- drawPCAExplained()
#'
#' @export
#'
drawPCAExplained <- function(explainedData = NULL){
    p <- NULL
    if (is.null(explainedData)) return(NULL)
    
    p<- plot_ly(data=explainedData, x=~PCs, y=~explained,
                type = 'bar')
  
    p$elementId <- NULL
    p
}


#' getPCselection
#'
#' Generates the PC selection number to be used within DEBrowser.
#' @param id, namespace id
#' @param num, PC selection number
#' @param xy, x or y coordinate
#' @note \code{getPCselection}
#' @return PC selection for PCA analysis
#' @examples
#'     x <- getPCselection("pca")
#' @export
#'
getPCselection <- function(id, num = 1, xy = "x" ) {
    ns <- NS(id)
    numericInput(ns(paste0("pcsel", xy)),
        paste0("PC selection[", xy, "]"), num, 1, 6)
}

#' getColorShapeSelection
#'
#' Generates the fill and shape selection boxes for PCA plots.
#' metadata file has to be loaded in this case
#'
#' @param metadata, metadata table
#' @param input, input
#' @param session, session
#' @return Color and shape selection boxes
#' @examples
#'     x <- getColorShapeSelection()
#' @export
#'
getColorShapeSelection <- function(metadata = NULL, input = NULL, session = NULL) {
    if (is.null(metadata) ||  is.null(session)) return (NULL)
    list(fluidRow(column(12, selectGroupInfo(metadata, input, session$ns("color_pca"), "Color field")),
    column(12, selectGroupInfo(metadata, input, session$ns("shape_pca"), "Shape field"))))
}

#' getLegendSelect
#'
#' select legend
#' @param id, namespace id
#' @note \code{getLegendSelect}
#' @examples
#'     x <- getLegendSelect("pca")
#' @export
#'
getLegendSelect <- function(id = "pca") {
    ns <- NS(id)
    lst.choices <- as.list(c("color", "shape"))
    selectInput(ns("legendSelect"), label = "Select legend",
                choices = lst.choices,
                selected = "color")
}

#' getTextOnOff
#'
#' text on PCA plot on and off
#' @param id, namespace id
#' @note \code{getTextOnOff}
#' @examples
#'     x <- getTextOnOff("pca")
#' @export
#'
getTextOnOff <- function(id = "pca") {
    ns <- NS(id)
    lst.choices <- as.list(c("On", "Off"))
    selectInput(ns("textonoff"), label = "Text On/Off",
                choices = lst.choices,
                selected = "Off")
}

#' getHideLegendOnOff
#'
#' hide legend
#' @param id, namespace id
#' @examples
#'     x <- getHideLegendOnOff("pca")
#' @export
#'
getHideLegendOnOff <- function(id = "pca") {
    ns <- NS(id)
    lst.choices <- as.list(c("On", "Off"))
    selectInput(ns("legendonoff"), label = "Legend On/Off",
                choices = lst.choices,
                selected = "On")
}