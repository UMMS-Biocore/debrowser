#' debrowsermainplot
#'
#' Module for a scatter, volcano and ma plots that are going to be used 
#' as a mainplot in debrowser
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param data, a matrix that includes expression values
#' @return main plot
#'
#' @return panel
#' @export
#'
#' @examples
#'     x <- debrowsermainplot()
#'
debrowsermainplot <- function(input = NULL, output = NULL, session = NULL, data = NULL) {
    if (is.null(data)) return(NULL)
    
    plotdata <-  reactive({
        plotData(data, input)
    })
    output$mainplot <- renderUI({
        list(fluidRow(
            column(12,
            shinydashboard::box(
                collapsible = TRUE, title = "Main Plots", status = "primary", 
                solidHeader = TRUE,width = NULL,
                draggable = TRUE, plotlyOutput(session$ns("main"), 
                height=input$plotheight, width=input$plotwidth)
            ))))
    })
    output$mainPlotControlsUI <- renderUI({
        if (input$mainplot == "scatter"){
            x <- paste0('log10 Norm. Mean(Read Counts) in cond1')
            y <- paste0('log10 Norm. Mean(Read Counts) in cond2')
        }else if  (input$mainplot == "volcano"){
            x <- "log2FC"
            y <- "-log10padj"
        }else {
            x <- "A"
            y <- "M"
        }
        list(
            textInput(session$ns('xlab'),'x label', x),
            textInput(session$ns('ylab'),'y label', y),
            checkboxInput(session$ns('labelsearched'), 'Label searched points', value = FALSE),
            conditionalPanel(paste0("input['",session$ns("labelsearched"), "']"),
            colourpicker::colourInput(session$ns("labelcolor"), "Label colour", "black"),
            selectInput(session$ns("labelsize"), "Label Size", choices=c(6:30), selected=14))
        )
    })
    selectedPoint <- reactive({
        eventdata <- event_data("plotly_click", source = session$ns("source"))
        if (is.null(eventdata)){
            eventdata <- event_data("plotly_hover", source = session$ns("source"))
        }
        key <- ""
        if (!is.null(eventdata$key))
            key <- as.vector(unlist(eventdata$key))
        
        return(key)
    })
    
    getSelected  <- reactive({
        keys <- NULL
        selGeneList <- event_data("plotly_selected", source = session$ns("source"))
        if (is.null(selGeneList$key)) return (NULL)
        keys <- as.vector(unlist(selGeneList$key))
        return(keys)
    })
    
    output$main <- renderPlotly({
        data <- plotdata()$data
        mainScatterNew(input, data, session$ns("source"))
    })
    
    list( shg = (selectedPoint), shgClicked=(selectedPoint), selGenes=(getSelected))
}

#' getMainPlotUI
#'
#' main plot for volcano, scatter and maplot.  
#' @param id, namespace id
#' @note \code{getMainPlotUI}
#' @return the panel for main plots;
#'
#' @examples
#'     x <- getMainPlotUI("main")
#'
#' @export
#'
getMainPlotUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("mainplot"))
}

#' mainScatterNew
#'
#' Creates the main scatter, volcano or MA plot to be displayed within the main
#' panel.
#' @param input, input params
#' @param data, dataframe that has log2FoldChange and log10padj values
#' @param source, for event triggering to select genes
#' @return scatter, volcano or MA plot
#'
#' @examples
#'     
#'     x <- mainScatterNew()
#'
#' @export
#'
mainScatterNew <- function(input = NULL, data = NULL, source = NULL) {
    if ( is.null(data) ) return(NULL)
    
    p <- plot_ly(source = source, data=data, x=~x, y=~y, key=~key, alpha = 0.8,
                 color=~Legend, colors=getLegendColors(getLevelOrder(unique(data$Legend))), 
                 type="scatter", mode = "markers",
                 width=input$width - 100, height=input$height,
                 text=~paste("<b>", ID, "</b><br>",
                             "<br>", "padj=", format.pval(padj, digits = 2), " ",
                             "-log10padj=", round(log10padj, digits = 2),
                             "<br>", "log2FC=", round(log2FoldChange, digits = 2), " ",
                             "foldChange=", round(foldChange, digits = 2),
                             "<br>", sep = " ")) %>%
        plotly::layout(xaxis = list(title = input$xlab),
               yaxis = list(title = input$ylab)) %>% 
        plotly::layout(
            margin = list(l = input$left,
                          b = input$bottom,
                          t = input$top,
                          r = input$right
            ))
    
    if (!is.null(input$labelsearched) && input$labelsearched == TRUE){
        searched_genes <- data[(data$Legend == "GS"),]
        a <- list()
        for (i in seq_len(nrow(searched_genes))) {
            m <- searched_genes[i, ]
            a[[i]] <- list(
                x = m$x,
                y = m$y,
                text = rownames(m),
                color = 'blue',
                xref = "x",
                yref = "y",
                showarrow = TRUE,
                arrowhead = 0.5,
                ax = 20,
                ay = -40,
                font = list(color = input$labelcolor,
                            face = 2,
                            size = input$labelsize)
            )
        }
        
        p <- p %>%  plotly::layout(annotations = a)
    }
    if (!is.null(input$svg) && input$svg == TRUE)
        p <- p %>% config(toImageButtonOptions = list(format = "svg"))
    p$elementId <- NULL
    
    return(p)
}

#' plotData
#'
#' prepare plot data for mainplots 
#'
#' @note \code{plotData}
#' @param pdata, data
#' @param input, input
#' @return prepdata
#' @examples
#'     x <- plotData()
#' @export
#'
plotData <- function(pdata = NULL, input = NULL){
    if (is.null(pdata)) return(NULL)
    pdata$key <- pdata$ID
    data_rest <- pdata[ pdata$Legend!="NS",]
    data_NS <- pdata[ pdata$Legend=="NS",]
    backperc <- 10
    if (!is.null(input$backperc))  backperc <- input$backperc
    mainplot <- "scatter"
    if (!is.null(input$mainplot))  mainplot <- input$mainplot
    
    datapoints <- as.integer(nrow(data_NS) * backperc/ 100)
    if (nrow(data_NS) > datapoints){
        data_rand <- data_NS[sample(1:nrow(data_NS), datapoints,
            replace=FALSE),]
    }else{
        data_rand  <- data_NS
    }
    plot_init_data <- rbind(data_rand, data_rest)
    plot_init_data$Legend  <- factor(plot_init_data$Legend, 
         levels = getLevelOrder(unique(plot_init_data$Legend)))
    
    plot_data <- plot_init_data
    if (mainplot == "volcano") {
        plot_data <- plot_init_data[which(!is.na(plot_init_data$log2FoldChange)
                                          & !is.na(plot_init_data$log10padj)
                                          & !is.na(plot_init_data$Legend)),]
        plot_data$x <- plot_data$log2FoldChange
        plot_data$log10padj[plot_data$log10padj>input$log10padjCutoff] <- input$log10padjCutoff
        plot_data$y <- plot_data$log10padj
    } else if (mainplot == "maplot") {
        plot_data$x <- (plot_init_data$x + plot_init_data$y) / 2
        plot_data$y <- plot_init_data$y - plot_init_data$x
    }
    list( data = (plot_data))
}

#' mainPlotControlsUI
#'
#' Generates the left menu to be used for main plots
#'
#' @note \code{mainPlotControlsUI}
#' @param id, module ID
#' @return mainPlotControls
#' @examples
#'     x <- mainPlotControlsUI("main")
#' @export
#'
mainPlotControlsUI <- function(id) {
    ns <- NS(id)
    list(shinydashboard::menuItem(" Plot Type",
        startExpanded=TRUE,
        radioButtons(ns("mainplot"), "Main Plots:",
        c(Scatter = "scatter", VolcanoPlot = "volcano",
        MAPlot = "maplot"))
    ),
    shinydashboard::menuItem("Main Options",
        startExpanded=TRUE,
        sliderInput(ns("backperc"), "Background Data(%):",
        min=10, max=100, value=10, sep = "",
        animate = FALSE),
        conditionalPanel(condition <- paste0("input['", ns("mainplot"),"'] == 'volcano'"),
        sliderInput(ns("log10padjCutoff"), "Log10 padj value cutoff:",
        min=2, max=100, value=60, sep = "",
        animate = FALSE)
        ),
        uiOutput(ns("mainPlotControlsUI"))
    ))
    
}

#' getLegendColors
#'
#' Generates colors according to the data
#'
#' @note \code{getLegendColors}
#' @param Legend, unique Legends
#' @return mainPlotControls
#' @examples
#'     x <- getLegendColors(c("up", "down", "GS", "NS"))
#' @export
#'

getLegendColors<-function(Legend=c("up", "down", "NS"))
{
    colors <- c()
    for(i in seq(1:length(Legend))){
        if (Legend[i]=="Up"){
            colors <- c(colors, "red")
        }
        else if (Legend[i]=="Down"){
            colors <- c(colors, "blue")
        }
        else if (Legend[i]=="NS"){
            colors <- c(colors, "grey")
        }
        else if (Legend[i]=="GS"){
            colors <- c(colors, "green")
        }
    }
    colors
}
#' getLevelOrder
#'
#' Generates the order of the overlapping points 
#'
#' @note \code{getLevelOrder}
#' @param Level, factor levels shown in the legend
#' @return order
#' @examples
#'     x <- getLevelOrder(c("up", "down", "GS", "NS"))
#' @export
#'

getLevelOrder<-function(Level=c("up", "down", "NS"))
{
    levels <- c( "NS", "Up", "Down", "GS")
    for(i in seq(1:length(levels)))
    {
        if(!levels[i] %in% Level){
            levels <- levels[-(i)]
        }
    }
    levels
}

#' generateTestData
#'
#' This generates a test data that is suitable to main plots in debrowser
#' @param dat, DESeq results will be generated for loaded data
#' @return testData
#'
#' @examples
#'     x <- generateTestData()
#'
#' @export
#'
generateTestData <- function(dat = NULL) {
    if (is.null(dat)) return (NULL)
    ##################################################
    columns <- dat$columns
    conds <- dat$conds
    data <- dat$data
    params <-
        #Run DESeq2 with the following parameters
        c("DESeq2", "parametric", F, "Wald", "None")
    non_expressed_cutoff <- 10
    data <- subset(data, rowSums(data) > 10)
    deseqrun <- runDE(data, columns, conds, params)
    
    met <- as.data.frame(cbind(as.vector(conds), columns))
    colnames(met) <- c("conds", "columns")
    cols1 <- as.vector(met[met$conds==as.character(unique(met$conds)[1]), "columns"])
    cols2 <- as.vector(met[met$conds==as.character(unique(met$conds)[2]), "columns"])
    
    de_res <- data.frame(deseqrun)
    norm_data <- getNormalizedMatrix(data[, columns])
    rdata <- cbind(rownames(de_res), norm_data[rownames(de_res), columns],
        log10(rowMeans(norm_data[rownames(de_res),cols1])
        + 0.1), log10( rowMeans( norm_data[ rownames( de_res ), cols2])
        + 0.1), de_res[rownames(de_res),
        c("padj", "log2FoldChange")], 2 ^ de_res[rownames(de_res),                                                                                            "log2FoldChange"], -1 *
        log10(de_res[rownames(de_res), "padj"]))
    colnames(rdata) <- c("ID", columns, "x", "y", "padj",
        "log2FoldChange", "foldChange", "log10padj")
    rdata <- as.data.frame(rdata)
    rdata$padj[is.na(rdata$padj)] <- 1
    
    padj_cutoff <- 0.01
    foldChange_cutoff <- 2
    
    
    rdata$Legend <- "NS"
    rdata$Legend[rdata$log2FoldChange > log2(foldChange_cutoff) &
                     rdata$padj < padj_cutoff] <- "Up"
    rdata$Legend[rdata$log2FoldChange <= log2(1 / foldChange_cutoff) &
                     rdata$padj < padj_cutoff] <- "Down"
    
    dat <- rdata
    dat$M <- rdata$x - rdata$y
    dat$A <- (rdata$x + rdata$y) / 2
    dat
}
