#' getQCPanel
#'
#' Gathers the conditional panel for QC plots
#'
#' @param input, user input
#' @note \code{getQCSection}
#' @return the panel for QC plots
#'
#' @examples
#'     x <- getQCPanel()
#'
#' @export
#'
getQCPanel <- function(input = NULL) {
    height = "700"
    width = "500"
    if (!is.null(input)) {
        height = input$height
        width = input$width
    }
    a <- list(
        wellPanel(helpText( "Please select the parameters and press the 
        submit button in the left menu for the plots" ),
        getHelpButton("method", 
        "http://debrowser.readthedocs.io/en/develop/quickstart/quickstart.html#quality-control-plots")),
        conditionalPanel(condition = "input.qcplot == 'IQR' 
                         || input.qcplot == 'Density'
                         || input.qcplot == 'pca'",
            column(12, ggvisOutput("ggvisQC1")),
            column(12, ggvisOutput("ggvisQC2"))
        ),
        conditionalPanel(condition = 
            "(!(input.interactive && input.qcplot == 'heatmap'))",
            column(12, plotOutput("qcplotout",
            height = height, width = width))),
        conditionalPanel(condition = "input.qcplot == 'pca'",
            column(12, plotOutput("pcaexplained",
            height = height, width = width))),    
        conditionalPanel(condition = "(input.interactive && input.qcplot == 'heatmap')",
            d3heatmap::d3heatmapOutput("intheatmap", width = width, height=height),
            tags$script('
                $(document).ready(function() {
                    $("#heatmap").on("shiny:recalculating", function(event) {
                    $(".d3heatmap-tip").remove();
                    });
                });
        '))
       )
    return(a)
}

#' getIntHeatmapVis
#'
#' Gathers the conditional panel for interactive heatmap
#'
#' @param randstr, randstr
#' @note \code{getIntHeatmapVis}
#' @return the panel interactive heatmap
#'
#' @examples
#'     x <- getIntHeatmapVis()
#'
#' @export
#'
getIntHeatmapVis <- function(randstr = NULL) {
    if (is.null(randstr)) return(NULL)
    a <- list(
    conditionalPanel(condition = 
        "(input.qcplot == 'heatmap' && input.interactive)",
        column(12, ggvisOutput(paste0("heatmapplot-", randstr)))
    ))
}
#' getQCPlots
#'
#' Gathers the plot data to be displayed within the
#' quality checks panel.
#'
#' @note \code{getQCPlots}
#' @param dataset, the dataset to use
#' @param input, user input
#' @param metadata, coupled samples and conditions
#' @param inputQCPlot, input QC params
#' @param drawPCAExplained, to draw pca loading plot
#' @return the panel for QC plots
#' @examples
#'     x <- getQCPlots()
#'
#' @export
#'
getQCPlots <- function(dataset = NULL, input = NULL,
    metadata = NULL, inputQCPlot = NULL, drawPCAExplained = NULL) {
    if (is.null(dataset)) return(NULL)
    a <- NULL
    if (nrow(dataset) > 0) {
        dat <- getNormalizedMatrix(dataset, input$norm_method)
        if (input$qcplot == "all2all") {
            a <- all2all(dat, input$cex)
        } else if (input$qcplot == "heatmap") {
            a <- runHeatmap(dat, title = paste("Dataset:", input$dataset),
                clustering_method = inputQCPlot$clustering_method,
                distance_method = inputQCPlot$distance_method,  interactive = FALSE)
        } else if (input$qcplot == "pca") {
            sc <- getShapeColor(input)
            pcaplot <- plot_pca(dat, input$pcselx, input$pcsely,
                metadata = metadata, color = sc$color,
                size = 5, shape = sc$shape,
                textonoff = sc$textonoff, 
                legendSelect = sc$legendSelect )
            pcaplot %>% bind_shiny("ggvisQC1")
            drawPCAExplained %>%  bind_shiny("ggvisQC2")
            
        } else if (input$qcplot == "IQR" || input$qcplot == "Density" ) {
            prepAddQCPlots(dataset, input)
        }
    }
    a
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
    
    sc$textonoff = input$textonoff
    sc$legendSelect = input$legendSelect
    sc
}

#' getQCReplot
#'
#' Prepares QCplots for comparisons and others
#' @note \code{getQCReplot}
#' @param cols, the dataset to use
#' @param conds, the dataset to use
#' @param datasetInput, the dataset to use
#' @param input, user input
#' @param inputQCPlot, input QC params
#' @param drawPCAExplained, to draw pca loading plot
#' @return the panel for QC plots
#' @examples
#'     x <- getQCReplot()
#'
#' @export
#'
getQCReplot <- function(cols = NULL, conds = NULL, 
    datasetInput = NULL, input = NULL, inputQCPlot = NULL,
    drawPCAExplained = NULL){
    if (is.null(datasetInput)) return(NULL)
    samples <- c()
    color <- c()
    shape <- c()
    if (!is.null(cols) && !input$dataset == "comparisons"){
        new_cols <- cols[which(cols %in% input$col_list)]
        new_conds <- conds[which(cols %in% input$col_list)]
        dataset <- datasetInput[, new_cols]
        samples <- new_cols
        color  <- new_cols
        shape <- new_conds

    }else{
        dataset <- datasetInput[,c(input$col_list)]
        samples <- colnames(dataset)
        color  <- colnames(dataset)
        shape <- "Conds"
    }
    mdata <- readMetaData(input)
    if (!is.null(input$color_pca) && input$color_pca != "None")
        color <- as.character(mdata[samples, input$color_pca])
    if (!is.null(input$shape_pca) && input$shape_pca != "None")
        shape <- as.character(mdata[samples, input$shape_pca])
    
    metadata <- cbind(samples, color, shape)

    if (nrow(dataset)<3) return(NULL)
    a <- getQCPlots(dataset, input, metadata,
                    inputQCPlot = inputQCPlot,
                    drawPCAExplained = drawPCAExplained)
}

#' saveQCPlot
#'
#' Saves the current QC plot selection to the users local disk.
#'
#' @note \code{saveQCPlot}
#' @param filename, filename
#' @param input, input params
#' @param datasetInput, dataset
#' @param cols, selected columns
#' @param conds, selected conditions
#' @param inputQCPlot, clustering method and distance method
#' @examples
#'     saveQCPlot()
#'
#' @export
#'
saveQCPlot <- function(filename = NULL, input = NULL, datasetInput = NULL, 
    cols = NULL, conds = NULL, inputQCPlot = NULL){
    if (is.null(datasetInput)) return(NULL)
    pdf(filename, height = input$height * 0.010370,
        width = input$width * 0.010370)
    if (!is.null(cols)){
        dataset <- datasetInput[, input$col_list]
        new_cols <- cols[which(cols %in% input$col_list)]
        new_conds <- conds[which(cols %in% input$col_list)]
        metadata <- cbind(new_cols, new_conds)
    }else{
        dataset <- datasetInput[,c(input$col_list)]
        metadata <- cbind(colnames(dataset), "Conds")
    }
    if (nrow(dataset)>2){
        getQCPlots(dataset, input, metadata,
            inputQCPlot = inputQCPlot)
        print(getPCAexplained(datasetInput, input))
    }
    dev.off()
}

#' getIQRPlot
#'
#' Makes IQR boxplot plot
#'
#' @param data, count or normalized data
#' @param cols, columns
#' @param title, title
#'
#' @export
#'
#' @examples
#'     getIQRPlot()
#'
getIQRPlot <- function(data=NULL, cols=NULL, title = ""){
    if (is.null(data)) return(NULL)
    data <- as.data.frame(data)
    data[, cols] <- apply(data[, cols], 2,
        function(x) log10(as.integer(x) + 1))
    
    data <- addID(data)
    mdata <- melt(as.data.frame(data[,c("ID", cols)]),"ID")
    colnames(mdata)<-c("ID", "samples", "logcount")
    ypos <- -5 * max(nchar(as.vector(mdata$samples)))
    visIQR <- mdata %>%
        ggvis(x = ~samples, y = ~logcount, fill := "green") %>%
        layer_boxplots() %>% 
        set_options(width = "auto", height = 350, resizable=TRUE) %>%
        add_title_pos(title = "", angle = 310,
            dy = ypos, dx = 0) %>%
        add_tooltip(getToolTipPCA, "hover") %>%
        add_axis("y", title = "logcount")
}

#' getDensityPlot
#'
#' Makes Density plots
#'
#' @param data, count or normalized data
#' @param cols, columns
#' @param title, title
#'
#' @export
#'
#' @examples
#'     getDensityPlot()
#'
getDensityPlot <- function(data=NULL, cols=NULL, title = ""){
    if (is.null(data)) return(NULL)
    data <- as.data.frame(data)
    data[, cols] <- apply(data[, cols], 2,
          function(x) log10(as.integer(x) + 1))
    
    data <- addID(data)
    mdata <- melt(as.data.frame(data[,c("ID", cols)]),"ID")
    colnames(mdata)<-c("ID", "samples", "density")
    ypos <- -5 * max(nchar(as.vector(mdata$samples)))
    visDensity <- mdata %>%
        ggvis(~density, fill = ~samples) %>%
        group_by(samples) %>%
        set_options(width = "auto", height = 350, resizable=TRUE) %>%
        layer_densities() %>% 
        add_axis("x", title = "logcount") %>%
        add_tooltip(getToolTipPCA, "hover") %>%
        add_axis("y", title = "density")
}

#' prepAddQCPlots
#'
#' prepares IQR and density plots
#'
#' @param data, barplot data
#' @param input, user input params 
#'
#' @export
#'
#' @examples
#'     prepAddQCPlots()
#'
#'
prepAddQCPlots <- function(data=NULL, input=NULL){
    if(is.null(data)) return(NULL)
    if(!is.null(input$qcplot)){
        if (input$qcplot == "IQR"){
            getIQRPlot(data, colnames(data), 
                "IQR Plot(Before Normalization)") %>% 
                bind_shiny("ggvisQC1")
            getIQRPlot(getNormalizedMatrix(data, input$norm_method), 
                colnames(data), "IQR Plot(After Normalization)") %>% 
                bind_shiny("ggvisQC2")
        }
        else if (input$qcplot == "Density"){
            getDensityPlot(data, colnames(data), 
                "Density Plot(Before Normalization)") %>% 
                bind_shiny("ggvisQC1")
            getDensityPlot(getNormalizedMatrix(data, input$norm_method), 
                colnames(data), "Density Plot(After Normalization)") %>% 
                bind_shiny("ggvisQC2")    
        }
    }
}

#' getSelectedCols
#'
#' gets selected columns
#'
#' @param data, all loaded data
#' @param datasetInput, selected dataset
#' @param input, user input params 
#'
#' @export
#'
#' @examples
#'     getSelectedCols()
#'
#'
getSelectedCols <- function(data = NULL, datasetInput = NULL, input=NULL){
    if(is.null(data) && is.null(datasetInput)) return(NULL)
    m <- NULL
    if (!is.null(input$dataset)){
        all <- input$samples
        selection <- input$col_list
        if("All" %in% input$col_list || length(input$col_list) == 0){
            selection <- all
        }else{
            selection <- input$col_list
        }
        if (!is.null(selection))
            m <- data[rownames(datasetInput), selection]
    }
    m
}