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
#'         demodata[rowSums(demodata[,2:7])>10,2:7]))
#'
#' @export
#'
run_pca <- function(x=NULL, retx = TRUE,
                center = TRUE, scale = TRUE) {
    if ( is.null(x) || ncol(x) < 2) return (NULL)
    x <- x[rowSums(x)>0, ]
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
#' @return pca list
#' @examples
#'     load(system.file("extdata", "demo", "demodata.Rda",
#'             package="debrowser"))
#'     metadata<-cbind(colnames(demodata[,2:7]), 
#'             colnames(demodata[,2:7]),
#'             c(rep("Cond1",3), rep("Cond2",3)))
#'     colnames(metadata)<-c("samples", "color", "shape")
#'
#'     a <- plot_pca(getNormalizedMatrix(
#'             demodata[rowSums(demodata[,2:7])>10,2:7]),
#'             metadata = metadata, color = "samples",
#'             size = 5, shape = "shape")
#'
#' @export
#'
plot_pca <- function(dat = NULL, pcx = 1, pcy = 2,
    metadata = NULL, color = NULL, shape = NULL,
    size = NULL, textonoff = "Off", legendSelect = "fill") {
        if ( is.null(dat) || ncol(dat) < 2) return(NULL)
    
        pca_data <- run_pca(dat)
        x <- pca_data$PCs
        explained <- pca_data$explained
        plot_data <- data.frame(x)
        # Prepare data frame to pass to ggplot
        if (!is.null(metadata)) {
            plot_data <- cbind(plot_data, metadata)
        } 
        xaxis <- paste0("PC", pcx)
        yaxis <- paste0("PC", pcy)
        p_data <- plot_data[,c(xaxis, yaxis, "samples", "color", "shape")]
        colnames(p_data) <- c("x", "y", "samples", "color", "shape")
        # Prepare axis labels
        xaxis <- sprintf("PC%d (%.2f%%)", pcx,
            round(explained[pcx] * 100, 2))
        yaxis <- sprintf("PC%d (%.2f%%)", pcy,
            round(explained[pcy] * 100, 2))

        a <- p_data %>% ggvis(x = ~x, y = ~y) %>% 
            layer_points(size := 100, 
                fill =~ color, shape =~ shape, key := ~samples) %>%
            add_tooltip(getToolTipPCA, "hover") %>%
            add_axis("x", title = xaxis) %>%
            add_axis("y", title = yaxis) %>%
            set_options(duration = 0, width = "auto", height = "auto", 
                resizable = TRUE)
        if (textonoff == "On")
            a <- a %>% layer_text(text := ~samples,  fontSize := 12, 
                       align := "left", baseline := "bottom", stroke := "black")
        if (legendSelect == "color") {
            a <- a %>% hide_legend("shape") %>%
            add_legend("fill")
        }
        else{
            a <- a %>% hide_legend("fill") %>%
            add_legend("shape")
        }
        a
}

#' getToolTipPCA
#'
#' Prepares tooltiptext for PCA plot
#'
#' @param dat, data
#' @return tooltip text
#'
#' @export
#'
#' @examples
#' x <- getToolTipPCA()
#'
getToolTipPCA <- function(dat=NULL){
    if (is.null(dat)) return(NULL)
    
    paste0("<b>", dat$samples, "</b>")
}

#' getPCAexplained
#'
#' Creates a more detailed plot using the PCA results from
#' the selected dataset.
#'
#' @param datasetInput, selected data
#' @param input, from user
#' @return explained plot
#' @examples
#' load(system.file("extdata", "demo", "demodata.Rda", 
#' package="debrowser"))
#' input<-c()
#' input$qcplot<-"pca"
#' input$col_list<-colnames(demodata[,2:7])
#' x <- getPCAexplained(getNormalizedMatrix(demodata[,2:7]), 
#'     input)
#'
#' @export
#'
getPCAexplained <- function(datasetInput = NULL, 
    input = NULL) {
    if (is.null(datasetInput)) return(NULL)
    datexp <- NULL
    pcaset <- NULL
    if (input$qcplot == "pca"){
        if (!all(input$col_list %in% colnames(datasetInput), na.rm = FALSE)) return (NULL)
        dataset <- datasetInput[,input$col_list[c(input$col_list)
            %in% colnames(datasetInput)]]
        if (dim(dataset)[2] == 0) return(NULL)
        pca_data <- run_pca(dataset)
        datexp <- data.frame(cbind(unlist(lapply(
            c(1:length(pca_data$explained)), 
            function(x){paste0("PC", x)})), 
            round(pca_data$explained * 100, 2)))
        colnames(datexp) <- c("PCs", "explained")
        datexp$explained <- as.numeric( as.character(datexp$explained) )
         
        var <- pca_data$pca$sdev^2/sum(pca_data$pca$sdev^2)
        
        ## Select the genes for PCA, removing the least variable 

        dThresh.pctile <- 1 - as.numeric(input$pctile)     # distance threshold
        gList.dThresh <- c()

        d <- pca_data$pca$rotation[,c(input$pcselx)]
        dThresh<-quantile(d, dThresh.pctile)
        gList.dThresh <- names(which(d>dThresh))
        pcaset <-  datasetInput[gList.dThresh, ]
    }
    return (list(plotdata =  datexp, pcaset = pcaset))
}

#' drawPCAExplained
#'
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
    a <- NULL
    if (is.null(explainedData)) return(NULL)
    a <- explainedData %>% ggvis(x = ~PCs, y = ~explained) %>% 
        layer_bars() %>%
        set_options(width = "auto", height = "auto", resizable = TRUE ) %>%
        scale_ordinal('x', domain=explainedData$PCs)
    a
}

