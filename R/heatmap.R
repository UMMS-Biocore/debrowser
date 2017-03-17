#' runHeatmap
#'
#' Creates a heatmap based on the user selected parameters within shiny.
#'
#' @param data, a matrixthat includes expression values
#' @param title, title of the heatmap
#' @param dend, dendogram
#' @param names, a flag to show the rownames
#' @param clustering_method = c('complete', 'ward.D2', 'single', 'average',
#' 'mcquitty', 'median' , 'centroid')
#' @param distance_method = c('cor','euclidean', 'maximum', 'manhattan',
#' 'canberra', 'binary' ,'minkowski')
#' @param interactive, interactive heatmap
#' @return heatmap.2 plot
#'
#' @examples
#'     x <- runHeatmap(mtcars)
#'
#' @export
#' @import gplots
#' @import RColorBrewer
#'
runHeatmap <- function(data, title="Title", dend = "both",
    names = FALSE,
    clustering_method = c("ward.D2", "complete", "single",
        "average", "mcquitty", "median", "centroid"),
    distance_method = c("euclidean", "cor", "maximum",
        "manhattan", "canberra", "binary", "minkowski"), 
    interactive = FALSE) {
    if(is.null(data)) return(NULL)
    if (nrow(data)>5000)
        data <- data[1:5000, ]
    ld <- log2(data + 0.1)
    cldt <- scale(t(ld), center = TRUE, scale = TRUE)
    cld <- t(cldt)
    hclust2 <- function(x, ...) hclust(x, method = clustering_method)
    dist2 <- function(x, ...) {
        if (distance_method != "cor") {
            return(dist(x, method = distance_method))
        } else {
            return(as.dist(1 - cor(t(x))))
        }
    }
    if (interactive == FALSE){
        m <- heatmap.2(cld, Rowv = TRUE, main = title, dendrogram = dend,
        Colv = TRUE, col = redblue(256), labRow = names,
        distfun = dist2, hclustfun = hclust2, density.info = "none",
        trace = "none", margins = c(10, 10))
    }
    else {
        m <- d3heatmap::d3heatmap(cld,
                  colors = redblue(256),
                  RowV = TRUE,
                  ColV = TRUE,
                  distfun = dist2, hclustfun = hclust2,
                  yaxis_font_size = "7px",
                  xaxis_font_size = "10px",
                  show_grid = FALSE)
    }
    m
}


#' getIntHeatmap
#'
#' getIntHeatmap
#'
#' @param data, heatData
#' @param input, all input params
#' @param inputQCPlot, input poarams for QC
#' @return plot
#' @export
#'
#' @examples
#'     getIntHeatmap()
#'
getIntHeatmap <- function(data = NULL,  input = NULL, inputQCPlot = NULL) {
    if(is.null(data)) return(NULL)
    if (input$interactive == TRUE)
        runHeatmap(data, title = paste("Dataset:", input$dataset),
            clustering_method = inputQCPlot$clustering_method,
            distance_method = inputQCPlot$distance_method, interactive = TRUE)
}

#' getSelHeat
#'
#' heatmap selection functionality
#'
#' @param data, selected genes
#' @param input, input params
#' @return plot
#' @export
#'
#' @examples
#'     x <- getSelHeat()
#'
getSelHeat <- function(data=NULL, input = NULL) {
    if (is.null(input)) return(NULL)
    randstr <- reactive({
        stri_rand_strings(n=1, length=8, pattern="[A-Za-z0-9]")
    })
    getSelected <- reactive({
        selectedData <- data[unlist(strsplit(input, ",")), ]
    })
    list( getSelected = isolate(getSelected), 
        randstr=isolate(randstr) )
}