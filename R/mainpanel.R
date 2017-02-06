#' getMainPanel
#'
#' main panel for volcano, scatter and maplot.  
#' Barplot and box plots are in this page as well.
#'
#' @param randstr, random string for the plot containers
#' @note \code{getMainPanel}
#' @return the panel for main plots;
#'
#' @examples
#'     x <- getMainPanel()
#'
#' @export
#'
getMainPanel <- function(randstr = NULL) {
    if (is.null(randstr)) return (NULL)
    a <- list(
    column( 6, wellPanel( ggvisOutput(
            paste0("vplot1-",randstr)) ) ),
    column( 6, wellPanel( ggvisOutput(
    paste0("vplot2-",randstr)
        ) ) ),
        column( 6, wellPanel( ggvisOutput("plot3") ) ),
        column( 6, wellPanel( ggvisOutput("plot4") ) )) 
}

#' getMainPanelPlots
#'
#' Gathers the the plots to be used within the main panel.
#'
#' @param filt_data, filtered data
#' @param cols, selected columns
#' @param conds, seleced conditions
#' @param input, input from ui
#' @param compselect, selected comparison number
#' @return panel
#' @export
#'
#' @examples
#'     x <- getMainPanelPlots()
#'
getMainPanelPlots <- function(filt_data = NULL, 
    cols = NULL, conds = NULL,
    input = NULL, compselect = NULL) {
    if (is.null(filt_data) || 
        is.null(cols) || is.null(conds) ||
        is.null(input$padjtxt) || is.null(input$foldChangetxt)  )
            return(NULL)

    lb <- link_brush()
    x <- paste0("Cond", 2*compselect - 1) 
    y <- paste0("Cond", 2*compselect) 
    
    domains <- getDomains(filt_data)
    colors <- getColors(domains)
    randstr <- reactive({
        stri_rand_strings(n=1, length=8, pattern="[A-Za-z0-9]")
    })
    filt_data_rest <- filt_data[ filt_data$Legend!="NS",]
    filt_data_NS <- filt_data[ filt_data$Legend=="NS",]
    datapoints <- as.integer(nrow(filt_data_NS) * input$backperc / 100)
    if (nrow(filt_data_NS) > datapoints)
       filt_data_rand <- filt_data_NS[sample(1:nrow(filt_data_NS), datapoints,
            replace=FALSE),]
    else
       filt_data_rand  <- filt_data_NS
    filt_data <- rbind(filt_data_rand, filt_data_rest)
    
    if (input$mainplot == "volcano") {
        volcano_dat <- reactive({
        filt_data[which(!is.na(filt_data$log2FoldChange)
                        & !is.na(filt_data$log10padj)
                        & !is.na(filt_data$Legend)),]
        })
        volcano_selected <- reactive({
        volcano_dat()[lb$selected(), ]
        })
        volcano_dat1 <- reactive({volcano_dat()})
        volcano_dat1 %>% volcanoPlot(lb,  data_tooltip, domains, colors) %>%
        bind_shiny(paste0("vplot1-", randstr()))
        volcano_selected %>% volcanoZoom(data_tooltip, domains, colors) %>%
        bind_shiny(paste0("vplot2-", randstr()))
    } else if (input$mainplot == "scatter") {
        gene_selected <- reactive({
        filt_data[lb$selected(), ]
        })
        filt_data %>% mainScatter(lb, data_tooltip, x, y, domains, colors) %>% 
        bind_shiny(paste0("vplot1-", randstr()))
        gene_selected %>% scatterZoom(data_tooltip, x, y, domains, colors) %>% 
        bind_shiny(paste0("vplot2-", randstr()))
    } else if (input$mainplot == "maplot") {
        ma_dat <- reactive({
        dat <- filt_data
        dat$M <- filt_data$x - filt_data$y
        dat$A <- (filt_data$x + filt_data$y) / 2
        return(dat)
        })
        ma_dat1 <- reactive({ma_dat()})
        ma_selected <- reactive({
        ma_dat1()[lb$selected(), ]
        })
        ma_dat1 %>% MAPlot(lb, data_tooltip, domains, colors) %>% 
        bind_shiny(paste0("vplot1-", randstr()))
        ma_selected %>% MAZoom(data_tooltip, domains, colors) %>% 
        bind_shiny(paste0("vplot2-", randstr()))
    }
    # Function for generating tooltip text
    data_tooltip <- function(dd) {
        if (is.null(dd) || is.null(dd$ID)) return(NULL)
        f <- filt_data
        # Pick out the gene with this ID
        dat <- f[f$ID == dd$ID, ]
        bardata <- as.data.frame(cbind(cols,
            t(dat[, cols]), conds) )
        getHoverPlots(bardata, dat$ID)
        paste0(getToolTipText(dat))
    }
    getSelected <- reactive({
        m <- NULL
        if (input$mainplot == "volcano") {
            m <- volcano_selected()
        } else if (input$mainplot == "scatter") {
            m <- gene_selected()
        } else if (input$mainplot == "maplot") {
            m <- ma_selected()
        }
        m
    })
    list( getSelected = isolate(getSelected), 
        randstr=isolate(randstr) )
}
