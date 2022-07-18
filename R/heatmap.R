#' debrowserheatmap
#'
#' Heatmap module to create interactive heatmaps and get selected list from
#' a heatmap
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param expdata, a matrix that includes expression values
#' @return heatmapply plot
#'
#' @examples
#'     x <- debrowserheatmap()
#'
#' @export
#'
#'
debrowserheatmap <- function( input, output, session, expdata = NULL){
    if(is.null(expdata)) return(NULL)
    output$heatmap <- renderPlotly({
        shinyjs::onevent("mousemove", "heatmap", js$getHoverName(session$ns("hoveredgenename")))
        shinyjs::onevent("click", "heatmap", js$getHoverName(session$ns("hoveredgenenameclick")))

        withProgress(message = 'Drawing Heatmap', detail = "interactive", value = 0, {
            runHeatmap(input, session, orderData())
        })
    })
    output$heatmap2 <- renderPlot({
        withProgress(message = 'Drawing Heatmap', detail = "non-interactive", value = 0, {
            runHeatmap2(input, session, orderData())
        })
    })
    heatdata <- reactive({
        cld <- prepHeatData(expdata, input)
        if (input$kmeansControl)
        {
            res <- niceKmeans(cld, input)
            cld <- res$clustered
        }
        cld
    })
    
    button <- reactiveVal(FALSE)
    orderData <- reactive({
        newclus <- heatdata()
        if (input$changeOrder && isolate(button()) && !is.null(input$clusterorder)){
            newclus <- changeClusterOrder(isolate(input$clusterorder), newclus)
        }
        button(FALSE)
        newclus
    })
    observeEvent(input$changeOrder,{
        button(TRUE)
    })
    output$heatmapUI <- renderUI({
        if (is.null(input$interactive)) return(NULL)
        shinydashboard::box(
            collapsible = TRUE, title = session$ns("Heatmap"), status = "primary", 
            solidHeader = TRUE, width = NULL,
            draggable = TRUE,   getPlotArea(input, session))
    })
    
    hselGenes <- reactive({
        if (is.null(input$selgenenames)) return("")
        unlist(strsplit(input$selgenenames, split=","))
    })
    shg <- reactive({
        if (is.null(input$hoveredgenename)) return("")
        js$getSelectedGenes(session$ns("heatmap"), session$ns("selgenenames"))
        input$hoveredgenename
    })
    observe({
        if(!input$changeOrder)
            updateTextInput(session, "clusterorder", value = paste(seq(1:input$knum), collapse=","))
        
        if (is.null(shg()))
            js$getSelectedGenes()
    })
    shgClicked <- reactive({
        if (is.null(input$hoveredgenenameclick) || input$hoveredgenenameclick == "") return(input$hoveredgenename)
        input$hoveredgenenameclick
    })
    
    list( shg = (shg), shgClicked=(shgClicked), selGenes=(hselGenes), getSelected = (orderData))
}
#' getPlotArea
#'
#' returns plot area either for heatmaply or heatmap.2
#' @param input, input variables
#' @param session, session 
#' @return heatmapply/heatmap.2 plot area
#'
#' @examples
#'     x <- getPlotArea()
#'
#' @export
#'
#'
getPlotArea <- function(input = NULL, session = NULL){
    if (is.null(input)) return(NULL)
    ret <- c()
    
    if (input$interactive){
        ret <- plotlyOutput(session$ns("heatmap"),
            height=input$height, width=input$width)
    }
    else{
        ret <- plotOutput(session$ns("heatmap2"),
            height = input$height, input$width)
    }
    ret
}

#' runHeatmap
#'
#' Creates a heatmap based on the user selected parameters within shiny
#' @param input, input variables
#' @param session, session 
#' @param expdata, a matrix that includes expression values
#' @return heatmapply plot
#'
#' @examples
#'     x <- runHeatmap()
#'
#' @export
#'
#'
runHeatmap <- function(input = NULL, session = NULL, expdata = NULL){
    if (is.null(expdata)) return(NULL)
    cld <-expdata
    hclustfun_row <- function(x, ...) hclust(x, method = input$hclustFun_Row)
    hclustfun_col <- function(x, ...) hclust(x, method = input$hclustFun_Col)
    distfun_row <- function(x, ...) {
        if (input$distFun_Row != "cor") {
            return(dist(x, method = input$distFun_Row))
        } else {
            return(as.dist(1 - cor(t(x))))
        }
    }
    distfun_col <- function(x, ...) {
        if (input$distFun_Col != "cor") {
            return(dist(x, method = input$distFun_Col))
        } else {
            return(as.dist(1 - cor(t(x))))
        }
    }
    
    if (!input$customColors ) {
        heatmapColors <- eval(parse(text=paste0(input$pal,
                                                '(',input$ncol,')')))
    }
    else{
        if (!is.null(input$color1))
            heatmapColors <- colorRampPalette(c(input$color1, 
               input$color2, input$color3))(n = 1000)
    }
    
    if (!input$kmeansControl){
        p <- heatmaply(cld,
                       main = input$main,
                       xlab = input$xlab,
                       ylab = input$ylab,
                       row_text_angle = input$row_text_angle,
                       column_text_angle = input$column_text_angle,
                       dendrogram = input$dendrogram,
                       branches_lwd = input$branches_lwd,
                       seriate = input$seriation,
                       colors = heatmapColors,
                       distfun_row =  distfun_row,
                       hclustfun_row = hclustfun_row,
                       distfun_col = distfun_col,
                       hclustfun_col = hclustfun_col,
                       showticklabels = c(input$labCol, input$labRow),
                       k_col = input$k_Col, 
                       k_row = input$k_Row
        ) 
    }else {
        if (!input$showClasses){
            cld <- data.frame(cld)
            cld <- as.matrix(cld [, -match("class",names(cld))])
        }
        rhcr <- hclust(dist(cld))
        chrc <- hclust(dist(t(cld)))
        p <- heatmaply(cld,
                       main = input$main,
                       xlab = input$xlab,
                       ylab = input$ylab,
                       row_text_angle = input$row_text_angle,
                       column_text_angle = input$column_text_angle,
                       #dendrogram = input$dendrogram,
                       dendrogram = "none",
                       branches_lwd = input$branches_lwd,
                       seriate = input$seriation,
                       colors = heatmapColors,
                       showticklabels = c(input$labCol, input$labRow),
                       Rowv = as.dendrogram(rhcr),
                       Colv = as.dendrogram(chrc),
                       k_col = input$k_Col,
                       k_row = input$knum
        )
    }
    p <- p %>% 
        plotly::layout(
            height=input$height, width=input$width,
            margin = list(l = input$left,
                          b = input$bottom,
                          t = input$top,
                          r = input$right
            ))
    if (!is.null(input$svg) && input$svg == TRUE)
        p <- p %>% config(toImageButtonOptions = list(format = "svg"))
    p$elementId <- NULL
    p
}

#' runHeatmap2
#'
#' Creates a heatmap based on the user selected parameters within shiny
#' @param input, input variables
#' @param session, session 
#' @param expdata, a matrix that includes expression values
#' @return heatmap.2
#'
#' @examples
#'     x <- runHeatmap2()
#'
#' @export
#'
#'
runHeatmap2 <- function(input = NULL, session = NULL, expdata = NULL){
    if(is.null(expdata)) return(NULL)
    if (nrow(expdata)>5000)
        expdata <- expdata[1:5000, ]
    
    if (!input$customColors ) {
        heatmapColors <- eval(parse(text=paste0(input$pal,
                                                '(',input$ncol,')')))
    }
    else{
        if (!is.null(input$color1))
            heatmapColors <- colorRampPalette(c(input$color1, 
                                                input$color2, input$color3))(n = 1000)
        #heatmapColors <- colorRampPalette(c("red", "white", "blue"))(n = 1000)
    }
    
    hclustfun_row <- function(x, ...) hclust(x, method = input$hclustFun_Row)
    distfun_row <- function(x, ...) {
        if (input$distFun_Row != "cor") {
            return(dist(x, method = input$distFun_Row))
        } else {
            return(as.dist(1 - cor(t(x))))
        }
    }
    if (!input$showClasses && "class" %in% names(expdata) ){
        expdata <- data.frame(expdata)
        expdata <- as.matrix(expdata [, -match("class",names(expdata))])
    }
    if (input$kmeansControl){
        m <- heatmap.2(as.matrix(expdata), Rowv = FALSE, main = input$main, dendrogram = input$dendrogram,
                       Colv = FALSE, col = heatmapColors, labRow = input$labRow,
                       distfun = distfun_row, hclustfun = hclustfun_row, density.info = "none",
                       trace = "none", margins = c(10,10))
    }else{
        m <- heatmap.2(as.matrix(expdata), main = input$main, dendrogram = input$dendrogram,
                       col = heatmapColors, labRow = input$labRow,
                       distfun = distfun_row, hclustfun = hclustfun_row, density.info = "none",
                       trace = "none", margins = c(10,10))
    }
    m
}


#' changeClusterOrder
#'
#' change order of K-means clusters
#'
#' @note \code{changeClusterOrder}
#' @param order, order
#' @param cld, data
#' @return heatmap plot area
#' @examples
#'     x <- changeClusterOrder()
#' @export
#'
changeClusterOrder <- function(order = NULL, cld = NULL){
    if (is.null(order) || is.null(cld) ) return(NULL)
    newcluster <- c()
    idx <- as.integer(as.vector(unlist(strsplit(order, ","))))
    da <- data.frame(cld)
    for (i in 1:length(idx)) {
        newcluster <- rbind(newcluster, da[da$class == idx[i], ])
    }
    newcluster
}


#' niceKmeans
#'
#' Generates hierarchially clustered K-means clusters
#'
#' @note \code{niceKmeans}
#' @param df, data
#' @param input, user inputs
#' @param iter.max, max iteration for kmeans clustering
#' @param nstart, n for kmeans clustering
#' @return heatmap plot area
#' @examples
#'     x <- niceKmeans()
#' @export
#'
niceKmeans <-function (df = NULL, input = NULL, iter.max = 1000, nstart=100) {
    if(is.null(df)) return(NULL)
    source <-df
    kmeans <- kmeans(source, centers = input$knum, iter.max = iter.max, algorithm=input$kmeansalgo, nstart=nstart)
    clustered <- data.frame()
    distfun_row <- function(x, ...) {
        if (input$distFun_Row != "cor") {
            return(dist(x, method = input$distFun_Row))
        } else {
            return(as.dist(1 - cor(t(x))))
        }
    }
    breaks <- c();
    for (i in 1:input$knum) {
        cluster <- source[kmeans$cluster==i,]
        rows <- row.names(cluster)
        clust <- hclust(distfun_row(as.matrix(cluster)), method = input$hclustFun_Row)
        clust$rowInd <- clust[[3]]
        cluster.ordered <- cluster[clust$rowInd,]
        cluster.ordered.genes <- rows[clust$rowInd]
        row.names(cluster.ordered) <- cluster.ordered.genes
        class <- data.frame(row.names = cluster.ordered.genes)
        class[,"class"] <- i 
        cluster.ordered <- cbind(cluster.ordered, class)
        clustered <- rbind(clustered, cluster.ordered)
        if(i > 1 & i < input$knum) {
            breaks[i] <- as.numeric(breaks[i-1]) + length(rows)
        } else if(i==1) {
            breaks[i] <- length(rows);
        }
    }
    
    result <- list();
    result$clustered <- clustered;
    result$breaks <- breaks;
    return(result);
}


#' getHeatmapUI
#'
#' Generates the left menu to be used for heatmap plots
#'
#' @note \code{getHeatmapUI}
#' @param id, module ID
#' @return heatmap plot area
#' @examples
#'     x <- getHeatmapUI("heatmap")
#' @export
#'
getHeatmapUI <- function(id) {
    ns <- NS(id)
    uiOutput(ns("heatmapUI"))
}

#' heatmapControlsUI
#'
#' Generates the left menu to be used for heatmap plots
#'
#' @note \code{heatmapControlsUI}
#' @param id, module ID
#' @return HeatmapControls
#' @examples
#'     x <- heatmapControlsUI("heatmap")
#' @export
#'
heatmapControlsUI <- function(id) {
    ns <- NS(id)
    list(
        checkboxInput(ns('interactive'), 'Interactive', value = FALSE),
        kmeansControlsUI(id),
        shinydashboard::menuItem("Scale Options",
            checkboxInput(ns('scale'), 'Scale', value = TRUE),
            checkboxInput(ns('center'), 'Center', value = TRUE),
            checkboxInput(ns('log'), 'Log', value = TRUE),
            textInput(ns('pseudo'),'Pseudo Count','0.1')
        ),
        dendControlsUI(id, "Row"),
        dendControlsUI(id, "Col"),
        shinydashboard::menuItem("Heatmap Colors",
            conditionalPanel(paste0("!input['", ns("customColors"), "']"),
            palUI(id),
            sliderInput(ns("ncol"), "# of Colors", 
            min = 1, max = 256, value = 256)),
            customColorsUI(id)
        ),
        shinydashboard::menuItem("Heatmap Dendrogram",
            selectInput(ns('dendrogram'),'Type',
            choices = c("both", "row", "column", "none"),selected = 'both'),
            selectizeInput(ns("seriation"), "Seriation", 
            c(OLO="OLO", GW="GW", Mean="mean", None="none"),selected = 'OLO'),
            sliderInput(ns('branches_lwd'),'Branch Width',
            value = 0.6,min=0,max=5,step = 0.1)
        ),
        shinydashboard::menuItem("Heatmap Layout",
            textInput(ns('main'),'Title',''),
            textInput(ns('xlab'),'Sample label',''),
            sliderInput(ns('row_text_angle'),'Sample Text Angle',
            value = 0,min=0,max=180),
            textInput(ns('ylab'), 'Gene/Region label',''),
            sliderInput(ns('column_text_angle'),'Gene/Region Text Angle',
            value = 45,min=0,max=180)
        ))
}
#' kmeansControlsUI
#'
#' get kmeans controls
#'
#' @note \code{kmeansControlsUI}
#' @param id, module ID
#' @return controls
#' @examples
#'     x <- kmeansControlsUI("heatmap")
#' @export
#'
kmeansControlsUI <- function(id) {
    ns <- NS(id)
    shinydashboard::menuItem("kmeans",
        checkboxInput(ns('kmeansControl'), 'kmeans clustering', value = FALSE),
        conditionalPanel(paste0("input['", ns("kmeansControl"), "']"),
            sliderInput(ns("knum"), "k: # of Clusters", 
                min = 2, max = 20, value = 2),
            selectizeInput(ns("kmeansalgo"), "kmeans.algorithm",
                c("Hartigan-Wong", "Lloyd", "Forgy",
                "MacQueen"), selected = 'Lloyd'),
            textInput(ns('clusterorder'), 
                'The order of the clusters', ""),
            actionButtonDE(ns("changeOrder"), label = "Change Order", styleclass = "primary"),
            checkboxInput(ns('showClasses'), 'Show Classes', value = FALSE)))
}
#' dendControlsUI
#'
#' get distance metric parameters 
#'
#' @note \code{dendControlsUI}
#' @param id, module ID
#' @param dendtype, Row or Col
#' @return controls
#' @examples
#'     x <- dendControlsUI("heatmap")
#' @export
#'
dendControlsUI <- function(id, dendtype = "Row") {
    ns <- NS(id)
    shinydashboard::menuItem(paste0(dendtype, " dendrogram"),
        selectizeInput(ns(paste0("distFun_", dendtype)), "Dist. method", 
            distFunParamsUI(),
            selected = 'euclidean'),
        selectizeInput(ns(paste0("hclustFun_", dendtype)), "Clustering linkage",
            clustFunParamsUI(), 
            selected = 'complete'),
        sliderInput(ns(paste0("k_", dendtype)), "# of Clusters",
            min = 1, max = 10, value = 2),
        checkboxInput(ns(paste0('lab',dendtype)), paste0(dendtype, ' Labels'), value = TRUE))
}

#' clustFunParamsUI
#'
#' get cluster function parameter control
#'
#' @note \code{clustFunParamsUI}
#' @return cluster params
#' @examples
#'     x <- clustFunParamsUI()
#' @export
#'
clustFunParamsUI <- function() {
    c(Complete= "complete",Single= "single",Average= "average",
    Mcquitty= "mcquitty",Median= "median",Centroid= "centroid",
    Ward.D= "ward.D",Ward.D2= "ward.D2")
}

#' distFunParamsUI
#'
#' get distance metric parameters 
#'
#' @note \code{distFunParamsUI}
#' @return funParams
#' @examples
#'     x <- distFunParamsUI()
#' @export
#'
distFunParamsUI <- function() {
    c(Cor="cor", Euclidean="euclidean",Maximum='maximum',
    Manhattan='manhattan',Canberra='canberra',
    Binary='binary',Minkowski='minkowski')
}

#' palUI
#'
#' get pallete 
#'
#' @note \code{palUI}
#' @param id, namespace ID
#' @return pals
#' @examples
#'     x <- palUI("heatmap")
#' @export
#'
palUI <- function(id) {
    ns <- NS(id)
    # colSel='RdBu'
    colSel='BlueRed'
    selectizeInput(inputId = ns("pal"), 
    label ="Select Color Palette",
    choices = c('BlueRed' = 'bluered',
        'RdBu' = 'RdBu',
        'RedBlue' = 'redblue',
        'RdYlBu' = 'RdYlBu',
        'RdYlGn' = 'RdYlGn',
        'BrBG' = 'BrBG',
        'Spectral' = 'Spectral',
        'BuGn' = 'BuGn',
        'PuBuGn' = 'PuBuGn',
        'YlOrRd' = 'YlOrRd',
        'Heat' = 'heat.colors',
        'Grey' = 'grey.colors'),
    selected=colSel)
}

#' customColorsUI
#'
#' get Custom Color controls
#'
#' @note \code{getColRng}
#' @param id, namespace ID
#' @return color range
#' @examples
#'     x <- customColorsUI("heatmap")
#' @export
#'
customColorsUI <- function(id) {
    ns <- NS(id)
    list(
        checkboxInput(ns('customColors'), 'Custom Colors', value = FALSE),
        conditionalPanel(paste0("input['", ns("customColors"), "']"),
            colourpicker::colourInput(ns("color1"), "Choose min colour", "blue"),
            colourpicker::colourInput(ns("color2"), "Choose median colour", "white"),
            colourpicker::colourInput(ns("color3"), "Choose max colour", "red")))
}

#' prepHeatData
#'
#' scales the data
#'
#' @param expdata, a matrixthat includes expression values
#' @param input, input variables
#' @return heatdata
#'
#' @examples
#'     x <- prepHeatData()
#'
#' @export
#'
prepHeatData <- function(expdata = NULL, input = NULL) 
{
    if(is.null(expdata)) return(NULL)
    ld <- expdata
    if (!is.null(input$pseudo))
        ld <- ld + as.numeric(input$pseudo)
    if (!is.null(input$log) && input$log)
        ld <- log2(ld)
    cldt <- scale(t(ld), center = input$center, scale = input$scale)
    cld <- t(cldt)
    return(cld)
}

#' getSelHeat
#'
#' heatmap selection functionality
#'
#' @param expdata, selected genes
#' @param input, input params
#' @return plot
#' @export
#'
#' @examples
#'     x <- getSelHeat()
#'
getSelHeat <- function(expdata = NULL, input = NULL) {
    if (is.null(input)) return(NULL)
    getSelected <- reactive({
        expdata[unlist(strsplit(input, ",")), ]
    })
    list( getSelected = isolate(getSelected) )
}


#' heatmapJScode
#'
#' heatmap JS code for selection functionality
#'
#' @return JS Code
#' @export
#'
#' @examples
#'     x <- heatmapJScode()
#'
heatmapJScode <- function() {        
    'shinyjs.getHoverName = function(params){
    
    var defaultParams = {
    controlname : "hoveredgenename"
    };
    params = shinyjs.getParams(params, defaultParams);
    var out = ""
    
    if (typeof  document.getElementsByClassName("nums")[0] != "undefined"){
    if (typeof  document.getElementsByClassName("nums")[0].querySelectorAll("tspan.line")[0] != "undefined"){
    out = document.getElementsByClassName("nums")[0].querySelectorAll("tspan.line")[0].innerHTML.match("row: (.*)")[1]
    $("#heatmap-heatmap").attr("gname", out)
    }
    }
    Shiny.onInputChange(params.controlname, $("#heatmap-heatmap").attr("gname"));
    }
    shinyjs.resetInputParam = function(params){
        var defaultParams = {
                controlname : "hoveredgenename"
        };
        params = shinyjs.getParams(params, defaultParams);
        console.log(params.controlname)
        Shiny.onInputChange(params.controlname, "");
    }

    shinyjs.getSelectedGenes = function(params){
    var defaultParams = {
    plotId : "heatmap",
    controlname : "selgenenames"
    };
    params = shinyjs.getParams(params, defaultParams);
    var count = document.getElementById(params.plotId).querySelectorAll("g.y2tick").length
    var start = 0
    var out = ""
    
    for (i = start; i < count; i++)
    {
        if (typeof document.getElementById(params.plotId).querySelectorAll("g.y2tick")[i] != "undefined"){
        out += document.getElementById(params.plotId).querySelectorAll("g.y2tick")[i].innerHTML.match(">(.*)</text>")[1]  + ","
        }
    }
    Shiny.onInputChange(params.controlname, out);
    }'
}

#' getJSLine
#'
#' heatmap JS code for selection functionality
#'
#' @return JS Code
#' @export
#'
#' @examples
#'     x <- getJSLine()
#'
getJSLine <-function()
{        
    list(shinyjs::useShinyjs(),
         shinyjs::extendShinyjs(text = heatmapJScode(), functions = c("getHoverName", "getSelectedGenes", "resetInputParam")))
}


#' heatmapServer
#'
#' Sets up shinyServer to be able to run heatmapServer interactively.
#'
#' @note \code{heatmapServer}
#' @param input, input params from UI
#' @param output, output params to UI
#' @param session, session variable
#' @return the panel for main plots;
#'
#' @examples
#'     heatmapServer
#'
#' @export


heatmapServer <- function(input, output, session) {
    updata <- reactiveVal()
    selected <- reactiveVal()
    expdata <- reactiveVal()
    observe({
        updata(callModule(debrowserdataload, "load", "Submit"))
    })
    observe({
        if(!is.null(updata()$load()$count))
        if (nrow(updata()$load()$count) > 1000){
            updateCheckboxInput(session, "mostvaried", value = TRUE)
            expdata(getMostVariedList(updata()$load()$count, 
            colnames(updata()$load()$count), input))
        }
        else
            expdata(updata()$load()$count)
    })
    
    observeEvent (input$Submit, {
        updateTabItems(session, "DEBrowserHeatmap", "Heatmap")
    })
    observe({
        if (!is.null(expdata())){
            withProgress(message = 'Creating plot', style = "notification", value = 0.1, {
                selected(callModule(debrowserheatmap, "heatmap", expdata()))
            })
        }
    })
    output$heatmap_hover <- renderPrint({
        if (!is.null(selected()) && !is.null(selected()$shgClicked()) && 
            selected()$shgClicked() != "")
            return(paste0("Clicked: ",selected()$shgClicked()))
        else
            return(paste0("Hovered:", selected()$shg()))
    })
    output$heatmap_selected <- renderPrint({
        if (!is.null(selected()))
            selected()$selGenes()
    })
    output$topn <- renderPrint({
        if (!is.null(input$topn))
            input$topn
    })
    output$mincount <- renderPrint({
        if (!is.null(input$mincount))
            input$mincount
    })
}

#' heatmapUI
#'
#' Creates a shinyUI to be able to run DEBrowser interactively.
#'
#' @param input, input variables
#' @param output, output objects
#' @param session, session
#'
#' @note \code{heatmapUI}
#' @return the panel for heatmapUI;
#'
#' @examples
#'     x<-heatmapUI()
#'
#' @export
#'

heatmapUI <- function(input, output, session) {
    header <- dashboardHeader(
        title = "DEBrowser Heatmap"
    )
    sidebar <- dashboardSidebar(  getJSLine(),  
        sidebarMenu(id="DEBrowserHeatmap",
        menuItem("Upload", tabName = "Upload"),
        menuItem("Heatmap", tabName = "Heatmap"),
        menuItem("Options", tabName = "Heatmap",
        checkboxInput('mostvaried', 'Most Varied Set', value = FALSE),
        conditionalPanel( (condition <- "input.mostvaried"),
        textInput("topn", "top-n", value = "500" ), 
        textInput("mincount", "total min count", value = "10" )),
        plotSizeMarginsUI("heatmap"),
        heatmapControlsUI("heatmap"))))
    
    body <- dashboardBody(
        tabItems(
            tabItem(tabName="Upload", dataLoadUI("load")),
            tabItem(tabName="Heatmap",  getHeatmapUI("heatmap"),
                    column(4,
                        verbatimTextOutput("heatmap_hover"),
                        verbatimTextOutput("heatmap_selected"),
                        verbatimTextOutput("topn"),
                        verbatimTextOutput("mincount")
                    ))
        ))
    
    dashboardPage(header, sidebar, body, skin = "blue")
}
