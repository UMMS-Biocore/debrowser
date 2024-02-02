#' getLeftMenu
#'
#' Generates the left menu for for plots within the DEBrowser.
#'
#' @param input, input values
#' @note \code{getLeftMenu}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- getLeftMenu()
#' @export
#'
getLeftMenu <- function(input = NULL) {
if (is.null(input)) return(NULL)
   leftMenu <- list(
        conditionalPanel( (condition <- "input.methodtabs=='panel1'"),
            getMainPlotsLeftMenu()),
        conditionalPanel( (condition <- "input.methodtabs=='panel2'"),
        shinydashboard::menuItem(" Plot Type", startExpanded = TRUE,
        wellPanel(radioButtons("qcplot",
                paste("QC Plots:", sep = ""),
                c(PCA = "pca", All2All = "all2all", Heatmap = "heatmap", IQR = "IQR",
                  Density = "Density")))),
            getQCLeftMenu(input)),
        conditionalPanel( (condition <- "input.methodtabs=='panel3'"),
            actionButton("startGO", "Submit"),
        shinydashboard::menuItem(" Plot Type", startExpanded = TRUE,
            wellPanel(radioButtons("goplot", paste("Go Plots:", sep = ""),
                c(enrichGO = "enrichGO", enrichKEGG = "enrichKEGG",
                Disease = "disease", compareClusters = "compare", GSEA = "GSEA")))),
                getGOLeftMenu()
                ),
        conditionalPanel( (condition <- "input.methodtabs=='panel4'"),
        shinydashboard::menuItem(" Select Columns", startExpanded=TRUE,
             uiOutput("getColumnsForTables")
        ))
    )
   return(leftMenu)
}
#' getMainPlotsLeftMenu
#'
#' Generates the Main PLots Left menu to be displayed within the DEBrowser.
#'
#' @note \code{getMainPlotsLeftMenu}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- getMainPlotsLeftMenu()
#' @export
#'
getMainPlotsLeftMenu <- function() {
    mainPlotsLeftMenu <- list(
        plotSizeMarginsUI("main",  w=600, h=400),
        shinydashboard::menuItem("Heatmap Options", startExpanded=FALSE,
        heatmapControlsUI("heatmap"),
        plotSizeMarginsUI("heatmap", w=550, h=400)),
        plotSizeMarginsUI("barmain", w=550,h=400, t=90),
        plotSizeMarginsUI("boxmain", w=550, h=400, t=90)
        )
    return(mainPlotsLeftMenu)
}

#' getGOLeftMenu
#'
#' Generates the GO Left menu to be displayed within the DEBrowser.
#'
#' @note \code{getGOLeftMenu}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- getGOLeftMenu()
#' @export
#'
getGOLeftMenu <- function() {
    list(
    shinydashboard::menuItem(" Go Term Options", startExpanded=TRUE, 
    textInput("gopvalue", "p.adjust", value = "0.01" ),
        getOrganismBox(),
            actionButton("GeneTableButton", "DE Genes"),
            conditionalPanel( (condition <- "input.goplot=='enrichKEGG'"),
                      actionButton("KeggPathway", "KeggPathway")),
            conditionalPanel( ( condition <- "(input.goplot=='enrichGO' ||
            (input.goplot=='compare' && input.gofunc!='enrichDO' &&
            input.gofunc!='enrichKEGG'))" ),
            selectInput("ontology", "Choose an ontology:",
                choices =  c( "CC", "MF", "BP"))
            ),
            conditionalPanel( ( condition <- "input.goplot!='compare'"),
                selectInput("goextplot", "Plot Type:",
                choices =  c("Summary", "Dotplot"))
            ),
            conditionalPanel( ( condition <- "input.goplot=='compare'"),
                selectInput("gofunc", "Plot Function:",
                choices =  c( "enrichGO", "enrichDO", "enrichKEGG"))
            ),
            conditionalPanel( ( condition <- "input.goplot=='GSEA'"),
                selectInput("sortfield", "Sort field:",
                choices =  c( "stat", "log2FoldChange"))
            ),
            downloadButton("downloadGOPlot", "Download Plots"))
    )

}

#' getQCLeftMenu
#'
#' Generates the left menu to be used for QC plots within the
#' DEBrowser.
#'
#' @param input, input values
#' @note \code{getQCLeftMenu}
#' @return QC left menu
#' @examples
#'     x <- getQCLeftMenu()
#' @export
#'
getQCLeftMenu <- function( input = NULL) {
    if (is.null(input)) return(NULL)
        list(
        shinydashboard::menuItem(" Select Columns", startExpanded=TRUE, 
            uiOutput("columnSelForQC")),
            shinydashboard::menuItem(" QC Options", startExpanded=FALSE,
            conditionalPanel( (condition <- "input.qcplot=='heatmap'"),
                plotSizeMarginsUI("heatmapQC"),
                heatmapControlsUI("heatmapQC")),
            conditionalPanel( condition <- "(input.qcplot=='all2all')",
                plotSizeMarginsUI("all2all"),
                all2allControlsUI("all2all")
            ),
            conditionalPanel( condition <- "(input.qcplot=='Density')",
                              plotSizeMarginsUI("density"),
                              plotSizeMarginsUI("normdensity")
            ),
            conditionalPanel( condition <- "(input.qcplot=='IQR')",
                              plotSizeMarginsUI("IQR"),
                              plotSizeMarginsUI("normIQR")
            ),
        getHelpButton("method",
        "http://debrowser.readthedocs.io/en/master/heatmap/heatmap.html"),
        conditionalPanel( (condition <- "input.qcplot=='pca'"),
            shinydashboard::menuItem("PCA Options",
            pcaPlotControlsUI("qcpca")),
            plotSizeMarginsUI("qcpca", w=600, h=400, t=0, b=0, l=0, r=0)
        ))
    )
}

#' getCutOffSelection
#'
#' Gathers the cut off selection for DE analysis
#'
#' @param nc, total number of comparisons
#' @note \code{getCutOffSelection}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- getCutOffSelection()
#' @export
#'
getCutOffSelection <- function(nc = 1){
    compselect <- getCompSelection("compselect", nc)
    list( conditionalPanel( (condition = "input.dataset!='most-varied' &&
        input.methodtabs!='panel0'"),
        shinydashboard::menuItem(" Filter",
        #h4("Filter"),
        textInput("padj", "padj", value = "0.01" ),
        textInput("foldChange", "foldChange", value = "2" ),
        compselect
        )
    ) )
}

#' getMainPanel
#'
#' main panel for volcano, scatter and maplot.  
#' Barplot and box plots are in this page as well.
#'
#' @note \code{getMainPanel}
#' @return the panel for main plots;
#'
#' @examples
#'     x <- getMainPanel()
#'
#' @export
#'
getMainPanel <- function() {
    list(
        fluidRow(column(6,
            getMainPlotUI("main")
        ),
        column(6,
            getHeatmapUI("heatmap")
        )),
        fluidRow(column(6,
            getBarMainPlotUI("barmain")),
        column(6,
            getBoxMainPlotUI("boxmain"))))
}

#' getProgramTitle
#'
#' Generates the title of the program to be displayed within DEBrowser.
#' If it is called in a program, the program title will be hidden
#'
#' @param session, session var
#' @note \code{getProgramTitle}
#' @return program title
#' @examples
#'     title<-getProgramTitle()
#' @export
#'
getProgramTitle <- function(session = NULL) {
    if (is.null(session)) return (NULL)
    DEBrowser <- NULL
    title<-parseQueryString(session$clientData$url_search)$title
    if (is.null(title) || title != "no" )
        DEBrowser <- list(titlePanel("DEBrowser"))
    else
        DEBrowser <- list(titlePanel(" "))
    return(DEBrowser)
}

#' getLoadingMsg
#'
#' Creates and displays the loading message/gif to be displayed
#' within the DEBrowser.
#'
#' @param output, output message
#' @note \code{getLoadingMsg}
#' @return loading msg
#' @examples
#'     x <- getLoadingMsg()
#' @export
#'
getLoadingMsg <- function(output = NULL) {
    addResourcePath(prefix = "www", directoryPath =
        system.file("extdata", "www",
        package = "debrowser"))
    imgsrc_full <- "www/images/loading_start.gif"
    imgsrc_small <- "www/images/loading.gif"
    a <- list(
        tags$head(tags$style(type = "text/css", "
            #loadmessage {
            position: fixed;
            top: 0px;
            left: 0px;
            width: 100%;
            height: 100%;
            padding: 5px 0px 5px 0px;
            text-align: center;
            font-weight: bold;
            font-size: 100%;
            color: #000000;
            opacity: 0.8;
            z-index: 100;
            }
            #loadmessage_small {
            position: fixed;
            left: 50%;
            transform: translateX(-50%);
            top: 50px;
            text-align: center;
            opacity: 0.8;
            z-index: 999999;
            }
                             ")),
        conditionalPanel(condition = paste0("$('html').hasClass('shiny-busy')",
            "& input.startDE & input.methodtabs=='panel0'"),
            tags$div(id = "loadmessage",
            tags$img(src = imgsrc_full
            ))),
        conditionalPanel(condition =  paste0("$('html').hasClass('shiny-busy')",
                "& !(input.startDE & input.methodtabs=='panel0')"),
            tags$div(id = "loadmessage_small",
            tags$img(src = imgsrc_small
            )))
        )
}

#' getLogo
#'
#' Generates and displays the logo to be shown within DEBrowser.
#'
#' @note \code{getLogo}
#' @return return logo
#' @examples
#'     x <- getLogo()
#' @export
#'
getLogo <- function(){
    addResourcePath(prefix = "www", directoryPath =
        system.file("extdata", "www",
        package = "debrowser"))
    imgsrc <- "www/images/logo.png"
    a<-list(img(src=imgsrc, align = "right"))
}

#' getStartupMsg
#'
#' Generates and displays the starting message within DEBrowser.
#'
#' @note \code{getStartupMsg}
#' @return return startup msg
#' @examples
#'     x <- getStartupMsg()
#' @export
#'
getStartupMsg <- function() {
a <- list( column( 12, 
helpText("Please select a file or load the demo data."),
helpText( "For more information;" ),
helpText(   a("Quick Start Guide",
href = "http://debrowser.readthedocs.org",
target = "_blank"),
getHelpButton("method", "http://debrowser.readthedocs.org")) ))
}

#' getAfterLoadMsg
#'
#' Generates and displays the message to be shown after loading data
#' within the DEBrowser.
#'
#' @note \code{getAfterLoadMsg}
#' @return return After Load Msg
#' @examples
#'     x <- getAfterLoadMsg()
#' @export
#'
getAfterLoadMsg <- function() {
a <- list( column( 12, wellPanel(
helpText( "Please choose the appropriate conditions for DESeq analysis
            and press 'Run DESeq' button in the left menu" ),
helpText( "To be able to select conditions please click
            'Condition1' or 'Condition2' boxes.
            You can also use delete button to remove the
            samples from the list."))))
}

#' getStartPlotsMsg
#'
#' Generates and displays the starting messgae to be shown once
#' the user has first seen the main plots page within DEBrowser.
#'
#' @note \code{getStartPlotsMsg}
#' @return return start plot msg
#' @examples
#'     x <- getStartPlotsMsg()
#' @export
#'
getStartPlotsMsg <- function() {
a <- list( conditionalPanel(condition <- "!input.goMain",
    column( 12, 
    helpText( "Please choose the appropriate parameters to discover
               more in DE Results" ),
    getHelpButton("method", "http://debrowser.readthedocs.io/en/master/quickstart/quickstart.html"))))
}

#' getCondMsg
#'
#' Generates and displays the current conditions and their samples
#' within the DEBrowser.
#'
#' @param dc, columns
#' @param input, selected comparison
#' @param cols, columns
#' @param conds, selected conditions
#' @note \code{getCondMsg}
#' @return return conditions
#' @examples
#'     x <- getCondMsg()
#' @export
#'
getCondMsg <- function(dc = NULL, input = NULL, cols = NULL, conds = NULL) {
    if (is.null(cols) || is.null(conds)) return (NULL)
    num <- input$compselect
    if (is.null(num)) num <- 1
    cnd <- data.frame(cbind(conds, cols))
    cond_names <- dc[[as.numeric(num)]]$cond_names
    
    params_str <- paste(dc[[as.numeric(num)]]$demethod_params, collapse = ',')
    heatmap_str <-  paste0( "<b>Heatmap Params: Scaled:</b> ", input[['heatmap-scale']],
        " <b>Centered:</b> ", input[['heatmap-center']],
        " <b>Log:</b> ", input[['heatmap-log']],
        " <b>Pseudo-count:</b> ", input[['heatmap-pseudo']])
    a <-list( conditionalPanel(condition <- "input.goMain",
            shinydashboard::box(
            collapsible = TRUE, title = "Plot Information", status = "primary", 
            solidHeader = TRUE, width = NULL,
            draggable = TRUE,
            style = "overflow-x:scroll",
            HTML( paste0( "<b>DE Params:</b> ", params_str,
            " - <b>Dataset:</b> ", input$dataset," <b>Normalization:</b> ",input$norm_method,
            " - ", heatmap_str,
            "</br><b>", cond_names[1], ":</b> "),
            paste(cnd[cnd$conds ==  unique(conds)[1], "cols"],
            collapse =","),
            paste0(" vs. ","<b>", cond_names[2], ":", "</b> "),
            paste(cnd[cnd$conds == unique(conds)[2], "cols"],
            collapse =",")),
        getHelpButton("method",
"http://debrowser.readthedocs.io/en/master/quickstart/quickstart.html#the-main-plots-of-de-analysis")
)))
}

#' togglePanels
#'
#' User defined toggle to display which panels are to be shown within
#' DEBrowser.
#'
#' @param num, selected panel
#' @param nums, all panels
#' @param session, session info
#' @note \code{togglePanels}
#' @examples
#'     x <- togglePanels()
#' @export
#'
togglePanels <- function(num = NULL, nums = NULL, session = NULL){
    if (is.null(num)) return (NULL)
    for(i in 0:4){
        if (i %in% nums)
            shinyjs::show(selector =
                paste0("#methodtabs li a[data-value=panel",i,"]"))
        else
            shinyjs::hide(selector =
                paste0("#methodtabs li a[data-value=panel",i,"]"))
    }
    if(num)
        updateTabsetPanel(session, "methodtabs",
            selected = paste0("panel", num))
}


#' getTableStyle
#'
#' User defined selection that selects the style of table to display
#' within the DEBrowser.
#'
#' @param dat, dataset
#' @param input, input params
#' @param padj, the name of the padj value column in the dataset
#' @param foldChange, the name of the foldChange column in the dataset
#' @param DEsection, if it is in DESection or not
#' @note \code{getTableStyle}
#' @examples
#'     x <- getTableStyle()
#' @export
#'
getTableStyle <- function(dat = NULL, input = NULL,
    padj = c("padj"), foldChange=c("foldChange"), DEsection = TRUE){
    if (is.null(dat)) return (NULL)
    
    a <- dat
    if(!is.null(padj) && DEsection && all(padj %in% names(dat$x$data) ))
        a <- a %>% formatStyle(
            padj,
            color = styleInterval(c(0, input$padj),
            c('black', "white", "black")),
            backgroundColor = styleInterval(
            input$padj, c('green', 'white'))
        )
    if(!is.null(foldChange) && DEsection && all( foldChange %in% names(dat$x$data) ) )
        a <- a %>%
            formatStyle(
            foldChange,
            color = styleInterval(c(1/as.numeric(input$foldChange),
            as.numeric(input$foldChange)), c('white', 'black', 'white')),
            backgroundColor = styleInterval(
            c(1/as.numeric(input$foldChange),
            as.numeric(input$foldChange)),
            c('blue', 'white', 'red'))
    )
    a
}

#' textareaInput
#'
#' Generates a text area input to be used for gene selection within
#' the DEBrowser.
#'
#' @param id, id of the control
#' @param label, label of the control
#' @param value, initial value
#' @param rows, the # of rows
#' @param cols, the # of  cols
#' @param class, css class
#' @examples
#'     x <- textareaInput("genesetarea", "Gene Set",
#'         "Fgf21", rows = 5, cols = 35)
#' @export
#'
textareaInput <- function(id, label, value, rows=20, cols=35,
    class="form-control"){
    tags$div(
    class="form-group shiny-input-container",
    tags$label('for'=id,label),
    tags$textarea(id=id,class=class,rows=rows,cols=cols,value))
}

#' showObj
#'
#' Displays a shiny object.
#'
#' @param btns, show group of objects with shinyjs
#' @examples
#'     x <- showObj()
#' @export
#'
showObj <- function(btns = NULL) {
    if (is.null(btns)) return (NULL)
    for (btn in seq(1:length(btns)))
        shinyjs::show(btns[btn])
}

#' hideObj
#'
#' Hides a shiny object.
#'
#' @param btns, hide group of objects with shinyjs
#' @examples
#'     x <- hideObj()
#' @export
#'
hideObj <- function(btns = NULL) {
    if (is.null(btns)) return (NULL)
    for (btn in seq(1:length(btns)))
        shinyjs::hide(btns[btn])
}

#' getKEGGModal
#' prepares a helpbutton for to go to a specific site in the documentation
#'
#' @return the info button
#'
#' @examples
#'     x<- getKEGGModal()
#'
#' @export
getKEGGModal<-function(){
    bsModal("modalExample", "KEGG Pathway", "KeggPathway", size = "large",
    div(style = "display:block;overflow-y:auto; overflow-x:auto;",imageOutput("KEGGPlot")))
}

#' getDownloadSection
#'
#' download section button and dataset selection box in the
#' menu for user to download selected data.
#'
#' @param choices, main vs. QC section
#'
#' @note \code{getDownloadSection}
#' @return the panel for download section in the menu;
#'
#' @examples
#'     x<- getDownloadSection()
#'
#' @export
#'
getDownloadSection <- function(choices=NULL) {
    list(conditionalPanel( (condition = "input.methodtabs!='panel0'"),
        shinydashboard::menuItem(" Data Options",                
        selectInput("dataset", "Choose a dataset:",
        choices = choices),
        conditionalPanel( (condition = "input.dataset=='selected'"),
        selectInput("selectedplot", "The plot used in selection:",
        choices = c("Main Plot", "Main Heatmap", "QC Heatmap"))),
        selectInput("norm_method", "Normalization Method:",
        c("none", "MRN", "TMM", "RLE", "upperquartile"), selected = "MRN"),
        downloadButton("downloadData", "Download Data"),
        conditionalPanel(condition = "input.dataset=='most-varied'",
        textInput("topn", "top-n", value = "500" ), 
        textInput("mincount", "total min count", value = "10" )),
        textareaInput("genesetarea","Search", 
        "", rows = 5, cols = 35),
        helpText("Regular expressions can be used\n
        Ex: ^Al => Al.., Al$ => ...al")
    )))
}

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
    qcPanel <- list(
        wellPanel(helpText( HTML("Please select the parameters and press the 
                            submit button in the left menu for the plots. 
                            The default data set is <b>'most-varied'</b> 500 genes 
                            and total min count is 10 in QC plots. Make sure to
                            change the parameters, if you need to look another part of th data. 
                            For example if you need to draw plots for all detected genes after
                            filtering, select <b>'alldetected'</b> in 
                            'Data Options' -> 'Choose Dataset' on the left menu.") ),
                  getHelpButton("method", 
                                "http://debrowser.readthedocs.io/en/master/quickstart/quickstart.html#quality-control-plots")),
        conditionalPanel(condition = "input.qcplot == 'pca'",
                         getPCAPlotUI("qcpca")),    
        conditionalPanel(condition = "(input.qcplot == 'heatmap')",
                         getHeatmapUI("heatmapQC")),
        conditionalPanel(condition = "(input.qcplot == 'IQR')",
                         getIQRPlotUI("IQR"),
                         getIQRPlotUI("normIQR")),
        conditionalPanel(condition = "(input.qcplot == 'Density')",
                         getDensityPlotUI("density"),
                         getDensityPlotUI("normdensity")),
        conditionalPanel(condition = "(input.qcplot == 'all2all')",
                         getAll2AllPlotUI("all2all"))
        )
    return(qcPanel)
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
    if(is.null(data) || is.null(datasetInput)) return(NULL)
    selCols <- NULL
    if (!is.null(input$dataset)){
        selection <- colnames(data)
        if (!is.null(input$col_list))
            selection <- input$col_list

        selection <- selection[selection %in% colnames(data)]
        
        if (!is.null(selection))
            selCols <- data[rownames(datasetInput), selection]
    }
    return(selCols)
}


#' removeExtraCols
#'
#' remove extra columns for QC plots
#'
#' @param dat, selected data
#'
#' @export
#'
#' @examples
#'     removeExtraCols()
#'
#'
removeExtraCols <- function(dat = NULL){
    rcols <- c(names(dat)[grep("^padj", names(dat))], 
               names(dat)[grep("^foldChange", names(dat))],
               names(dat)[grep("^log2FoldChange$", names(dat))],
               names(dat)[grep("^pvalue$", names(dat))],
               names(dat)[grep("^Legend$", names(dat))],
               names(dat)[grep("^Size$", names(dat))],
               names(dat)[grep("^log10padj$", names(dat))],
               names(dat)[grep("^x$", names(dat))],
               names(dat)[grep("^y$", names(dat))],
               names(dat)[grep("^M$", names(dat))],
               names(dat)[grep("^A$", names(dat))],
               names(dat)[grep("^ID$", names(dat))],
               names(dat)[grep("^stat$", names(dat))]
    )
    if (!is.null(rcols))
        dat <- dat[, !(names(dat) %in% rcols)]
    else
        dat
}