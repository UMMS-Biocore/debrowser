#' getDataPrepPanel
#'
#' Create and show the Condition selection screen to the user
#' within the DEBrowser.
#'
#' @param flag, flag to show the element in the ui
#' @note \code{getDataPrepPanel}
#' @return returns the left menu according to the selected tab;
#' @examples
#'     x <- getDataPrepPanel()
#' @export
#'
getDataPrepPanel <- function(flag = FALSE){
    a <- NULL
    if(flag)
    a<- list(
        conditionalPanel(condition = "input.demo ||
            output.dataready",
        wellPanel(
        uiOutput("sampleSelector"),
        actionButton("goDE", "Go to DE Analysis!"),
        actionButton("goQCplots", "Go to QC plots!"),
        actionButton("resetsamples", "Reset!"),
        conditionalPanel(condition = "(input.goDE) || (output.restore_DE > 0)",
            helpText( "Please add new comparisons for DE analysis!" ),
            uiOutput("conditionSelector"),
            column(12,actionButton("add_btn", "Add New Comparison"),
            actionButton("rm_btn", "Remove"),
            getHelpButton("method", "http://debrowser.readthedocs.io/en/develop/deseq/deseq.html")),
            br(),
            actionButton("startDE", "Submit!"),
            br(),
           tags$style(type='text/css', "#startDE { margin-top: 10px;}")  ))),
        conditionalPanel(condition = "!input.demo &&
            !output.fileUploaded",
            uiOutput("startup"))
    )
    a
}

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
a <- list(
        conditionalPanel( (condition <- "input.methodtabs=='panel1'"),
                          actionButton("startPlots", "Submit!"),
        shinydashboard::menuItem(" Plot Type", icon = icon("star-o"), startExpanded=TRUE,
        radioButtons("mainplot", paste("Main Plots:", sep = ""),
            c(Scatter = "scatter", VolcanoPlot = "volcano",
            MAPlot = "maplot"))
                ),
            getMainPlotsLeftMenu()),
        conditionalPanel( (condition <- "input.methodtabs=='panel2'"),
                          
            conditionalPanel( condition <- "input.qcplot=='heatmap'",
                actionButton("startQCPlot", "Submit!")),
                          
        shinydashboard::menuItem(" Plot Type", icon = icon("star-o"), startExpanded = TRUE,
        wellPanel(radioButtons("qcplot",
                paste("QC Plots:", sep = ""),
                c(PCA = "pca", All2All = "all2all", Heatmap = "heatmap", IQR = "IQR",
                  Density = "Density")))),
            getQCLeftMenu(input)),
        conditionalPanel( (condition <- "input.methodtabs=='panel3'"),
            actionButton("startGO", "Submit!"),
        shinydashboard::menuItem(" Plot Type", icon = icon("star-o"), startExpanded = TRUE,
            wellPanel(radioButtons("goplot", paste("Go Plots:", sep = ""),
                c(enrichGO = "enrichGO", enrichKEGG = "enrichKEGG",
                Disease = "disease", compareClusters = "compare")))),
                getGOLeftMenu()
                ),
        conditionalPanel( (condition <- "input.methodtabs=='panel4'"),
        shinydashboard::menuItem(" Select Columns", icon = icon("star-o"), startExpanded=TRUE,
             uiOutput("getColumnsForTables")
        ))
    )
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
    a <- list(
        shinydashboard::menuItem(" Background Selection", icon = icon("star-o"),
                                 
              sliderInput("backperc", "Background Data(%):",
                          min=10, max=100, value=10, sep = "",
                          animate = FALSE))
             )
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
    a <- list(
    shinydashboard::menuItem(" Go Term Options", icon = icon("star-o"), startExpanded=TRUE, 
                                       
    tags$head(tags$script(HTML(logSliderJScode("gopvalue")))),
    sliderInput("gopvalue", "p.adjust cut off",
        min=0, max=10, value=6, sep = "",
        animate = FALSE),
    textInput("pvaluetxt", "or p.adjust", value = "0.01" ),
        getOrganismBox(),
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
            downloadButton("downloadGOPlot", "Download Plots"))
    )

}

#' getPCselection
#'
#' Generates the PC selection number to be used within DEBrowser.
#'
#' @param num, PC selection number
#' @param xy, x or y coordinate
#' @note \code{getPCselection}
#' @return PC selection for PCA analysis
#' @examples
#'     x <- getPCselection()
#' @export
#'
getPCselection <- function(num = 1, xy = "x" ) {
    numericInput(paste0("pcsel", xy),
        paste0("PC selection[", xy, "]"), num, 1, 6)
}

#' getColorShapeSelection
#'
#' Generates the fill and shape selection boxes for PCA plots.
#' metadata file has to be loaded in this case
#'
#' @param input, input values
#' @return Color and shape selection boxes
#' @examples
#'     x <- getColorShapeSelection()
#' @export
#'
getColorShapeSelection <- function(input = NULL) {
    if (is.null(input)) return (NULL)
    a <- list(selectBatchEffect(input, "color_pca", "Color field"),
           selectBatchEffect(input, "shape_pca", "Shape field"))
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
    a <- list(
        shinydashboard::menuItem(" Select Columns", icon = icon("star-o"), startExpanded=TRUE, 
            uiOutput("columnSelForQC")),
            shinydashboard::menuItem(" QC Options", icon = icon("star-o"), startExpanded=FALSE,
            conditionalPanel( (condition <- "input.qcplot=='heatmap'"),
                 checkboxInput("interactive", "Interactive", value = FALSE)),
            conditionalPanel( (condition <- "(input.qcplot=='all2all' ||
            input.qcplot=='heatmap') && !(input.interactive)"),
            sliderInput("width", "width",
            min = 100, max = 2000, step = 10, value = 700),
            sliderInput("height", "height",
            min = 100, max = 2000, step = 10, value = 500)),
            conditionalPanel( (condition <- "input.qcplot=='all2all'"),
                sliderInput("cex", "corr font size",
                min = 0.1, max = 10,
                step = 0.1, value = 2)),
            conditionalPanel( (condition <- "input.qcplot=='heatmap'"),
                selectInput("clustering_method", "Clustering Method:",
                choices <- c("complete", "ward.D2", "single", "average",
                "mcquitty", "median", "centroid")),
                selectInput("distance_method", "Distance Method:",
                choices <- c("cor", "euclidean", "maximum", "manhattan",
                "canberra", "binary", "minkowski")),
                getHelpButton("method",
                              "http://debrowser.readthedocs.io/en/develop/quickstart/quickstart.html#heat-maps")
            ),
        conditionalPanel( (condition <- "input.qcplot=='pca'"),
            getPCselection(1, "x"),
            getPCselection(2, "y"),
            textInput("pctile", "Top %", value = "0.05" ),
            getTextOnOff(),
            getLegendSelect(),
            getColorShapeSelection(input)
        ),
        downloadButton("downloadPlot", "Download Plot"))
    )
}

#' logSliderJScode
#'
#' Generates the log based slider to be used by the user within
#' DEBrowser.
#'
#' @param slidername, id of the slider
#' @note \code{logSliderJScode}
#' @return returns the slider values in log10 scale
#' @examples
#'     x <- logSliderJScode()
#' @export
#'
logSliderJScode <- function(slidername = NULL){
    if (is.null(slidername)) return (NULL)
    a <- paste0("$(function() {
    setTimeout(function(){
    var vals = [0];
    var powStart = 4;
    var powStop = 0;
    for (i = powStart; i >= powStop; i--) {
    var val = Math.pow(10, -i)/2;
    val = parseFloat(val.toFixed(8));
    vals.push(val);
    var val = Math.pow(10, -i);
    val = parseFloat(val.toFixed(8));
    vals.push(val);
    }
    $('#", slidername,"').data('ionRangeSlider').update({'values':vals})
    }, 4)})")
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
    compselect <- getCompSelection(nc)
    a <- list(
    conditionalPanel( (condition <- "input.dataset!='most-varied' &&
                       input.methodtabs!='panel0'"),
        tags$head(tags$script(HTML(logSliderJScode("padj")))),
        shinydashboard::menuItem(" Filter", icon = icon("star-o"),
            #h4("Filter"),
            sliderInput("padj", "padj value cut off",
                min=0, max=10, value=6, sep = "",
                animate = FALSE),
            textInput("padjtxt", "or padj", value = "0.01" ),
            sliderInput("foldChange", "Fold Change cut off",
                1, 10, 2, step = 0.1),
            textInput("foldChangetxt", "or foldChange", value = "2" ),
            compselect
        )
    ) )
}

#' getInitialMenu
#'
#' Displays the initial menu within DEBrowser.
#'
#' @param input, input from user
#' @param output, output to user
#' @param session, session info
#' @note \code{getInitialMenu}
#' @return returns the initial menu
#' @examples
#'     x <- getInitialMenu()
#' @export
#'
getInitialMenu <- function(input = NULL, output = NULL, session = NULL) {
    if (is.null(input)) return (NULL)
    a<-NULL
    if (is.null(parseQueryString(session$clientData$url_search)$jsonobject))
    {
        a<-list(
            conditionalPanel(condition = "!input.demo &&
                !output.dataready",
                actionLink("demo", "Load Demo!"),
                #getHelpButton("method", "http://debrowser.readthedocs.io/en/develop/quickstart/quickstart.html"),
                fileInput("file1", "Choose TSV File",
                    accept = c("text/tsv",
                        "text/comma-separated-values,text/plain",
                        ".tsv")),
                fileInput("file2", "Choose Meta Data File (Optional)",
                          accept = c("text/tsv",
                                     "text/comma-separated-values,text/plain",
                        ".tsv"))
                ,
                uiOutput("batchEffect"),
                actionButton("gotoanalysis", "Go to Analysis!"))
            )
    }
    a
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
    a<-NULL
    title<-parseQueryString(session$clientData$url_search)$title
    if (is.null(title) || title != "no" )
        a <- list(titlePanel("DEBrowser"))
    else
        a <- list(titlePanel(" "))
    a
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
helpText("Please select a file or load the demo data!"),
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
            and press 'Run DESeq!' button in the left menu" ),
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
a <- list( conditionalPanel(condition <- "!input.startPlots",
    column( 12, 
    helpText( "Please choose the appropriate parameters and
            press submit button to draw the plots!" ),
    getHelpButton("method", "http://debrowser.readthedocs.io/en/develop/quickstart/quickstart.html#the-main-plots"))))
}

#' getCondMsg
#'
#' Generates and displays the current conditions and their samples
#' within the DEBrowser.
#'
#' @param dc, columns
#' @param num, selected comparison
#' @param cols, columns
#' @param conds, selected conditions
#' @note \code{getCondMsg}
#' @return return conditions
#' @examples
#'     x <- getCondMsg()
#' @export
#'
getCondMsg <- function(dc = NULL, num = NULL, cols = NULL, conds = NULL) {
    if (is.null(cols) || is.null(conds)) return (NULL)
    if (is.null(num)) num <- 1
    cnd <- data.frame(cbind(conds, cols))
    params_str <- paste(dc[[as.numeric(num)]]$demethod_params, collapse = ',')
    a <-list( conditionalPanel(condition <- "input.startPlots",
        column( 12, wellPanel(
            style = "overflow-x:scroll",
            HTML( paste0( "<b>Selected Parameters:</b> ", params_str,
            "</br><b>",unique(conds)[1], ":</b> "),
            paste(cnd[cnd$conds == unique(conds)[1], "cols"],
            collapse =","),
            paste0(" vs. ","<b>",unique(conds)[2], ":", "</b> "),
            paste(cnd[cnd$conds == unique(conds)[2], "cols"],
            collapse =",")),
        getHelpButton("method",
"http://debrowser.readthedocs.io/en/develop/quickstart/quickstart.html#the-main-plots")))))
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

#' getCompSelection
#'
#' Gathers the user selected comparison set to be used within the
#' DEBrowser.
#'
#' @param count, comparison count
#' @note \code{getCompSelection}
#' @examples
#'     x <- getCompSelection(count = 2)
#' @export
#'
getCompSelection <- function(count = NULL) {
  a <- NULL
  if (count>1){
        a <- list(selectInput("compselect",
        label = "Choose a comparison:",
        choices = c(1:count) ))
  }
  a
}

#' selectBatchEffect
#'
#' Batch effect column selection
#'
#' @param input, input values
#' @param selectname, name of the select box
#' @param label, label of the select box
#' @note \code{selectBatchEffect}
#' @examples
#'     x <- selectBatchEffect()
#' @export
#'
selectBatchEffect <- function(input = NULL,
    selectname = "batchselect",
    label = "Batch effect correction column") {
    if (is.null(input$file2)) return (NULL)

     metadata <- readMetaData(input)
     lst.choices <- as.list(c("None", colnames(metadata)))
     selectInput(selectname, label = label,
            choices = lst.choices,
            selected = 1)
}

#' readMetaData
#'
#' read metadata file
#'
#' @param input, input values
#' @note \code{readMetaData}
#' @examples
#'     x <- readMetaData()
#' @export
#'
readMetaData <- function(input = NULL) {
if (is.null(input$file2)) return (NULL)

metadata <- read.table(input$file2$datapath, sep = "\t",
                       header = TRUE, row.names = 1)
}

#' getTextOnOff
#'
#' text on PCA plot on and off
#'
#' @note \code{getTextOnOff}
#' @examples
#'     x <- getTextOnOff()
#' @export
#'
getTextOnOff <- function() {
    lst.choices <- as.list(c("On", "Off"))
    selectInput("textonoff", label = "Text On/Off",
                choices = lst.choices,
                selected = "Off")
}

#' getLegendSelect
#'
#' select legend
#'
#' @note \code{getLegendSelect}
#' @examples
#'     x <- getLegendSelect()
#' @export
#'
getLegendSelect <- function() {
    lst.choices <- as.list(c("color", "shape"))
    selectInput("legendSelect", label = "Select legend",
                choices = lst.choices,
                selected = "color")
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
    if(!is.null(padj) && padj != "" && DEsection)
        a <- a %>% formatStyle(
            padj,
            color = styleInterval(c(0, input$padjtxt),
            c('black', "white", "black")),
            backgroundColor = styleInterval(
            input$padjtxt, c('green', 'white'))
        )
    if(!is.null(foldChange) && foldChange != "" && DEsection)
        a <- a %>%
            formatStyle(
            foldChange,
            color = styleInterval(c(1/as.numeric(input$foldChangetxt),
            as.numeric(input$foldChangetxt)), c('white', 'black', 'white')),
            backgroundColor = styleInterval(
            c(1/as.numeric(input$foldChangetxt),
            as.numeric(input$foldChangetxt)),
            c('red', 'white', 'green'))
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

#' Buttons including Action Buttons and Event Buttons
#'
#' Creates an action button whose value is initially zero, and increments by one
#' each time it is pressed.
#'
#' @param inputId Specifies the input slot that will be used to access the
#'   value.
#' @param label The contents of the button--usually a text label, but you could
#'   also use any other HTML, like an image.
#' @param styleclass The Bootstrap styling class of the button--options are
#'   primary, info, success, warning, danger, inverse, link or blank
#' @param size The size of the button--options are large, small, mini
#' @param block Whehter the button should fill the block
#' @param icon Display an icon for the button
#' @param css.class Any additional CSS class one wishes to add to the action
#'   button
#' @param ... Other argument to feed into shiny::actionButton
#'
#' @export
#'
#' @examples
#'     actionButton("goDE", "Go to DE Analysis!")
#'
actionButton <- function(inputId, label, styleclass = "", size = "",
                         block = FALSE, icon = NULL, css.class = "", ...) {
    if (styleclass %in% c("primary", "info", "success", "warning",
                          "danger", "inverse", "link")) {
        btn.css.class <- paste("btn", styleclass, sep = "-")
    } else btn.css.class = ""

    if (size %in% c("large", "small", "mini")) {
        btn.size.class <- paste("btn", size, sep = "-")
    } else btn.size.class = ""

    if (block) {
        btn.block = "btn-block"
    } else btn.block = ""

    if (!is.null(icon)) {
        icon.code <- HTML(paste0("<i class='fa fa-", icon, "'></i>"))
    } else icon.code = ""
    tags$button(id = inputId, type = "button", class = paste("btn action-button",
        btn.css.class, btn.size.class, btn.block, css.class, collapse = " "),
        icon.code, label, ...)
}

#' getHelpButton
#' prepares a helpbutton for to go to a specific site in the documentation
#'
#' @param name, name that are going to come after info
#' @param link, link of the help
#' @return the info button
#'
#' @examples
#'     x<- getHelpButton()
#'
#' @export
getHelpButton<-function(name = NULL, link = NULL){
if (is.null(name)) return(NULL)
btn <- actionButton(paste0("info_",name),"",icon="info",
                  styleclass="info", size="small")

a <- HTML(paste0("<a id=\"info_",name,"\" href=\"",link,"\" target=\"_blank\">",
                 btn,"</a>"))

}
