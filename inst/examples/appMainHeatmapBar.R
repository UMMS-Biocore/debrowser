library(debrowser)
library(plotly)
library(heatmaply)
library(gplots)
library(DESeq2)
source("../../R/mainScatter.R")

header <- dashboardHeader(
    title = "DEBrowser Main Plots"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DEAnalysis",
    menuItem("Main", tabName = "Main"),
    mainPlotControlsUI("main"),
    plotSizeMarginsUI("main",  w=600, h=400),
    textareaInput("genesetarea","Search", 
                  "", rows = 5, cols = 35),
    helpText(HTML("&nbsp;&nbsp;Regular expressions can be used<br>",
    "&nbsp;&nbsp;Ex: ^Al => Al.., Al$ => ...al")),
    heatmapControlsUI("heatmap"),
    plotSizeMarginsUI("heatmap", w=600, h=360),
    plotSizeMarginsUI("barmain", w=600,h=400),
    plotSizeMarginsUI("boxmain", w=600, h=400)))

body <- dashboardBody(
    tabItems(
        tabItem(tabName="Main",
                fluidRow(column(6,
                getMainPlotUI("main")
                ),
                column(6,
                getHeatmapUI("heatmap")
                )),
                fluidRow(column(6,
                getBarMainPlotUI("barmain")),
                column(6,
                getBoxMainPlotUI("boxmain")))
        )
    ))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    #Example usage with demodata
    load(system.file("extdata", "demo", "demodata.Rda",
                     package = "debrowser"))
    dat <-c()
    dat$columns <- c("exper_rep1", "exper_rep2", "exper_rep3",
                 "control_rep1", "control_rep2", "control_rep3")
    dat$conds <- factor( c("Control", "Control", "Control",
                       "Treat", "Treat", "Treat") )
    dat$data <- data.frame(demodata[, dat$columns])
    
    # You can also use your dataset by reading your data from a file like below;
    # The data in this commented out exampele is not supplied but these lines 
    # can give you an idea about how to read the data from a file;
    #
    # data  <- read.table("~/Downloads/shKRAS.tsv", header=T, row.names=1, sep="\t")
    # dat$columns <- c("CNT.2", "CNT.3", "CNT.4", 
    #               "shKRAS_T1", "shKRAS_T2", "shKRAS_T3")
    # dat$conds <- factor( c("Control", "Control", "Control",
    #                        "shKRAS", "shKRAS", "shKRAS") )
    # dat$data <- data.frame(data[, dat$columns])
    #
    xdata <- generateTestData(dat)

    selected <- reactiveVal()
    observe({
        if(input$genesetarea != ""){
            xdata[rownames(getSearchData(xdata, input)), "Legend"] <- "GS"
        }
        selected(callModule(debrowsermainplot, "main", xdata))
    })
    selectedHeat <- reactiveVal()
    observe({
        if (!is.null(selected()) && !is.null(selected()$selGenes())) {
        withProgress(message = 'Creating plot', style = "notification", value = 0.1, {
            selectedHeat(callModule(debrowserheatmap, "heatmap", xdata[selected()$selGenes(), dat$columns]))
        })
        }
    })
    
    getGeneName <- reactive({
        gname <- NULL
        if (!is.null(selected()$shgClicked()))
            gname <- selected()$shgClicked()
        
        if (!is.null(selectedHeat()) && !is.null(selectedHeat()$shgClicked()))
            gname <- selectedHeat()$shgClicked()
        return(gname)
    })
    observe({
        if (!is.null(selected()) && !is.null(selected()$shgClicked()) && selected()$shgClicked()!=""){
            withProgress(message = 'Creating Bar/Box plots', style = "notification", value = 0.1, {
            callModule(debrowserbarmainplot, "barmain", xdata, 
                       dat$columns,
                       dat$conds, getGeneName())
            callModule(debrowserboxmainplot, "boxmain", xdata, 
                       dat$columns,
                       dat$conds, getGeneName())
            })
        }
    })
}
shinyApp(ui, server)
