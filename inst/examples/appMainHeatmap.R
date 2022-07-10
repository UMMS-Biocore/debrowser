library(plotly)
library(debrowser)
library(DESeq2)

header <- dashboardHeader(
    title = "DEBrowser Main Plots"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DEAnalysis",
    menuItem("Main", tabName = "Main"),
    mainPlotControlsUI("main"),
    plotSizeMarginsUI("main",  w=600, h=400),
    heatmapControlsUI("heatmap"),
    plotSizeMarginsUI("heatmap", w=600, h=400)))

body <- dashboardBody(
    tabItems(
        tabItem(tabName="Main",
                fluidRow(column(6,
                getMainPlotUI("main"),
                verbatimTextOutput("main_hover"),
                verbatimTextOutput("main_selected")
                ),
                column(6,
                getHeatmapUI("heatmap"),
                verbatimTextOutput("heatmap_hover"),
                verbatimTextOutput("heatmap_selected")
                ))
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
    selected <- callModule(debrowsermainplot, "main", xdata)
    
    output$main_hover <- renderPrint({
        selected$shgClicked()
    })
    output$main_selected <- renderPrint({
        selected$selGenes()
    })
    selectedHeat <- NULL
    observe({
        if (!is.null(selected$selGenes())) {
        withProgress(message = 'Creating plot', style = "notification", value = 0.1, {
            selectedHeat <- callModule(debrowserheatmap, "heatmap", 
                xdata[selected$selGenes(), dat$columns])
        })
        }
    })
    output$heatmap_hover <- renderPrint({
       if( !is.null(selectedHeat))
            selectedHeat$shgClicked()
    })
    output$heatmap_selected <- renderPrint({
        if( !is.null(selectedHeat))
        selectedHeat$selGenes()
    })
}
shinyApp(ui, server)
