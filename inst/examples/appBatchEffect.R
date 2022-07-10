library(debrowser)
library(DESeq2)
library(Harman)
options(warn =-1)
source("../../R/batcheffect.R")
source("../../R/funcs.R")
header <- dashboardHeader(
    title = "DEBrowser Batch Effect"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DataPrep",
       menuItem("BatchEffect", tabName = "BatchEffect")))

body <- dashboardBody(
    tabItems(
        tabItem(tabName="BatchEffect", batchEffectUI("batcheffect"),
                column(4,
                       verbatimTextOutput("batcheffecttable")
                )
        )
    ))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    load(system.file("extdata", "demo", "demodata.Rda",
                     package = "debrowser"))
    
    ldata <- reactiveValues(count=NULL, meta=NULL)
    ldata$count <- demodata
    ldata$meta <- metadatatable
    data <- callModule(debrowserbatcheffect, "batcheffect", ldata)
    observe({
        output$batcheffecttable <- renderPrint({
            head( data$BatchEffect()$count )
        })
    })
}

shinyApp(ui, server)
