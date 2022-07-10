library(debrowser)

options(shiny.maxRequestSize = 30*1024^2)

header <- dashboardHeader(
    title = "DEBrowser Upload"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DataPrep",
   menuItem("Upload", tabName = "Upload")))

body <- dashboardBody(
    tabItems(
        tabItem(tabName="Upload", dataLoadUI("load"),
                column(4,
                       verbatimTextOutput("counttable"),
                       verbatimTextOutput("metadatatable")
                )
        )
    ))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    data <- callModule(debrowserdataload, "load", "Filter")

    output$counttable <- renderPrint({
        head( data$load()$count )
    })
    output$metadatatable <- renderPrint({
        head( data$load()$meta)
    })
    
}

shinyApp(ui, server)
