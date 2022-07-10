library(debrowser)

header <- dashboardHeader(
  title = "DEBrowser Histogram Plots"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DataAssessment",
    menuItem("Histogram", tabName = "Histogram"),
    textInput("maxCutoff", "Max Cutoff", value = "10" ),
    shinydashboard::menuItem("histogram - Options",
    histogramControlsUI("histogram")),
    plotSizeMarginsUI("histogram", w=400, h=300),
    shinydashboard::menuItem("afterFiltering - Options",
    histogramControlsUI("afterFiltering")),
    plotSizeMarginsUI("afterFiltering", w=400, h=300)
))

body <- dashboardBody(
  tabItems(
    #########################################
    ## Introduction tab panel
    tabItem(tabName="Histogram", 
        fluidRow(
        column(5,
        getHistogramUI("histogram")),
        column(5,
        getHistogramUI("afterFiltering"))
        )
    )
  ))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    #load(system.file("extdata", "demo", "demodata.Rda",
     #                package = "debrowser"))
    
    filtd <- reactive({
        # Filter out the rows that has maximum 100 reads in a sample
        #subset(demodata, apply(demodata, 1, max, na.rm = TRUE)  >=  
        #    as.numeric(input$maxCutoff))
        subset(cpmnorm, apply(cpmnorm, 1, max, na.rm = TRUE)  >=  
                      as.numeric(input$maxCutoff))
    })
    observe({
      if(!is.null(filtd())){
          callModule(debrowserhistogram, "histogram", cpmnorm)
          callModule(debrowserhistogram, "afterFiltering", filtd())
      }
    })
}

shinyApp(ui, server)
