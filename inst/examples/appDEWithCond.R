library(debrowser)
library(DESeq2)

header <- dashboardHeader(
  title = "DEBrowser DE Analysis"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DataPrep",
    menuItem("CondSelect", tabName = "CondSelect"),
    menuItem("DEAnalysis", tabName = "DEAnalysis"),
    shinydashboard::menuItem("Filter", tabName = "DEAnalysis", startExpanded = TRUE,
    uiOutput("cutOffUI"),
    uiOutput("compselectUI"))
))

body <- dashboardBody(
  tabItems(
    tabItem(tabName="CondSelect",
        condSelectUI()),
    tabItem(tabName="DEAnalysis",
    uiOutput("deresUI"),
    column(12,
           verbatimTextOutput("dcres")
    ))
))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  load(system.file("extdata", "demo", "demodata.Rda",
                   package = "debrowser"))
  # Filter out the rows that has maximum 10 reads in a sample
  filtd <-
        subset(demodata, apply(demodata, 1, max, na.rm = TRUE)  >=  10)
    
  sel <- debrowsercondselect(input, output, session, demodata, metadatatable)
  dc <- reactiveVal()
  observeEvent(input$startDE, {
      updateTabItems(session, "DataPrep", "DEAnalysis")
      dc(prepDataContainer(filtd, sel$cc(), input))
  })
  output$condReady <- reactive({
      sel$cc()
  })
  outputOptions(output, 'condReady', suspendWhenHidden = FALSE)
  
  output$compselectUI <- renderUI({
      if (!is.null(sel$cc())) return(NULL)
        getCompSelection(sel$cc())
  })

  compsel <- reactive({
      cp <- 1
      if (!is.null(input$compselect))
          cp <- input$compselect
      cp
  })
  output$cutOffUI <- renderUI({
      if (is.null(dc())) return(NULL)
      cutOffSelectionUI(paste0("DEResults", compsel()))
  })  
  output$deresUI <- renderUI({
      if (is.null(dc())) return(NULL)
      column(12, getDEResultsUI(paste0("DEResults",compsel())))
  })
  output$dcres <- renderPrint({
      if (is.null(dc())) return("")
          print(head(sel$cc()))
  })
}

shinyApp(ui, server)
