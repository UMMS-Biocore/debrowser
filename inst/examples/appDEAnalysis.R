library(debrowser)
library(DESeq2)

header <- dashboardHeader(
  title = "DEBrowser DE Analysis"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DEAnalysis",
    menuItem("DEAnalysis", tabName = "DEAnalysis"),
    shinydashboard::menuItem("Filter", tabName = "DEAnalysis",
    cutOffSelectionUI("DEResults"))
))

body <- dashboardBody(
  tabItems(
    tabItem(tabName="DEAnalysis", 
    fluidRow(
        # If these getDEResultsUI line put into the UI. The results shown in a table.
        column(12, getDEResultsUI("DEResults")),
        column(12,
            verbatimTextOutput("DERes")
        )
    ))
))

ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  ######## Prepare demo data to use the module ########
  load(system.file("extdata", "demo", "demodata.Rda",
                   package = "debrowser"))
  filtd <-
      # Filter out the rows that has maximum 100 reads in a sample
      subset(demodata, apply(demodata, 1, max, na.rm = TRUE)  >=  10)
 
  params <-
      #Run DESeq2 with the following parameters
      c("DESeq2", "parametric", FALSE, "Wald", "None")
 
  cols <- 
      #For demo purpose use the coumns below;
      c("exper_rep1", "exper_rep2", "exper_rep3", "control_rep1", "control_rep2", "control_rep3")
  
  conds <- 
      #For each column, there is matching condition defined
      c("exper", "exper", "exper", "control", "control", "control")
  
  ######################################################
  deres<-callModule(debrowserdeanalysis, "DEResults", data = filtd, 
            columns = cols, conds = conds, params = params)
  observe({
      output$DERes <- renderPrint({
          head( deres$dat() )
      })
  })
}

shinyApp(ui, server)
