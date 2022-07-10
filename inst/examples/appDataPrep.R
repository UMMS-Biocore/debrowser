library(debrowser)
source("../../R/dataLoad.R")

options(shiny.maxRequestSize = 30*1024^2)

header <- dashboardHeader(
  title = "DEBrowser DataPrep"
)
sidebar <- dashboardSidebar(  sidebarMenu(id="DataPrep",
      menuItem("Upload", tabName = "Upload"),
      menuItem("Filter", tabName = "Filter"),
      menuItem("BatchEffect", tabName = "BatchEffect")
      ))

body <- dashboardBody(
  tabItems(
   tabItem(tabName="Upload", dataLoadUI("load"),
           column(4, verbatimTextOutput("loadedtable")
   )),
   tabItem(tabName="Filter",dataLCFUI("lcf"),                
           column(4, verbatimTextOutput("filtertable")
   )),
   tabItem(tabName="BatchEffect", batchEffectUI("batcheffect"),
           column(4, verbatimTextOutput("batcheffecttable")
   ))
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  filtd <- reactiveVal()
  batch <- reactiveVal()
  observe({
    updata <- reactive({ 
        ret <- callModule(debrowserdataload, "load", "Filter")
        ret
    })
    observeEvent (input$Filter, {
        if(!is.null(updata()$load())){ 
            updateTabItems(session, "DataPrep", "Filter")
            filtd(callModule(debrowserlowcountfilter, "lcf", updata()$load()))
        }
    })
    observeEvent (input$Batch, {
        if(!is.null(filtd()$filter())){ 
            updateTabItems(session, "DataPrep", "BatchEffect")
            batch(callModule(debrowserbatcheffect, "batcheffect", filtd()$filter()))
        }
    })
    
    output$loadedtable <- renderPrint({
         head( updata()$load()$count )
    })
    output$filtertable <- renderPrint({
         head( filtd()$filter()$count  )
    })
    output$batcheffecttable <- renderPrint({
        head( batch()$BatchEffect()$count )
    })
  })
}

shinyApp(ui, server)
