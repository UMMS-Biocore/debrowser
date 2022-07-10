#' deUI
#'
#' Creates a shinyUI to be able to run DEBrowser interactively.
#'
#' @note \code{deUI}
#' @return the panel for main plots;
#'
#' @examples
#'     x<-deUI()
#'
#' @export
#'

deUI <- function() {
    dbHeader <- shinydashboard::dashboardHeader(titleWidth = 250)
    dbHeader$children[[2]]$children <- tags$a(style='color: white;',
         id="top_logo" , paste0("DEBrowser v",getNamespaceVersion("debrowser")))
    addResourcePath(prefix = "www", directoryPath = system.file("extdata",
        "www", package = "debrowser"))
    library("debrowser")
    debrowser <- (fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS("
        #loading-debrowser {
        position: absolute;
        background: #000000;
        opacity: 0.9;
        z-index: 100;
        left: 0;
        right: 0;
        height: 100%;
        text-align: center;
        color: #EFEFEF;
    }"),
    # Loading message
    tags$div(h4(paste0("Loading DEBrowser v",getNamespaceVersion("debrowser"))), id = "loading-debrowser",
        tags$img(src = "www/images/initial_loading.gif")),
    tags$head(tags$title(paste0("DEBrowser v",getNamespaceVersion("debrowser"))),
        tags$link(rel = "stylesheet", type = "text/css",
        href = "www/shinydashboard_additional.css")
    ),
    dashboardPage(
        dbHeader,
        dashboardSidebar(
            width = 250,
            debrowser::getJSLine(),
                uiOutput("loading"),
                tabsetPanel(id = "menutabs", type = "tabs",
                tabPanel(title = "Data Prep", value = "dataprep", id="dataprep",
                sidebarMenu(id="DataPrep",
                    menuItem("Quick Start Guide", icon = icon("user"),
                             menuSubItem("Introduction", tabName = "Intro"),
                             menuSubItem("Data Assesment", tabName = "assesment"),
                             menuSubItem("Data Preparation", tabName = "preparation"),
                             menuSubItem("DE Anaylsis", tabName = "deanalysis"),
                             menuSubItem("FAQ", tabName ="FAQ")
                    ),
                    menuItem("Upload", icon = icon("upload"), tabName = "Upload"),
                    menuItem("Filter", icon = icon("filter"), tabName = "Filter"),
                    menuItem("BatchEffect",  icon = icon("align-left"), tabName = "BatchEffect"),
                    menuItem("CondSelect",  icon = icon("bars"), tabName = "CondSelect"),
                    menuItem("DEAnalysis", icon = icon("adjust"), tabName = "DEAnalysis"),
                    menuItem("DEFilter",  icon = icon("code"), tabName = "DEAnalysis",  startExpanded = TRUE,
                             uiOutput("cutOffUI"),
                             uiOutput("compselectUI"))
                ),helpText("Developed by ", a("UMMS Biocore.", 
                href="https://www.umassmed.edu/biocore/", target = "_blank"))),
                tabPanel(title = "Discover", value = "discover", id="discover",
                conditionalPanel(condition = "(output.dataready)",
                    conditionalPanel( (condition <- "input.methodtabs=='panel1'"),
                    debrowser::mainPlotControlsUI("main")),
                    uiOutput("downloadSection"),
                    uiOutput('cutoffSelection'),
                    uiOutput("leftMenu"))
                 ))
        ),
    dashboardBody(
        mainPanel(
            width = 12,
            tags$head(
                tags$style(type = "text/css",
                        "#methodtabs.nav-tabs {font-size: 14px} ")),
                tabsetPanel(id = "methodtabs", type = "tabs",
                    tabPanel(title = "Data Prep", value = "panel0", id="panel0",
                             tabItems(
                                 tabItem(tabName="Intro", debrowser::getIntroText()),
                                 tabItem(tabName="assesment", debrowser::getDataAssesmentText()),
                                 tabItem(tabName="preparation", debrowser::getDataPreparationText()),
                                 tabItem(tabName="deanalysis", debrowser::getDEAnalysisText()),
                                 tabItem(tabName="FAQ",  debrowser::getQAText()),
                                 tabItem(tabName="Upload", debrowser::dataLoadUI("load")),
                                 tabItem(tabName="Filter",
                                         conditionalPanel(
                                             (condition <- "input.Filter"),
                                         debrowser::dataLCFUI("lcf"))),
                                 tabItem(tabName="BatchEffect", 
                                         conditionalPanel(
                                             (condition <- "input.Batch"),
                                         debrowser::batchEffectUI("batcheffect"))),
                                 tabItem(tabName="CondSelect", 
                                         conditionalPanel(
                                             (condition <- "input.goDE || input.goDEFromFilter"),
                                        debrowser::condSelectUI())),
                                 tabItem(tabName="DEAnalysis", 
                                         conditionalPanel(
                                             (condition <- "input.goDE || input.goDEFromFilter"),
                                         uiOutput("deresUI")))
                             )),
                    tabPanel(title = "Main Plots", value = "panel1", id="panel1",
                            uiOutput("mainmsgs"),
                            uiOutput("mainpanel")),
                    tabPanel(title = "QC Plots", value = "panel2", id="panel2",
                            uiOutput("qcpanel")),
                    tabPanel(title = "GO Term", value = "panel3", id="panel3",
                            uiOutput("gopanel")),
                    tabPanel(title = "Tables", value = "panel4", id="panel4",
                            DT::dataTableOutput("tables")))
        ),
        debrowser::getTabUpdateJS()
        ))
    )
    )
    debrowser
}
