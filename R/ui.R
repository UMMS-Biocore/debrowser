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

    heatmapJScode <-
        "shinyjs.getNames = function(){
        var count = document.getElementsByClassName('tick').length;
        var start = 0;
        while(document.getElementsByClassName('tick')[start].getElementsByTagName('line')[0].getAttribute('x2') == 0){
        start += 1;
        }
        var out = '';
        for (i = start; i < count; i++)
        {
        if('opacity: 1;' == document.getElementsByClassName('tick')[i].getAttribute('style')){
        out += document.getElementsByClassName('tick')[i].getElementsByTagName('text')[0].innerHTML + ',';
        }
        }
        //document.getElementById('genenames').innerHTML = out;
        Shiny.onInputChange('genenames', out);
    };"
    dbHeader <- shinydashboard::dashboardHeader(titleWidth = 350)
    dbHeader$children[[2]]$children <-  tags$a(style='color: white;',
        href = "/" , "DEBrowser")
    
    addResourcePath(prefix = "www", directoryPath = system.file("extdata",
        "www", package = "debrowser"))

    
    if(!file.exists("shiny_saves/startup.rds")){
        startup_obj <- list()
        startup_obj$bookmark_counter <- 3
        startup_obj$startup_bookmark <- ""
        saveRDS(startup_obj, "shiny_saves/startup.rds")
    }        
        
    startup <- readRDS("shiny_saves/startup.rds")
    
    if (startup[['bookmark_counter']] == 0){
        a <- (fluidPage(
            tags$script('Shiny.addCustomMessageHandler("testmessage",
                function(message) {
                    window.location.href = message.bookmarked_url;
                }
        );') ))
    }

    else{
    a <- (fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = heatmapJScode, functions = c("getNames")),
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
    tags$div(h4("Loading DEBrowser"), id = "loading-debrowser",
        tags$img(src = "www/images/initial_loading.gif")),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css",
                  href = "www/shinydashboard_additional.css")
    ),
    #uiOutput("logo"),
    #uiOutput("programtitle"),
    #textOutput("text"),
    #div("UMASS Medical", id = "university-name"),
    shinydashboard::dashboardPage(
        # shinydashboard::dashboardHeader(
            # title = "DEBrowser",
            # titleWidth = 350
            # ),
        dbHeader,
        shinydashboard::dashboardSidebar(
            width = 350,
            
            #sidebarPanel(
            # To enable people to name their state save(bookmark)

            uiOutput("loading"),
            #width = 0,
            
            uiOutput("initialmenu"),
            conditionalPanel(condition = "(output.dataready)",
                uiOutput("leftMenu")),
            conditionalPanel(condition = "(output.dataready)",
                uiOutput("downloadSection")),
            conditionalPanel(condition = "(output.dataready)",
                uiOutput('cutoffSelection')),
            #)
            # ,
            # a("Refresh", href="", style='margin: 27px 27px 27px 27px')
            conditionalPanel(condition = paste0("((input.goDE) || ",
                "(output.restore_DE > 0)) && (!input.startDE)"),
                style = "padding: 27px;",
            actionButton("save_state", "Save State!"),
            conditionalPanel(condition = "input.save_state",
                textInput("bookmark_special_name", "Name your save:",
                    value = "", placeholder = "At Least 5 Characters"),
                    actionButton("name_bookmark", "Submit!"),
                    textOutput("bookmark_length_error"),
                    br(), br(), br()
            )),
            conditionalPanel(condition <- paste0("input.methodtabs=='panel0'"),
                htmlOutput("new_bookmark"),
                htmlOutput("past_named_bookmarks")
            )
        ),
    shinydashboard::dashboardBody(
        
        mainPanel(
            width = 10,
            tags$head(
                tags$style(type = "text/css",
                        "#methodtabs.nav-tabs {font-size: 14px} ")),
            tabsetPanel(id = "methodtabs", type = "tabs",
                tabPanel(title = "Data Prep", value = "panel0", id="panel0",
                        uiOutput("preppanel")),
                tabPanel(title = "Main Plots", value = "panel1", id="panel1",
                        uiOutput("mainmsgs"),
                        conditionalPanel(condition = "input.demo ||
                        output.dataready", uiOutput("mainpanel"))),
                tabPanel(title = "QC Plots", value = "panel2", id="panel2",
                        uiOutput("qcpanel")),
                tabPanel(title = "GO Term", value = "panel3", id="panel3",
                        uiOutput("gopanel")),
                tabPanel(title = "Tables", value = "panel4", id="panel4",
                        DT::dataTableOutput("tables")))
                )
        
            )
            )
        )
    )
    }
    a
}
