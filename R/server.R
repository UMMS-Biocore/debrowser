#' deServer
#'
#' Sets up shinyServer to be able to run DEBrowser interactively.
#'
#' @note \code{deServer}
#' @param input, input params from UI
#' @param output, output params to UI
#' @param session, session variable
#' @return the panel for main plots;
#'
#' @examples
#'     deServer
#'
#' @export
#' @importFrom shiny  actionButton  actionLink  addResourcePath  column 
#'             conditionalPanel  downloadButton  downloadHandler 
#'             eventReactive  fileInput  fluidPage  helpText  isolate 
#'             mainPanel  need  numericInput  observe  observeEvent 
#'             outputOptions  parseQueryString  plotOutput  radioButtons 
#'             reactive  reactiveValues  renderPlot  renderUI  runApp 
#'             selectInput  shinyApp  shinyServer  shinyUI  sidebarLayout 
#'             sidebarPanel  sliderInput  stopApp  tabPanel  tabsetPanel 
#'             textInput  textOutput  titlePanel  uiOutput tags HTML
#'             h4 img icon updateTabsetPanel  updateTextInput  validate 
#'             wellPanel checkboxInput br checkboxGroupInput onRestore
#'             reactiveValuesToList renderText onBookmark onBookmarked 
#'             updateQueryString           
#' @importFrom shinyjs show hide enable disable useShinyjs extendShinyjs
#'             js inlineCSS
#' @importFrom d3heatmap d3heatmap renderD3heatmap d3heatmapOutput
#' @importFrom DT datatable dataTableOutput renderDataTable formatStyle
#'             styleInterval formatRound
#' @importFrom ggplot2 aes aes_string geom_bar geom_point ggplot
#'             labs scale_x_discrete scale_y_discrete ylab
#'             autoplot
#' @importFrom ggvis add_axis add_legend add_tooltip axis_props
#'             bind_shiny create_broker ggvis ggvisOutput handle_brush
#'             hide_legend layer_bars layer_boxplots layer_points
#'             scale_nominal set_options %>% group_by layer_rects
#'             band scale_numeric hide_axis layer_densities scale_ordinal
#'             layer_text
#' @importFrom gplots heatmap.2 redblue
#' @importFrom igraph layout.kamada.kawai  
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics barplot hist pairs par rect text plot
#' @importFrom stats aggregate as.dist cor cor.test dist
#'             hclust kmeans na.omit prcomp var sd model.matrix
#'             p.adjust runif cov mahalanobis quantile
#' @importFrom utils read.csv read.table write.table update.packages
#'             download.file
#' @importFrom DOSE enrichDO enrichMap gseaplot dotplot
#' @importMethodsFrom AnnotationDbi as.data.frame as.list colnames
#'             head mappedkeys ncol nrow subset keys mapIds
#' @importMethodsFrom GenomicRanges as.factor
#' @importMethodsFrom IRanges as.matrix "colnames<-" mean
#'             nchar paste rownames toupper unique which
#'             as.matrix lapply rev "rownames<-"
#' @importMethodsFrom S4Vectors t grepl
#' @importMethodsFrom SummarizedExperiment cbind order
#' @importFrom jsonlite fromJSON
#' @importFrom methods new
#' @importFrom stringi stri_rand_strings
#' @importFrom annotate geneSymbols
#' @importFrom reshape2 melt
#' @importFrom baySeq getLibsizes getLikelihoods getLikelihoods.NB
#'             getPriors getPriors.NB nbinomDensity
#' @importMethodsFrom baySeq "densityFunction<-" "libsizes<-"
#' @importFrom clusterProfiler compareCluster enrichKEGG dotplot 
#' @importFrom DESeq2 DESeq DESeqDataSetFromMatrix results
#' @importFrom edgeR calcNormFactors equalizeLibSizes DGEList glmLRT
#'             exactTest estimateCommonDisp glmFit
#' @importFrom limma lmFit voom eBayes topTable
#' @importFrom sva ComBat
#' @import org.Hs.eg.db
#' @import org.Mm.eg.db
#' @import V8
#' @import shinydashboard
#' @import devtools

deServer <- function(input, output, session) {
    enableBookmarking("server")
    tryCatch(
    {
        debrowser::loadpacks()
        if (!interactive()) {
            options( shiny.maxRequestSize = 30 * 1024 ^ 2,
                    shiny.fullstacktrace = FALSE, shiny.trace=FALSE, 
                     shiny.autoreload=TRUE)
            debrowser::loadpack(debrowser)
        }
    shinyjs::hide("dropdown-toggle")
    shinyjs::js$setButtonHref()
    shinyjs::js$hideDropdown()
    if(exists(".startdebrowser.called")){
        shinyjs::hide("logout")
    }
    
    options("googleAuthR.webapp.client_id" = "186441708690-n65idoo8t19ghi7ieopat6mlqkht9jts.apps.googleusercontent.com")
    options("googleAuthR.webapp.client_secret" = "ulK-sj8bhvduC9kLU4VQl5ih")
    options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                            "https://www.googleapis.com/auth/userinfo.profile"))
    
    access_token <- callModule(googleAuthR::googleAuth, "initial_google_button")
    ## to use in a shiny app:
    user_details <- reactive({
        if(!is.null(access_token())){
            googleAuthR::with_shiny(get_user_info, shiny_access_token = access_token())
        }
    })
    
    output$user_name <- renderText({
        if(exists(".startdebrowser.called")){
            return("local")
        }
        loadingJSON$username
    })
    

        # username_to_add <- parseQueryString(session$clientData$url_search)[['username']]
        # if(username_to_add != ""){
        #     username_to_add <- paste0("?username=", username_to_add)
        # }
        # output$user_addition <- renderUI({
        #     tags$a(style='color: white;',
        #            href = paste0("/", username_to_add) , "DEBrowser")
        # })
        
        uiState <- reactiveValues()
        uiState$readyFlag <- 0
        uiState$readyCheck <- 0
        
        remove_bookmark <- function(ID){
            print("remove bookmark")
            if(!is.null(loadingJSON$username) && loadingJSON$username != ""){
                saves_path <- paste0("shiny_saves/", loadingJSON$username, "/past_saves.txt")
            } else {
                saves_path <- "shiny_saves/past_saves.txt"
            }
            
            current_file <- readLines(saves_path)
            my_new_file = current_file[-ID]
            to_unlink <- paste0("shiny_bookmarks/", current_file[ID])
            unlink(to_unlink, recursive = TRUE)
            #my_new_file = current_file[-grep(s,readLines(paste0("shiny_saves/", loadingJSON$username, "/past_saves.txt")))]
            fileConn<-file(saves_path)
            writeLines(my_new_file, fileConn)
            close(fileConn)
        }
        lapply(1:20, function(i) {
            shinyjs::onclick(paste0("remove_bm", i),
                 list(
                 remove_bookmark(i),
                 shinyjs::hide(paste0("remove_bm", i)),
                 shinyjs::hide(paste0("bookmark", i))
                 )
            )
        })
        
        bookmark_list <- reactive({
        if(!is.null(loadingJSON$username) && loadingJSON$username != ""){
            past_saves_path <- paste0("shiny_saves/", loadingJSON$username, "/past_saves.txt")

        } else {
            past_saves_path <- "shiny_saves/past_saves.txt"
        }
            
        if(file.exists(past_saves_path)){
            if(file.size(past_saves_path) > 0){
                query <- parseQueryString(session$clientData$url_search)
                json_addition <- ""
                if(!is.null(query$jsonobject)){
                    json_addition <- "&jsonobject=saved"
                }
                
                conn <- file(past_saves_path,open="r")
                lines <- readLines(conn)
                bookmark_count <- length(lines)
                output$past_named_bookmarks <- renderText({"History:"})
                
                lapply(1:bookmark_count, function(i) {
                    a <- strsplit(lines[i], "0u0")
                    if(length(a[[1]]) == 2){
                        to_show <- a[[1]][2]
                        user_addition <- paste0("&username=", a[[1]][1])
                    } else{
                        to_show <- lines[i]
                        user_addition <- ""
                    }
                    
                    output[[paste0('bookmark', i)]] <- renderUI({
                        list(
                            a(paste0(to_show), class="bm_id",
                                href= paste0("?_state_id_=", lines[i],
                                    user_addition, json_addition)),
                            a(href="#", id=paste0("remove_bm", i), 
                                class="removebm",
                                img(src="www/images/delete_button.png"),
                                onclick=paste0('remove_bookmark("',  
                                    lines[i], '")'))
                        )
                    })
                })
                close(conn)
            }
        } else {
            output$past_named_bookmarks <- renderUI({list()})
        }
        })
        
        # To hide the panels from 1 to 4 and only show Data Prep
        togglePanels(0, c(0), session)
        
        ###############################################################
        #     Helper to copy the bookmark to a user named directory   #
        ###############################################################
        get_state_id <- function(prev_url){
            query_string <- paste0("?", strsplit(prev_url, "?",
                fixed = TRUE)[[1]][2])
            query_list <- parseQueryString(query_string)
            return(query_list[["_state_id_"]])
        }
        
        ###############################################################
        # Removing the directory of previous state that is bookmarked #
        ###############################################################
        delete_previous_bookmark <- function(prev_url){
            query_string <- paste0("?", strsplit(prev_url, "?",
               fixed = TRUE)[[1]][2])
            query_list <- parseQueryString(query_string)
            state_id <- query_list[["_state_id_"]]
            cat(paste0("previous url: ", state_id, "\n"))
            
            if(!is.null(state_id)){
                cat(paste0("state id = = ", state_id))
                # Get the state id from the query string
                bookmark_dir <- "shiny_bookmarks/"
                
                file_remove_cmd <- paste0('dir="', bookmark_dir, state_id, '"
                   if [ -d "$dir" ]
                   then
                       rm -R $dir ', '
                   else
                       echo "$dir not found or folder needs a new name."
                   fi')
                cat( paste0( system( file_remove_cmd ), "\n") )
            }
        }
        
        ###############################################################
        #           Bookmark on every single user input               #
        ###############################################################
        observe({
            startup_path <- "shiny_saves/startup.rds"
            if(!is.null(loadingJSON$username) && loadingJSON$username != ""){
                    startup_path <- paste0("shiny_saves/", 
                        loadingJSON$username ,"/startup.rds")
            }
            if(file.exists(startup_path)){
                startup <- readRDS(startup_path)
            } else {
                startup <- list()
            }
            if(is.null(startup[['bookmark_counter']])){
                startup[['bookmark_counter']] <- 3
            }
            
            if(startup[['bookmark_counter']] == 0){
                startup[['bookmark_counter']] <- 1
                saveRDS(startup, startup_path)
                
                session$sendCustomMessage(type = 'testmessage',
                    message = list(new_url = paste0("?_state_id_=",
                    startup[['startup_bookmark']]), controller = input$controller))
            }
            bookmark_list()
        })
        
        ###############################################################
        #         To save user chosen name as bookmark id             #
        ###############################################################
        observeEvent(input$name_bookmark, {
            
            session$doBookmark()
            
            chosen_name <- input$bookmark_special_name
            if(nchar(chosen_name) < 5){
                to_display <- "You must type in at least 5 characters."
            } else if(!grepl('^[A-Za-z0-9]+$', chosen_name)){
                to_display <- "You can only use numbers and English letters."
            } else if(grepl("0u0", chosen_name)){
                to_display <- "You cannot use '0u0' in the name."
            } else{
                output$bookmark_length_error <- renderText({""})
                result <- copy_to_new_directory(chosen_name)
                if(result == 35){
                    to_display <- paste0(chosen_name, " is already saved.")
                } else {
                    if (result == 42) {
                        shinyjs::hide("bookmark_special_name")
                        shinyjs::hide("name_bookmark")
                        #shinyjs::hide("message_for_loading")
                        user_addition <- ""
                        if(!is.null(loadingJSON$username) && (loadingJSON$username != "")){
                            user_addition <- paste0("&username=", loadingJSON$username)
                        }
                        query_list <- parseQueryString(session$clientData$url_search)
                        chosen_link <- chosen_name
                        if(!is.null(loadingJSON$username) && (loadingJSON$username != "")){
                            chosen_link <- paste0(loadingJSON$username, "0u0",
                                                  chosen_name)
                        }
                        
                        old_bookmark_id <- parseQueryString(session$clientData$url_search)[["_state_id_"]]
                        old_json_path <- paste0("shiny_bookmarks/", old_bookmark_id, "/file1.JSON")
                        if(file.exists(old_json_path)){
                            file.copy(old_json_path, paste0("shiny_bookmarks/", chosen_link, "/file1.JSON"))
                        }
                        old_tsv_path <- paste0("shiny_bookmarks/",
                                               old_bookmark_id, "/file1.tsv")
                        if(file.exists(old_tsv_path)){
                            file.copy(old_tsv_path, paste0("shiny_bookmarks/", 
                                chosen_link, "/file1.tsv"), overwrite = TRUE)
                        }    

                        query <- parseQueryString(session$clientData$url_search)
                        json_addition <- ""
                        if(!is.null(query$jsonobject)){
                            json_addition <- "&jsonobject=saved"
                        }
                        
                        bm_link <- paste0('<p style="margin-left: 27px;">New Save:</p><a style="margin: 27px;" ',
                                          ' href="?_state_id_=',
                            chosen_link, user_addition, 
                            json_addition, '">', chosen_name, '</a>')

                        output$new_bookmark <- renderText({bm_link})
                        shinyjs::show("save_state")
                        to_display <- paste0("Successfully saved. ",
                            "URL updated with your choice to access later.")
                    } else {
                        to_display <- "Something went wrong with the save."
                    }
                }
            }
            output$bookmark_length_error <- renderText({ to_display })
        })
        
        #####################################################################
        #     To copy the bookmarked folder into a user named directory     #
        #####################################################################
        copy_to_new_directory <- function(new_state_id){
            query_list <- parseQueryString(session$clientData$url_search)
            if(!is.null(loadingJSON$username) && (loadingJSON$username != "")){
                new_state_id <- paste0(loadingJSON$username, "0u0",
                                       new_state_id)
            }
            
            # Get the state id from the query string
            bookmark_dir <- "shiny_bookmarks/"
            old_state_id <- system(paste0("ls -t1 shiny_bookmarks",
                                          " |  head -n 1"), intern=TRUE)
            
            if(!dir.exists(paste0(bookmark_dir, new_state_id))){
                
                if(file.rename(paste0(bookmark_dir, old_state_id), 
                               paste0(bookmark_dir, new_state_id))){
                    
                    if(!is.null(query_list$jsonobject)){
                        download.file(query_list$jsonobject, paste0(bookmark_dir,
                                                                    new_state_id, "/file1.JSON"))
                    }
                    user_addition <- ""
                    if(!is.null(loadingJSON$username) && (loadingJSON$username != "")){

                        user_addition <- paste0("&username=", loadingJSON$username)
                    }
                    updateQueryString(paste0("?_state_id_=", new_state_id, user_addition))

                    startup_path <- "shiny_saves/startup.rds"
                    if(!is.null(loadingJSON$username) && loadingJSON$username != ""){
                            startup_path <- paste0("shiny_saves/", 
                                loadingJSON$username ,"/startup.rds")
                    }
                    startup <- readRDS(startup_path)
                    startup[['startup_bookmark']] <- new_state_id
                    saveRDS(startup, startup_path)
                    
                    if(!is.null(loadingJSON$username) && loadingJSON$username != ""){
                        f_path <- paste0("shiny_saves/", loadingJSON$username, "/past_saves.txt")
                    } else {
                        f_path <- "shiny_saves/past_saves.txt"
                    }
                    write(new_state_id,file=f_path,
                          append=TRUE)
                    return(42)
                }
                else{
                    return(13)
                }
                
            } else {
                return(35)
            }
        }
        
        # Save extra values in state$values when we bookmark...
        onBookmark(function(state) {
            cat("on bookmark", "\n")
            
            # state$values can store data onBookmark to be restored later
            state$values$input_save <- input
    
            storeDataset <- function(){
                state$values$data <- Dataset()
            }
            try(storeDataset())
            
            state$values$nc <- choicecounter$nc
            
            state$values$samples <- input$samples
            
            cat(paste0("+++++++++++++++++++++++++++++++++++++++++++++++++++++",
                       " nc ", choicecounter$nc, " qc ", choicecounter$qc, "\n"))
            
            # Delete the previously bookmarked state
            # Note that onBookmark precedes onBookmarked
            
            if(!is.null(input$file2)){
                cat("input$file2$datapath: ", input$file2$datapath,
                    " --------------++++++ \n")
            }
        })
        
        onBookmarked(function(url) {
            user_addition <- ""
            if(!is.null(loadingJSON$username) && (loadingJSON$username != "")){
                user_addition <- paste0("&username=", loadingJSON$username)
            }
            updateQueryString(paste0(url, user_addition))
            cat(paste0("updating the url to: ", url, "\n"))

            startup_path <- "shiny_saves/startup.rds"
            if(!is.null(loadingJSON$username) && loadingJSON$username != ""){
                    startup_path <- paste0("shiny_saves/", 
                        loadingJSON$username ,"/startup.rds")
            }
            
            if(file.exists(startup_path)){
                startup <- readRDS(startup_path)
            } else {
                startup <- list()
            }
            if(!file.exists("shiny_saves")){
                dir.create("shiny_saves")
            }
            shiny_saves_dir <- paste0("shiny_saves/", loadingJSON$username)
            if(!file.exists(shiny_saves_dir)){
                dir.create(shiny_saves_dir)
            }
                
            
            startup[['startup_bookmark']] <- get_state_id(url)
            saveRDS(startup, startup_path)
            
            bookmark_dir_id <- get_state_id(url)
            file.copy(isolate(input$file1$datapath), 
                      paste0("shiny_bookmarks/", bookmark_dir_id, "/file1.tsv"))
        })
        
        
        # Read values from state$values when we restore
        onRestore(function(state) {
            log_out <- parseQueryString(session$clientData$url_search)[['logout']]
            if(!is.null(log_out) && (log_out != "")){
                shinyStore::updateStore(session, "text",
                                        isolate(""))
            } else {
            
            shinyjs::js$showDropdown()
            json_obj <- parseQueryString(session$clientData$url_search)[['jsonobject']]
            # coming from json
            if(!is.null(json_obj) && (json_obj != "")){
                loadingJSON$username <- parseQueryString(session$clientData$url_search)[['username']]
                shinyStore::updateStore(session, "text",
                                        isolate(loadingJSON$username))
            } else{
                user_email <- user_details()$emails$value
                username_from_email <- gsub("[[:punct:]]", "", user_email)
                # just logged in via google
                if(!is.null(user_email) && (username_from_email != "")){
                    loadingJSON$username <- username_from_email
                    shinyStore::updateStore(session, "text",
                                            isolate(loadingJSON$username))
                    
                } else{
                    # Check local storage
                    if(!is.null(input$store$text) && (input$store$text != "")){
                        start_state <- parseQueryString(session$clientData$url_search)[['start']]
                        if(!is.null(start_state) && (start_state != "") &&
                           start_state == "true"){
                            loadingJSON$username <- input$store$text
                        }
                        else{
                            state_id_current <- parseQueryString(session$clientData$url_search)[['_state_id_']]
                            if(!is.null(state_id_current) && (state_id_current != "") &
                               !grepl(input$store$text, state_id_current)){
                                # Someone is trying to restore someone else's bookmark
                            } else{
                                # Own bookmark
                                loadingJSON$username <- input$store$text
                            }
                        }
                    }
                }
            }
            

            cat(paste0("RESTORE++++++++++++++++++++++++++++++++++++++++++++++",
                       " nc ", choicecounter$nc, "qc ", choicecounter$qc, "\n"))
            query_list <- parseQueryString(session$clientData$url_search)
            
            
            # username_from_url <- parseQueryString(session$clientData$url_search)$username
            # bookmark_from_url <- parseQueryString(session$clientData$url_search)[['_state_id_']]
            # if(!is.null(username_from_url) && (username_from_url != "")){
            #     if(!is.null(bookmark_from_url) && (bookmark_from_url != "")
            #        && file.exists(paste0("shiny_bookmarks/", bookmark_from_url)) ){
            #         loadingJSON$username <- username_from_url
            #     }
            # }
            dir_to_create <- paste0("shiny_saves/", loadingJSON$username)
            if(!file.exists(dir_to_create)){
                dir.create(dir_to_create)
            }
            

            startup_path <- "shiny_saves/startup.rds"
            if(!is.null(loadingJSON$username) && loadingJSON$username != ""){
                    startup_path <- paste0("shiny_saves/", 
                        loadingJSON$username ,"/startup.rds")
            }
            startup <- list()
            startup[['bookmark_counter']] <- 2
            startup[['startup_bookmark']] <- query_list[["_state_id_"]]
            
            if(!is.null(query_list[["_state_id_"]])){
                saveRDS(startup, startup_path)
                saveRDS(state$values$input_save, paste0("shiny_bookmarks/", 
                    query_list[["_state_id_"]] , "/input_save.rds"))
                
                if(!is.null(state$values$data)){
                    cat("The file is uploaded, go to the next tab.", "\n")
                    buttonValues$gotoanalysis <- TRUE
                }
                if(!is.null(state$values$nc)){
                    choicecounter$nc <- state$values$nc
                }
                
                if(choicecounter$nc > 0){
                    shinyjs::enable("startDE")
                }
                cat("url_search", session$clientData$url_search, "\n")
            }}

            cat("Restoring ", "\n")
        })
        
        onRestored(function(state) {
            cat("here")
        })
            observeEvent(input$stopApp, {
                stopApp(returnValue = invisible())
            })
            # observeEvent(input$bookmark_before_startDE, {
            #     output$bookmark_saved_output <- renderText({ 
            #         "Save Successful! URL updated."
            #     })
            # })
            output$programtitle <- renderUI({
                togglePanels(0, c(0), session)
                getProgramTitle(session)
            })
            output$mainpanel <- renderUI({
                a <- NULL
                if (!is.null(randstr()))
                    a <- getMainPanel(randstr())
                a
            })
            output$qcpanel <- renderUI({
                getQCPanel(input)
            })
            output$gopanel <- renderUI({
                getGoPanel(!is.null(init_data()))
            })
            output$cutoffSelection <- renderUI({
                nc <- 1
                if (!is.null(choicecounter$nc)) nc <- choicecounter$nc
                getCutOffSelection(nc)
            })
            output$downloadSection <- renderUI({
                choices <- c("most-varied", "alldetected", "pcaset")
                if (buttonValues$startDE)
                    choices <- c("up+down", "up", "down",
                                 "comparisons", "alldetected",
                                 "most-varied", "pcaset")
                if (!is.null(selected$data))
                    if (!is.null(selected$data$getSelected())
                        && nrow(selected$data$getSelected())>1)
                        choices <- c(choices, "selected")
                        a <- getDownloadSection(TRUE, choices)
                    a
            })
            output$preppanel <- renderUI({
                getDataPrepPanel(!is.null(init_data))
            })
            output$leftMenu  <- renderUI({
                getLeftMenu(input)
            })
            output$initialmenu <-renderUI({
                getInitialMenu(input, output, session)
            })
            output$loading <- renderUI({
                getLoadingMsg()
            })
            output$logo <- renderUI({
                getLogo()
            })
            output$startup <- renderUI({
                getStartupMsg()
            })
            output$afterload <- renderUI({
                getAfterLoadMsg()
            })
            output$mainmsgs <- renderUI({
                if (is.null(condmsg$text))
                    getStartPlotsMsg()
                else
                    condmsg$text 
            })
            
            buttonValues <- reactiveValues(goQCplots = FALSE, goDE = FALSE,
                startDE = FALSE, gotoanalysis = FALSE)
            
            loadingJSON <- reactiveValues(username = "")
            output$isRestoring <- reactive({
                startup_path <- "shiny_saves/startup.rds"
                if(!is.null(loadingJSON$username)){
                    if(loadingJSON$username != ""){
                        startup_path <- paste0("shiny_saves/", 
                                               loadingJSON$username ,"/startup.rds")
                    }
                }
                startup <- readRDS(startup_path)
                return(startup[['bookmark_counter']] == 2)
            })
            
            output$dataready <- reactive({
                query <- parseQueryString(session$clientData$url_search)
                jsonobj<-query$jsonobject
                if (is.null(jsonobj))
                    hide(id = "loading-debrowser", anim = TRUE, animType = "fade")  
                return(!is.null(Dataset()))
            })
            
            outputOptions(output, "dataready", 
                          suspendWhenHidden = FALSE)
            output$definished <- reactive({
                return(!is.null(filt_data()))
            })
            outputOptions(output, "definished", 
                          suspendWhenHidden = FALSE)
            
            observeEvent(input$gotoanalysis, {
                buttonValues$gotoanalysis <- TRUE
            })
            Dataset <- reactive({
                a <- NULL
                query <- parseQueryString(session$clientData$url_search)
                jsonobj<-query$jsonobject
                if ( buttonValues$gotoanalysis == TRUE || (!is.null(input$demo) && 
                                                           input$demo == TRUE) || !is.null(jsonobj) ){
                    a <- load_data(input, session)
                    if (!is.null(input$batchselect) && input$batchselect!="None")
                    {
                        a<-correctBatchEffect(a, input)
                    }
                }
                if (!is.null(jsonobj))
                    hide(id = "loading-debrowser", anim = TRUE, animType = "fade")  
                a
            })
            choicecounter <- reactiveValues(nc = 0, qc = 0, 
                lastselecteddataset = "")
            observeEvent(input$add_btn, {
                shinyjs::enable("startDE")
                buttonValues$startDE <- FALSE
                choicecounter$nc <- choicecounter$nc + 1}
            )
            observeEvent(input$rm_btn, {
                buttonValues$startDE <- FALSE
                if (choicecounter$nc > 0) 
                    choicecounter$nc <- choicecounter$nc - 1
                if (choicecounter$nc == 0) 
                    shinyjs::disable("startDE")
            })
            observeEvent(input$goDE, {
                shinyjs::disable("startDE")
                hideObj(c("goQCplots", "goDE"))
                showObj(c("add_btn","rm_btn","startDE", "fittype"))
                query <- parseQueryString(session$clientData$url_search)
                # if(!is.null(query$jsonobject)){
                #     session$doBookmark()
                # }
            })
            observeEvent(input$resetsamples, {
                buttonValues$startDE <- FALSE
                showObj(c("goQCplots", "goDE"))
                hideObj(c("add_btn","rm_btn","startDE"))
                choicecounter$nc <- 0
            })
            samples <- reactive({
                if (is.null(Dataset())) return(NULL)
                getSamples(colnames(Dataset()), index = 2)
            })
            output$restore_DE <- reactive({
                choicecounter$nc
            })
            outputOptions(output, 'restore_DE', suspendWhenHidden = FALSE)
            
            
            output$sampleSelector <- renderUI({
                if (is.null(samples())) return(NULL)
                if (is.null(input$samples))
                    samp <- samples()
                else
                    samp <- input$samples
                a <- list(
                    selectInput("samples",
                                label = "Samples",
                                choices = samp, multiple = TRUE,
                                selected = samp)
                )
            })
            output$batchEffect <- renderUI({
                if(!is.null(input$file2)){
                    selectBatchEffect(input)
                }
            })
            output$conditionSelector <- renderUI({
                selectConditions(Dataset(), choicecounter, input, loadingJSON)
            })
            dc <- reactive({
                dc <- NULL
                if (buttonValues$startDE == TRUE){
                    dc <- prepDataContainer(Dataset(), choicecounter$nc, 
                         isolate(input))
                }
                dc
            })
            # observeEvent(input$bookmark_before_startDE, {
            #     session$doBookmark()
            # })
            
            observeEvent(input$save_state, {
                shinyjs::hide("save_state")
                shinyjs::show("bookmark_special_name")
                shinyjs::show("name_bookmark")
            })
            observeEvent(input$startDE, {
                buttonValues$startDE <- TRUE
                buttonValues$goQCplots <- FALSE
                init_data <- NULL 
                togglePanels(1, c( 0, 1, 2, 3, 4), session)
                choicecounter$qc <- 0
            })
            observeEvent(input$goQCplots, {
                choicecounter$qc <- 1
                buttonValues$startDE <- FALSE
                buttonValues$goQCplots <- TRUE
                togglePanels(2, c( 0, 2, 4), session)
            })
            comparison <- reactive({
                compselect <- 1
                if (!is.null(input$compselect))
                    compselect <- as.integer(input$compselect)
                dc()[[compselect]]
            })
            conds <- reactive({ comparison()$conds })
            cols <- reactive({ comparison()$cols })
            init_data <- reactive({ 
                if (!is.null(comparison()$init_data))
                    comparison()$init_data 
                else
                    qcdata()
            })
            filt_data <- reactive({
                if (!is.null(comparison()$init_data) &&
                    !is.null(input$padjtxt) &&
                    !is.null(input$foldChangetxt))
                    applyFilters(init_data(), isolate(cols()), isolate(conds()),
                                 input)
            })
            randstr <- reactive({ 
                a<-NULL
                if (!is.null(selected$data$randstr))
                    a<-selected$data$randstr() 
                a
            })
            selected <- reactiveValues(data = NULL)
            observe({
                setFilterParams(session, input)
                if (!is.null(input$genenames) && input$interactive == TRUE){
                    if (!is.null(isolate(filt_data())))
                        selected$data <- getSelHeat(isolate(filt_data()), input$genenames)
                    else
                        selected$data <- getSelHeat(isolate(init_data()), input$genenames)
                }
            })
            condmsg <- reactiveValues(text = NULL)
            observeEvent(input$startPlots, {
                compselect <- 1
                if (!is.null(input$compselect) ) 
                    compselect <- as.integer(input$compselect)
                if (!is.null(isolate(filt_data())) && !is.null(input$padjtxt) && 
                    !is.null(input$foldChangetxt)) {
                    condmsg$text <- getCondMsg(isolate(dc()), input$compselect,
                                               isolate(cols()), isolate(conds()))
                    selected$data <- getMainPanelPlots(isolate(filt_data()), 
                                                       isolate(cols()), isolate(conds()), input, compselect)
                }
            })
            qcdata <- reactive({
                prepDataForQC(Dataset()[,input$samples])
            })
            edat <- reactiveValues(val = NULL)
            output$qcplotout <- renderPlot({
                if (!is.null(input$col_list) || !is.null(isolate(df_select()))){
                    updateTextInput(session, "dataset", 
                                    value =  choicecounter$lastselecteddataset)
                    edat$val <- explainedData()
                    getQCReplot(isolate(cols()), isolate(conds()), 
                                df_select(), isolate(input), inputQCPlot(),
                                drawPCAExplained(edat$val$plotdata) )
                }
            })
            df_select <- reactive({
                if (!is.null(isolate(Dataset())))
                    getSelectedCols(Dataset(), datasetInput(), input)
            })
            
            v <- c()
            output$intheatmap <- d3heatmap::renderD3heatmap({
                shinyjs::onclick("intheatmap", js$getNames(v))
                getIntHeatmap(isolate(df_select()), input, inputQCPlot())
            })
            
            output$columnSelForHeatmap <- renderUI({
                wellPanel(id = "tPanel",
                          style = "overflow-y:scroll; max-height: 200px",
                          checkboxGroupInput("col_list", "Select col to include:",
                                             isolate(input$samples), 
                                             selected=isolate(input$samples))
                )
            })
            
            explainedData <- reactive({
                getPCAexplained( datasetInput(), input )
            })
            
            inputQCPlot <- reactiveValues(clustering_method = "ward.D2",
                                          distance_method = "cor", interactive = FALSE, width = 700, height = 500)
            inputQCPlot <- eventReactive(input$startQCPlot, {
                m <- c()
                m$clustering_method <- input$clustering_method
                m$distance_method <- input$distance_method
                m$interactive <- input$interactive
                m$width <- input$width
                m$height <- input$height
                return(m)
            })
            
            goplots <- reactive({
                dat <- getDataForTables(input, init_data(),
                                        filt_data(), selected,
                                        getMostVaried(),  isolate(mergedComp()),
                                        isolate(edat$val$pcaset))
                getGOPlots(dat[[1]][, isolate(cols())], input)
            })
            
            inputGOstart <- eventReactive(input$startGO, {
                goplots()
            })
            output$GOPlots1 <- renderPlot({
                if (!is.null(inputGOstart()$p) && input$startGO){
                    return(inputGOstart()$p)
                }
            })
            
            output$getColumnsForTables <-  renderUI({
                if (is.null(table_col_names())) return (NULL)
                selected_list <- table_col_names()
                if (!is.null(input$table_col_list))
                    selected_list <- input$table_col_list
                a <- list(
                    wellPanel(id = "tPanel",
                              style = "overflow-y:scroll; max-height: 200px",
                              checkboxGroupInput("table_col_list", "Select col to include:",
                                                 table_col_names(), 
                                                 selected=selected_list)
                    )
                )
            })
            table_col_names <- reactive({
                if (is.null(tabledat())) return (NULL)
                colnames(tabledat()[[1]])
            })
            tabledat <- reactive({
                dat <- getDataForTables(input, init_data(),
                                        filt_data(), selected,
                                        getMostVaried(),  isolate(mergedComp()),
                                        isolate(edat$val$pcaset))
                if (is.null(dat)) return (NULL)
                dat2 <- removeCols(c("ID", "x", "y","Legend", "Size"), dat[[1]])
                
                pcols <- c(names(dat2)[grep("^padj", names(dat2))], 
                           names(dat2)[grep("pvalue", names(dat2))])
                if (!is.null(pcols) & length(pcols) > 1)
                    dat2[,  pcols] <- apply(dat2[,  pcols], 2,
                                            function(x) format( as.numeric(x), scientific = TRUE, digits = 3 ))
                else
                    dat2[,  pcols] <- format( as.numeric( dat2[,  pcols] ), 
                                              scientific = TRUE, digits = 3 )
                rcols <- names(dat2)[!(names(dat2) %in% pcols)]
                dat2[,  rcols] <- apply(dat2[,  rcols], 2,
                                        function(x) round( as.numeric(x), digits = 2))  
                dat[[1]] <- dat2
                dat
            })
            
            output$tables <- DT::renderDataTable({
                dat <- tabledat()
                if (is.null(dat) || is.null(table_col_names())
                    || is.null(input$table_col_list) || length(input$table_col_list)<1) 
                    return (NULL)
                if (!dat[[2]] %in% input$table_col_list)
                    dat[[2]]= ""
                if (!dat[[3]] %in% input$table_col_list)
                    dat[[3]]= ""
                
                m <- DT::datatable(dat[[1]][, input$table_col_list],
                                   options = list(lengthMenu = list(c(10, 25, 50, 100),
                                                                    c("10", "25", "50", "100")),
                                                  pageLength = 25, paging = TRUE, searching = TRUE)) %>%
                    getTableStyle(input, dat[[2]], dat[[3]], buttonValues$startDE)
                
                m
            })
            getMostVaried <- reactive({
                a <- NULL
                if (choicecounter$qc == 0)
                    a <- filt_data()[filt_data()$Legend=="MV" | 
                                         filt_data()$Legend=="GS", ]
                else
                    a <- getMostVariedList(data.frame(init_data()), 
                                           c(input$samples), input$topn, input$mincount)
                a
            })
            
            output$gotable <- DT::renderDataTable({
                if (!is.null(inputGOstart()$table)){
                    DT::datatable(inputGOstart()$table,
                                  list(lengthMenu = list(c(10, 25, 50, 100),
                                                         c("10", "25", "50", "100")),
                                       pageLength = 25, paging = TRUE, searching = TRUE))
                }
            })
            
            mergedComp <- reactive({
                dat <- applyFiltersToMergedComparison(
                    isolate(mergedCompInit()), choicecounter$nc, input)
                ret <- dat[dat$Legend == "Sig", ]
                #ret[ret$Legend == "Sig", ] <- NULL
                ret
            })
            
            mergedCompInit <- reactive({
                merged <- getMergedComparison(
                    isolate(Dataset()), isolate(dc()), choicecounter$nc, input)
                merged
            })
            datasetInput <- function(addIdFlag = FALSE){
                m <- NULL
                if (choicecounter$qc == 0 ) {
                    mergedCompDat <- NULL
                    if (input$dataset == "comparisons")
                        mergedCompDat <- mergedComp()
                    m <- getSelectedDatasetInput(filt_data(), 
                                                 selected$data$getSelected(), getMostVaried(),
                                                 mergedCompDat, isolate(edat$val$pcaset), input)
                }
                else
                    m <- getSelectedDatasetInput(init_data(), 
                                                 getMostVaried = getMostVaried(),
                                                 explainedData = isolate(edat$val$pcaset),
                                                 input = input)
                if(addIdFlag)
                    m <- addID(m)
                if (input$dataset != "pcaset"){
                    choicecounter$lastselecteddataset = input$dataset
                }
                m
            }
            output$downloadData <- downloadHandler(filename = function() {
                paste(input$dataset, "csv", sep = ".")
            }, content = function(file) {
                dat <- getDataForTables(input, init_data(),
                                        filt_data(), selected,
                                        getMostVaried(),  isolate(mergedComp()),
                                        isolate(edat$val$pcaset))
                dat2 <- removeCols(c("x", "y","Legend", "Size"), dat[[1]])
                if(!("ID" %in% names(dat2)))
                    dat2 <- addID(dat2)
                write.table(dat2, file, sep = ",", row.names = FALSE)
            })
            
            # output$startup_object <- reactive({
            #     readRDS("shiny_saves/startup.rds")
            #     return(startup[['bookmark_counter']] == 0)
            # })
            
            output$downloadPlot <- downloadHandler(filename = function() {
                paste(input$qcplot, ".pdf", sep = "")
            }, content = function(file) {
                if (choicecounter$qc == 0)
                    saveQCPlot(file, input, datasetInput(), 
                               cols(), conds(), inputQCPlot())
                else
                    saveQCPlot(file, input, datasetInput(),
                               inputQCPlot = inputQCPlot())
            })
            
            output$downloadGOPlot <- downloadHandler(filename = function() {
                paste(input$goplot, ".pdf", sep = "")
            }, content = function(file) {
                pdf(file)
                print( inputGOstart()$p )
                dev.off()
            })
        },
        err=function(errorCondition) {
            cat("in err handler")
            message(errorCondition)
        },
        warn=function(warningCondition) {
            cat("in warn handler")
            message(warningCondition)
        })
}
