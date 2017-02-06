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

deServer <- function(input, output, session) {
    
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
        startup <- readRDS("shiny_saves/startup.rds")
        if(startup[['bookmark_counter']] == 0){
            cat("bookmark_counter: ", startup[['bookmark_counter']] , "\n")
            startup[['bookmark_counter']] <- 1
            saveRDS(startup, "shiny_saves/startup.rds")
            
            cat("startup[['bookmark_counter']] : " , "\n")
            print(startup[['bookmark_counter']])
            session$sendCustomMessage(type = 'testmessage',
                                      message = list(bookmarked_url = paste0("?_state_id_=",
                                                                             startup[['startup_bookmark']]), controller = input$controller))
        }
        
    })
    
    ###############################################################
    #         To save user chosen name as bookmark id             #
    ###############################################################
    observeEvent(input$name_bookmark, {
        
        
        
        chosen_name <- input$bookmark_special_name
        if(nchar(chosen_name) < 5){
            to_display <- "You must type in at least 5 characters."
        } else if(!grepl('^[A-Za-z0-9]+$', chosen_name)){
            to_display <- "You can only use numbers and English letters."
        } else{
            output$bookmark_length_error <- renderText({""})
            result <- copy_to_new_directory(chosen_name)
            if(result == 35){
                to_display <- "Please pick a new unique name."
            } else {
                if (result == 42) {
                    to_display <- paste0("Successfully saved. ",
                                         "URL updated with your choice to access later.")
                    
                    bm_link <- paste0('<a style="margin: 27px;" ',
                                      'target="_blank" href="?_state_id_=',
                                      chosen_name, '">', chosen_name, '</a>')
                    output$new_bookmark <- renderText({bm_link})
                    
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
        if(!is.null(query_list[['username']])){
            new_state_id <- paste0(query_list[['username']], "0User0",
                                   new_state_id)
        }
        
        # Get the state id from the query string
        bookmark_dir <- "shiny_bookmarks/"
        old_state_id <- system(paste0("ls -t1 shiny_bookmarks",
                                      " |  head -n 1"), intern=TRUE)
        
        if(!dir.exists(paste0(bookmark_dir, new_state_id))){
            
            if(file.rename(paste0(bookmark_dir, old_state_id), 
                           paste0(bookmark_dir, new_state_id))){
                
                if(!is.null(query_list[['username']])){
                    download.file(query_list$jsonobject, paste0(bookmark_dir,
                                                                new_state_id, "/file1.tsv"))
                }
                
                updateQueryString(paste0("?_state_id_=", new_state_id))
                startup <- readRDS("shiny_saves/startup.rds")
                startup[['startup_bookmark']] <- new_state_id
                saveRDS(startup, "shiny_saves/startup.rds")
                
                
                write(new_state_id,file="shiny_saves/past_saves.txt",
                      append=TRUE)
                return(42)
            }
            else{
                return(13)
            }
            
        } else {
            return(35)
        }
        
        
        # check_if_unique_command <- paste0('
        # 
        # dir="', bookmark_dir, new_state_id, '"
        # if [ -d "$dir" ]
        #     then
        #     exit 35
        # fi
        #  ')
        # 
        # # Now ready to copy the bookmark to a user chosen name
        # file_copy_unix_cmd <- paste0('
        # if [ -d "$dir" ] && [ "', state_id, '" != "', new_state_id, '"  ]
        # then
        #     mv $dir ', bookmark_dir, state_id , ' ', bookmark_dir , 
        #     new_state_id, '
        #     exit 42
        # fi
        # exit 1
        # ')
        # 
        # is_unique <- system( check_if_unique_command )
        # if(is_unique == 35){
        #     cat(paste0("unique value: ", is_unique, "\n" ))
        #     return(is_unique)
        # } else {
        #     to_return <- system( file_copy_unix_cmd )
        #     cat(paste0("returned value: ", to_return, "\n" ))
        #     return(to_return)
        # }
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
        updateQueryString(url)
        cat(paste0("updating the url to: ", url, "\n"))
        startup <- readRDS("shiny_saves/startup.rds")
        startup[['startup_bookmark']] <- get_state_id(url)
        saveRDS(startup, "shiny_saves/startup.rds")
        
        bookmark_dir_id <- get_state_id(url)
        file.copy(isolate(input$file1$datapath), 
                  paste0("shiny_bookmarks/", bookmark_dir_id, "/file1.tsv"))
        # Save the url to go to the bookmark later in a file
        # bookmark_log <- "saved_bookmark.txt"
        # line <- paste0(url, "\t", Sys.time(), "\n")
        # 
        # if(!file.exists(bookmark_log))
        #     file.create(bookmark_log)
        # write(line, file=bookmark_log)
        
    })
    
    
    # Read values from state$values when we restore
    onRestore(function(state) {
        cat(paste0("RESTORE++++++++++++++++++++++++++++++++++++++++++++++",
                   " nc ", choicecounter$nc, "qc ", choicecounter$qc, "\n"))
        query_list <- parseQueryString(session$clientData$url_search)
        
        
        startup <- readRDS("shiny_saves/startup.rds")
        startup[['bookmark_counter']] <- 2
        startup[['startup_bookmark']] <- query_list[["_state_id_"]]
        saveRDS(startup, "shiny_saves/startup.rds")
        
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
        # 
        # fileName <- "up_down.txt"
        # conn <- file(fileName,open="r")
        # linn <-readLines(conn)
        # for (i in 1:length(linn)){
        #     print(linn[i])
        # }
        # close(conn)
        
        cat("Restoring ", "\n")
    })
    
    onRestored(function(state) {
        cat("here")
    })
    
    
    tryCatch(
        {
            if (!interactive()) {
                options( shiny.maxRequestSize = 30 * 1024 ^ 2,
                         shiny.fullstacktrace = FALSE, shiny.trace=FALSE, 
                         shiny.autoreload=TRUE)
                #library(debrowser)
                #library(d3heatmap)
                #library(edgeR)
            }
            observeEvent(input$stopApp, {
                stopApp(returnValue = invisible())
            })
            observeEvent(input$bookmark_before_startDE, {
                output$bookmark_saved_output <- renderText({ 
                    "Save Successful! URL updated."
                })
            })
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
                if (!is.null(input$goDE) && input$goDE &&
                    !is.null(comparison()$init_data))
                    choices <- c("up+down", "up", "down",
                                 "comparisons", "alldetected",
                                 "most-varied", "pcaset")
                if (!is.null(selected$data))
                    if (!is.null(selected$data$getSelected())
                        && nrow(selected$data$getSelected())>1)
                        choices <- c(choices, "selected")
                    # if (!is.null(input$interactive) && input$interactive == TRUE)
                    #     choices <- c(choices, "selected")
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
            
            output$dataready <- reactive({
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
                session$doBookmark()
            })
            # observeEvent(input$file1, {		
            #     if((.bm.counter == 0) || (.bm.counter == 1)){		
            #         file.copy(isolate(input$file1$datapath), "file1.tsv",		
            #                   recursive = TRUE)		
            #     }		
            # })
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
            
            output$past_named_bookmarks <- renderText({
                path <- "shiny_saves/past_saves.txt"
                if(file.exists(path)){
                    conn <- file(path,open="r")
                    lines <- readLines(conn)
                    all_bookmarks <- "<div id='all_bookmarks' style='margin: 27px;'>"
                    if(length(lines) > 0){
                        all_bookmarks <- paste0(all_bookmarks, "<p>History:</p>")
                    }
                    for (i in length(lines):1){
                        bookmark_id <- lines[i]
                        if(bookmark_id != ""){
                            current <- paste0('<a target="_blank" href="?_state_id_=',
                                              bookmark_id, '">', bookmark_id, '</a>')
                            all_bookmarks <- paste0(all_bookmarks, current, "<br/>")
                        }
                    }
                    all_bookmarks <- paste0(all_bookmarks, "</div>")
                    close(conn)
                    return(all_bookmarks)
                }
                else {
                    return("")
                }
            })
            
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
                selectConditions(Dataset(), choicecounter, input)
            })
            dc <- reactive({
                dc <- NULL
                if (buttonValues$startDE == TRUE){
                    dc <- prepDataContainer(Dataset(), choicecounter$nc, 
                                            isolate(input))
                }
                dc
            })
            observeEvent(input$bookmark_before_startDE, {
                session$doBookmark()
            })
            
            observeEvent(input$save_state, {
                shinyjs::hide("save_state")
                session$doBookmark()
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
            
            output$startup_object <- reactive({
                readRDS("shiny_saves/startup.rds")
                return(startup[['bookmark_counter']] == 0)
            })
            
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