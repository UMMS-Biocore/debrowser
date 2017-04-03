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
#' @importFrom shiny actionButton actionLink addResourcePath column 
#'             conditionalPanel downloadButton downloadHandler 
#'             eventReactive fileInput fluidPage helpText isolate 
#'             mainPanel need numericInput observe observeEvent 
#'             outputOptions parseQueryString plotOutput radioButtons 
#'             reactive reactiveValues renderPlot renderUI runApp 
#'             selectInput shinyApp  shinyServer  shinyUI sidebarLayout 
#'             sidebarPanel sliderInput  stopApp  tabPanel tabsetPanel 
#'             textInput textOutput titlePanel uiOutput tags HTML
#'             h4 img icon updateTabsetPanel updateTextInput  validate 
#'             wellPanel checkboxInput br p checkboxGroupInput onRestore
#'             reactiveValuesToList renderText onBookmark onBookmarked 
#'             updateQueryString callModule enableBookmarking htmlOutput
#'             onRestored NS
#' @importFrom shinyjs show hide enable disable useShinyjs extendShinyjs
#'             js inlineCSS onclick
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
#' @importMethodsFrom DOSE dotplot summary
#' @importMethodsFrom AnnotationDbi as.data.frame as.list colnames
#'             exists sample subset head mappedkeys ncol nrow subset 
#'             keys mapIds
#' @importMethodsFrom GenomicRanges as.factor setdiff
#' @importMethodsFrom IRanges as.matrix "colnames<-" mean
#'             nchar paste rownames toupper unique which
#'             as.matrix lapply rev "rownames<-"
#'             gsub ifelse
#' @importMethodsFrom S4Vectors eval grep grepl levels rowMeans
#'             rowSums sapply t 
#' @importMethodsFrom SummarizedExperiment cbind order rbind
#' @importFrom jsonlite fromJSON
#' @importFrom methods new
#' @importFrom stringi stri_rand_strings
#' @importFrom annotate geneSymbols
#' @importFrom reshape2 melt
#' @importFrom baySeq getLibsizes getLikelihoods getLikelihoods.NB
#'             getPriors getPriors.NB nbinomDensity
#' @importMethodsFrom baySeq "densityFunction<-" "libsizes<-"
#' @importFrom clusterProfiler compareCluster enrichKEGG enrichGO
#' @importFrom DESeq2 DESeq DESeqDataSetFromMatrix results
#' @importFrom edgeR calcNormFactors equalizeLibSizes DGEList glmLRT
#'             exactTest estimateCommonDisp glmFit
#' @importFrom limma lmFit voom eBayes topTable
#' @importFrom sva ComBat
#' @importFrom RCurl getURL
#' @importFrom devtools install_github load_data
#' @import org.Hs.eg.db
#' @import org.Mm.eg.db
#' @import V8
#' @import shinydashboard

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
        options("googleAuthR.webapp.client_id" = 
        "186441708690-n65idoo8t19ghi7ieopat6mlqkht9jts.apps.googleusercontent.com")
        options("googleAuthR.webapp.client_secret" = "ulK-sj8bhvduC9kLU4VQl5ih")
        options(googleAuthR.scopes.selected = 
            c("https://www.googleapis.com/auth/userinfo.email",
            "https://www.googleapis.com/auth/userinfo.profile"))
    
        access_token <- callModule(googleAuth, "initial_google_button")
        # To hide the panels from 1 to 4 and only show Data Prep
        togglePanels(0, c(0), session)
        loadingJSON <- reactive({
            getJsonObj(isolate(session), isolate(input), access_token())
        })
        output$user_name <- renderText({
            if(exists(".startdebrowser.called")){
                return("local")
            }
            loadingJSON()$username
        })
    
        choicecounter <- reactiveValues(nc = 0, qc = 0, 
            lastselecteddataset = "")
        
        callModule(bookmarkServer, "bm", loadingJSON = loadingJSON())
        
        lapply(1:20, function(i) {
            shinyjs::onclick(paste0("bm-remove_bm", i),
                 list(
                     removeBookmark(i, loadingJSON()$username),
                     shinyjs::hide(paste0("bm-remove_bm", i)),
                     shinyjs::hide(paste0("bm-bookmark", i))
                 )
            )
        })
        # Save extra values in state$values when we bookmark...
        onBookmark(function(state) {
            # state$values can store data onBookmark to be restored later
            state$values$input_save <- input
            state$values$data <- Dataset
            state$values$nc <- choicecounter$nc
            state$values$samples <- input$samples
        })
        onRestored(function(state) {
            #Write the functions after restored
            shinyjs::js$showDropdown()
            if(!is.null(state$values$data)){
                #The file is uploaded, go to the next tab.
                buttonValues$gotoanalysis <- TRUE
            }
            if(!is.null(state$values$nc)){
                choicecounter$nc <- state$values$nc
            }
            if(choicecounter$nc > 0){
                shinyjs::enable("startDE")
            }
        })
        onBookmarked(function(url) {
            username <- loadingJSON()$username
            user_addition <- ""
            startup_path <- "shiny_saves/startup.rds"
            if(!is.null(username) && (username != "") ){
                user_addition <- paste0("&username=", username)
                startup_path <- paste0("shiny_saves/", 
                                       username ,"/startup.rds")
            }
            updateQueryString(paste0(url, user_addition))
            startup <- list()
            if(file.exists(startup_path)){
                startup <- readRDS(startup_path)
            }
            if(!file.exists("shiny_saves")){
                dir.create("shiny_saves")
            }
            shiny_saves_dir <- paste0("shiny_saves/", username)
            if(!file.exists(shiny_saves_dir)){
                dir.create(shiny_saves_dir)
            }
            startup[['startup_bookmark']] <- get_state_id(url)
            saveRDS(startup, startup_path)
            bookmark_dir_id <- get_state_id(url)
            file.copy(isolate(input$file1$datapath), 
                      paste0("shiny_bookmarks/", bookmark_dir_id, "/file1.tsv"))
        })
        observeEvent(input$stopApp, {
            stopApp(returnValue = invisible())
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
            if (buttonValues$startDE)
                choices <- c("up+down", "up", "down",
                             "comparisons", "alldetected",
                             "most-varied", "pcaset")
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
            if (buttonValues$gotoanalysis == TRUE || (!is.null(input$demo) && 
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
        observeEvent(input$add_btn, {
            shinyjs::enable("startDE")
            buttonValues$startDE <- FALSE
            choicecounter$nc <- choicecounter$nc + 1
        })
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
            selectConditions(Dataset(), choicecounter, input, loadingJSON())
        })
        dc <- reactive({
            dc <- NULL
            if (buttonValues$startDE == TRUE){
                dc <- prepDataContainer(Dataset(), choicecounter$nc, 
                     isolate(input))
            }
            dc
        })
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
            selected$data$randstr <- NULL
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
            if (!is.null(dc())){
                if (is.list(dc())){
                    if(length(dc())<compselect)
                        compselect <- 1
                    dc()[[compselect]]
                }
                else
                    dc()
            }
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
                !is.null(input$foldChangetxt)){
                applyFilters(init_data(), isolate(cols()), 
                    isolate(conds()), input)
            }
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
            if ((!is.null(input$genenames) && input$interactive == TRUE) || 
                (!is.null(input$genesetarea) && input$genesetarea != "")){
                m <- init_data()
                if (!is.null(filt_data()))
                    m <- filt_data()
                genenames <- ""
                if (!is.null(input$genenames)){
                    genenames <- input$genenames
                } else {
                   m <- getSearchData(m, input)
                   genenames <- paste(rownames(m), collapse = ",")
                }
                selected$data <- getSelHeat(m, genenames)
            }
        })
        condmsg <- reactiveValues(text = NULL)
        startPlots <- reactive({
            compselect <- 1
            if (!is.null(input$compselect) ) 
                compselect <- as.integer(input$compselect)
            if (!is.null(isolate(filt_data())) && !is.null(input$padjtxt) && 
                !is.null(input$foldChangetxt)) {
                condmsg$text <- getCondMsg(dc(), input$compselect,
                    cols(), conds())
                selected$data <- getMainPanelPlots(filt_data(), 
                    cols(), conds(), input, compselect)
            }
        })
        observeEvent(input$startPlots, {
            startPlots()
        })
        qcdata <- reactive({
            prepDataForQC(Dataset()[,input$samples], input)
        })
        edat <- reactiveValues(val = NULL)
        observeEvent(input$qcplot, {
            shinyjs::js$showQCPlot()
        })
        output$qcplotout <- renderPlot({
            if (is.null(input$col_list) && is.null(df_select())) return(NULL)
            updateTextInput(session, "dataset", 
                value =  choicecounter$lastselecteddataset)
            edat$val <- explainedData()
            #if(input$qcplot=="pca" || input$qcplot=="IQR" || input$qcplot=="Density")
            #    shinyjs::js$hideQCPlot()
            getQCReplot(isolate(cols()), isolate(conds()), 
                df_select(), input, inputQCPlot(),
                drawPCAExplained(edat$val$plotdata) )
        })
        df_select <- reactive({
            getSelectedCols(Dataset(), datasetInput(), input)

        })
        
        v <- c()
        output$intheatmap <- d3heatmap::renderD3heatmap({
            shinyjs::onclick("intheatmap", js$getNames(v))
            dat <- df_select()
            getIntHeatmap(dat, input, inputQCPlot())
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
            getPCAexplained( df_select(), input )
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
            if (!is.null(input$table_col_list) 
                && all(input$table_col_list %in% colnames(tabledat()[[1]])))
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
            if (!is.null(pcols) && length(pcols) > 1)
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
            if (!all(input$table_col_list %in% colnames(dat[[1]]), na.rm = FALSE)) 
                return(NULL)
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
                    c(input$samples), input)
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
                if (input$dataset == "comparisons"){
                    mergedCompDat <- mergedComp()
                }
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

        output$downloadPlot <- downloadHandler(filename = function() {
            paste(input$qcplot, ".pdf", sep = "")
        }, content = function(file) {
            
            if (choicecounter$qc == 0)
                saveQCPlot(file, input, df_select(), 
                           cols(), conds(), inputQCPlot())
            else
                saveQCPlot(file, input, df_select(),
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
