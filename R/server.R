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
#'             onRestored NS reactiveVal withProgress tableOutput
#'             selectizeInput fluidRow div renderPrint renderImage
#'             verbatimTextOutput imageOutput renderTable incProgress
#'             a h3 strong h2 withMathJax updateCheckboxInput
#'             showNotification updateSelectInput
#' @importFrom shinyjs show hide enable disable useShinyjs extendShinyjs
#'             js inlineCSS onclick
#' @importFrom DT datatable dataTableOutput renderDataTable formatStyle
#'             styleInterval formatRound
#' @importFrom ggplot2 aes aes_string geom_bar geom_point ggplot
#'             labs scale_x_discrete scale_y_discrete ylab
#'             autoplot theme_minimal theme geom_density
#'             geom_text element_blank margin
#' @importFrom plotly renderPlotly plotlyOutput plot_ly add_bars event_data
#'             hide_legend %>% group_by ggplotly
#' @importFrom gplots heatmap.2 redblue bluered
#' @importFrom igraph layout.kamada.kawai  
#' @importFrom grDevices dev.off pdf colorRampPalette 
#' @importFrom graphics barplot hist pairs par rect text plot
#' @importFrom stats aggregate as.dist cor cor.test dist
#'             hclust kmeans na.omit prcomp var sd model.matrix
#'             p.adjust runif cov mahalanobis quantile as.dendrogram
#'             density as.formula coef
#' @importFrom utils read.csv read.table write.table update.packages
#'             download.file read.delim data install.packages
#'             packageDescription installed.packages
#' @importFrom DOSE enrichDO
#' @importFrom enrichplot gseaplot dotplot
#' @importMethodsFrom DOSE summary
#' @importMethodsFrom AnnotationDbi as.data.frame as.list colnames
#'             exists sample subset head mappedkeys ncol nrow subset 
#'             keys mapIds select
#' @importMethodsFrom GenomicRanges as.factor setdiff
#' @importMethodsFrom IRanges as.matrix "colnames<-" mean
#'             nchar paste rownames toupper unique which
#'             as.matrix lapply "rownames<-" gsub
#' @importMethodsFrom S4Vectors eval grep grepl levels sapply t 
#' @importMethodsFrom SummarizedExperiment cbind order rbind
#' @importFrom jsonlite fromJSON
#' @importFrom methods new
#' @importFrom stringi stri_rand_strings
#' @importFrom annotate geneSymbols
#' @importFrom reshape2 melt
#' @importFrom Harman harman reconstructData
#' @importFrom clusterProfiler compareCluster enrichKEGG enrichGO gseGO bitr
#' @importFrom DESeq2 DESeq DESeqDataSetFromMatrix results estimateSizeFactors
#'             counts lfcShrink
#' @importFrom edgeR calcNormFactors equalizeLibSizes DGEList glmLRT
#'             exactTest estimateCommonDisp glmFit topTags
#' @importFrom shinydashboard dashboardHeader dropdownMenu messageItem
#'             dashboardPage dashboardSidebar sidebarMenu dashboardBody
#'             updateTabItems menuItem tabItems tabItem menuSubItem
#' @importFrom limma lmFit voom eBayes topTable
#' @importFrom sva ComBat
#' @importFrom RCurl getURL
#' @import org.Hs.eg.db
#' @import org.Mm.eg.db
#' @import shinyBS
#' @import colourpicker
#' @import RColorBrewer
#' @import heatmaply
#' @import apeglm
#' @import ashr

deServer <- function(input, output, session) {
    options(warn = -1)
    tryCatch(
    {
        if (!interactive()) {
            options( shiny.maxRequestSize = 30 * 1024 ^ 2,
                    shiny.fullstacktrace = FALSE, shiny.trace=FALSE, 
                    shiny.autoreload=TRUE, warn =-1)
        }
        # To hide the panels from 1 to 4 and only show Data Prep
        togglePanels(0, c(0), session)

        choicecounter <- reactiveValues(nc = 0)
        
        output$programtitle <- renderUI({
            togglePanels(0, c(0), session)
            getProgramTitle(session)
        })
        
        updata <- reactiveVal()
        filtd <- reactiveVal()
        batch <- reactiveVal()
        sel <- reactiveVal()
        dc <- reactiveVal()
        compsel <- reactive({
            cp <- 1
            if (!is.null(input$compselect_dataprep))
                cp <- input$compselect_dataprep
            cp
        })

        observe({
            updata(callModule(debrowserdataload, "load", "Filter"))
            updateTabItems(session, "DataPrep", "Upload")
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

            observeEvent (input$goDEFromFilter, {
                if(is.null(batch())) batch(setBatch(filtd()))
                updateTabItems(session, "DataPrep", "CondSelect")
                sel(debrowsercondselect(input, output, session,
                    batch()$BatchEffect()$count, batch()$BatchEffect()$meta))
                choicecounter$nc <- sel()$cc()
            })
            observeEvent (input$goDE, {
                updateTabItems(session, "DataPrep", "CondSelect")
                sel(debrowsercondselect(input, output, session,
                    batch()$BatchEffect()$count, batch()$BatchEffect()$meta))
                choicecounter$nc <- sel()$cc()
            })
            observeEvent (input$startDE, {
                if(!is.null(batch()$BatchEffect()$count)){
                    togglePanels(0, c(0), session)
                    res <- prepDataContainer(batch()$BatchEffect()$count, sel()$cc(), input, batch()$BatchEffect()$meta)
                    if(is.null(res)) return(NULL)
                    dc(res)
                    updateTabItems(session, "DataPrep", "DEAnalysis")
                    buttonValues$startDE <- TRUE
                    buttonValues$goQCplots <- FALSE
                    hideObj(c("load-uploadFile","load-demo", 
                        "load-demo2", "goQCplots", "goQCplotsFromFilter"))
                }
            })

            observeEvent (input$goMain, {
                updateTabItems(session, "methodtabs", "panel1")
                updateTabItems(session, "menutabs", "discover")
                togglePanels(0, c( 0, 1, 2, 3, 4), session)
            })
            
            output$compselectUI <- renderUI({
                if (!is.null(sel()) && !is.null(sel()$cc()))
                    getCompSelection("compselect_dataprep",sel()$cc())
            })

            output$cutOffUI <- renderUI({
                cutOffSelectionUI(paste0("DEResults", compsel()))
            })  
            output$deresUI <- renderUI({
                column(12, getDEResultsUI(paste0("DEResults",compsel())))
            })
        })
        output$mainpanel <- renderUI({
            getMainPanel()
        })
        output$qcpanel <- renderUI({
            getQCPanel(input)
        })
        output$gopanel <- renderUI({
            getGoPanel()
        })
        output$cutoffSelection <- renderUI({
            nc <- 1
            if (!is.null(choicecounter$nc)) nc <- choicecounter$nc
            getCutOffSelection(nc)
        })
        output$downloadSection <- renderUI({
            choices <- c("most-varied", "alldetected")
            if (buttonValues$startDE)
                choices <- c("up+down", "up", "down",
                             "comparisons", "alldetected",
                             "most-varied", "selected")
            choices <- c(choices, "searched")
            getDownloadSection(choices)
        })
       
        output$leftMenu  <- renderUI({
            getLeftMenu(input)
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
            if (is.null(condmsg()))
                getStartPlotsMsg()
            else
                condmsg()
        })
        buttonValues <- reactiveValues(goQCplots = FALSE, goDE = FALSE,
            startDE = FALSE)
        output$dataready <- reactive({
            hide(id = "loading-debrowser", anim = TRUE, animType = "fade")  
            return(!is.null(init_data()))
        })
        outputOptions(output, "dataready", 
                      suspendWhenHidden = FALSE)

        observeEvent(input$resetsamples, {
            buttonValues$startDE <- FALSE
            showObj(c("goQCplots", "goDE"))
            hideObj(c("add_btn","rm_btn","startDE"))
            choicecounter$nc <- 0
        })

        output$condReady <- reactive({
            if (!is.null(sel()))
                choicecounter$nc <- sel()$cc()
            choicecounter$nc
        })
        outputOptions(output, 'condReady', suspendWhenHidden = FALSE)
        observeEvent(input$goQCplotsFromFilter, {
            if(is.null(batch())) batch(setBatch(filtd()))
            buttonValues$startDE <- FALSE
            buttonValues$goQCplots <- TRUE
            updateTabItems(session, "menutabs", "discover")
            togglePanels(2, c( 0, 2, 4), session)
        })
        observeEvent(input$goQCplots, {
            buttonValues$startDE <- FALSE
            buttonValues$goQCplots <- TRUE
            updateTabItems(session, "menutabs", "discover")
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
            if (buttonValues$startDE && !is.null(comparison()$init_data))
                comparison()$init_data 
            else if (!is.null(batch()))
                batch()$BatchEffect()$count
        })
        filt_data <- reactive({
            if (!is.null(init_data()) && !is.null(comparison()) && !is.null(input$padj))
                applyFilters(init_data(), cols(), conds(), input)
        })

        selectedQCHeat <- reactiveVal()
        observe({
            if ((!is.null(input$genenames) && input$interactive == TRUE) || 
                (!is.null(input$genesetarea) && input$genesetarea != "")){
                tmpDat <- init_data()
                if (!is.null(filt_data()))
                    tmpDat <- filt_data()
                genenames <- ""
                if (!is.null(input$genenames)){
                    genenames <- input$genenames
                } else {
                   tmpDat <- getSearchData(tmpDat, input)
                   genenames <- paste(rownames(tmpDat), collapse = ",")
                }
            }
            if(!is.null(input$qcplot) && !is.null(normdat())){
                if (input$qcplot == "all2all") {
                    callModule(debrowserall2all, "all2all", normdat(), input$cex)
                } else if (input$qcplot == "pca") {
                    callModule(debrowserpcaplot, "qcpca", normdat(), batch()$BatchEffect()$meta)
                } else if (input$qcplot == "heatmap") {
                    selectedQCHeat(callModule(debrowserheatmap, "heatmapQC", normdat()))
                } else if (input$qcplot == "IQR") {
                    callModule(debrowserIQRplot, "IQR", df_select())
                    callModule(debrowserIQRplot, "normIQR", normdat())
                } else if (input$qcplot == "Density"){
                    callModule(debrowserdensityplot, "density", df_select())
                    callModule(debrowserdensityplot, "normdensity", normdat())
                }
            }
        })
        condmsg <- reactiveVal()
        selectedMain <- reactiveVal()
        observe({
            if (!is.null(filt_data())) {
            condmsg(getCondMsg(dc(), input,
                cols(), conds()))
            selectedMain(callModule(debrowsermainplot, "main", filt_data()))
            }
        })
        selectedHeat <- reactiveVal()
        observe({
            if (!is.null(selectedMain()) && !is.null(selectedMain()$selGenes())) {
                withProgress(message = 'Creating plot', style = "notification", value = 0.1, {
                    selectedHeat(callModule(debrowserheatmap, "heatmap", filt_data()[selectedMain()$selGenes(), cols()]))
                })
            }
        })
        
        selgenename <- reactiveVal()
        observe({
            if (!is.null(selectedMain()) && !is.null(selectedMain()$shgClicked()) 
                && selectedMain()$shgClicked()!=""){
                selgenename(selectedMain()$shgClicked())
                if (!is.null(selectedHeat()) && !is.null(selectedHeat()$shgClicked()) && 
                    selectedHeat()$shgClicked() != ""){
                    js$resetInputParam("heatmap-hoveredgenenameclick")
                }
            }
        })
        observe({
            if (!is.null(selectedHeat()) && !is.null(selectedHeat()$shgClicked()) && 
                selectedHeat()$shgClicked() != ""){
                selgenename(selectedHeat()$shgClicked())
            }
        })

        observe({
            if (!is.null(selgenename()) && selgenename()!=""){
                withProgress(message = 'Creating Bar/Box plots', style = "notification", value = 0.1, {
                    callModule(debrowserbarmainplot, "barmain", filt_data(), 
                               cols(), conds(), selgenename())
                    callModule(debrowserboxmainplot, "boxmain", filt_data(), 
                               cols(), conds(), selgenename())
                })
            }
        })
        
        normdat <-  reactive({
            if (!is.null(init_data()) && !is.null(datasetInput())){
                dat <- init_data()
                norm <- c()
                if(!is.null(cols())){
                    norm <- removeExtraCols(datasetInput())
                }else{
                    norm <- getNormalizedMatrix(dat, input$norm_method)
                }
                getSelectedCols(norm, datasetInput(), input)
            }
        })

        df_select <- reactive({
            if (!is.null(init_data()) && !is.null(datasetInput()) )
                getSelectedCols(init_data(), datasetInput(), input)
        })
        
        output$columnSelForQC <- renderUI({
            existing_cols <- colnames(removeExtraCols(datasetInput()))
            wellPanel(id = "tPanel",
                style = "overflow-y:scroll; max-height: 300px",
                checkboxGroupInput("col_list", "Select col to include:",
                existing_cols, 
                selected=existing_cols)
            )
        })

        selectedData <- reactive({
            dat <- isolate(filt_data())
            ret <- c()
            if (input$selectedplot == "Main Plot"  && !is.null(selectedMain())){
                ret <- dat[selectedMain()$selGenes(), ]
            }
            else if (input$selectedplot == "Main Heatmap" &&  !is.null(selectedHeat())){
                ret <- dat[selectedHeat()$selGenes(), ]
            }
            else if (input$selectedplot == "QC Heatmap" && !is.null(selectedQCHeat())){
                ret <- dat[selectedQCHeat()$selGenes(), ]
            }
            ret
        })
        
        datForTables <- reactive({
            getDataForTables(input, normdat(),
                filt_data(), selectedData(),
                getMostVaried(), mergedComp())
        })

        inputGOstart <- reactive({
            if (input$startGO){
                withProgress(message = 'GO Started', detail = "interactive", value = 0, {
                    dat <- datForTables()
                    getGOPlots(dat[[1]], isolate(getGSEARes()), input)
                })
            }
        })
        
        getGSEARes <- reactive({
            if (input$goplot == "GSEA"){
                dat <- datForTables()
                gopval <- as.numeric(input$gopvalue)
                getGSEA(dat[[1]], pvalueCutoff = gopval, 
                    org=input$organism, sortfield = input$sortfield)
            }
        })
        
        observeEvent(input$startGO, {
            inputGOstart()
        })

        output$GOPlots1 <- renderPlot({
            if (!is.null(inputGOstart()$p) && input$startGO){
                if (input$goplot == "GSEA" && !is.null(input$gotable_rows_selected)){
                    pid <- input$gotable_rows_selected
                    p <- gseaplot(inputGOstart()$enrich_p, by = "all", 
                    title = inputGOstart()$enrich_p$Description[pid[1]], 
                    geneSetID = pid[1])
                    return(p)
                }
                return(inputGOstart()$p)
            }
        })
        output$KEGGPlot <- renderImage({
            shiny::validate(need(!is.null(input$gotable_rows_selected),
                "Please select a category in the GO/KEGG table tab to be able
                to see the pathway diagram")
            )
            
            withProgress(message = 'KEGG Started', detail = "interactive", value = 0, {

            i <- input$gotable_rows_selected
           
            pid <- inputGOstart()$table$ID[i]

            drawKEGG(input, datForTables(), pid)
            list(src = paste0(pid,".b.2layer.png"),
                 contentType = 'image/png')
            })

        }, deleteFile = TRUE)

        getGOCatGenes <- reactive({
            if(is.null(input$gotable_rows_selected)) return (NULL)
            org <- input$organism
            dat <- tabledat()
            i <- input$gotable_rows_selected
            if (input$goplot == "GSEA"){
                genes <- inputGOstart()$enrich_p$core_enrichment[i]
            } else{
                genes <- inputGOstart()$enrich_p$geneID[i]
            }
            genedata <- getEntrezTable(genes,
                dat[[1]], org)
            dat[[1]] <- genedata
            dat
        })
        output$GOGeneTable <- DT::renderDataTable({
            shiny::validate(need(!is.null(input$gotable_rows_selected),
                "Please select a category in the GO/KEGG table to be able
                to see the gene list"))
            dat <- getGOCatGenes()
            if (!is.null(dat)){
                DT::datatable(dat[[1]],
                    list(lengthMenu = list(c(10, 25, 50, 100),
                        c("10", "25", "50", "100")),
                        pageLength = 25, paging = TRUE, searching = TRUE)) %>%
                    getTableStyle(input, dat[[2]], dat[[3]], buttonValues$startDE)
            }
        })
        
        output$getColumnsForTables <-  renderUI({
            if (is.null(table_col_names())) return (NULL)
            selected_list <- table_col_names()
            if (!is.null(input$table_col_list) 
                && all(input$table_col_list %in% colnames(tabledat()[[1]])))
                selected_list <- input$table_col_list
            colsForTable <- list(
                wellPanel(id = "tPanel",
                    style = "overflow-y:scroll; max-height: 200px",
                    checkboxGroupInput("table_col_list", "Select col to include:",
                    table_col_names(), 
                    selected=selected_list)
                )
            )
            return(colsForTable)
        })
        table_col_names <- reactive({
            if (is.null(tabledat())) return (NULL)
            colnames(tabledat()[[1]])
        })
        tabledat <- reactive({
            dat <- datForTables()
            if (is.null(dat)) return (NULL)
            if (nrow(dat[[1]])<1) return(NULL)
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
            if (!is.null(rcols) && length(rcols) > 1)
                dat2[,  rcols] <- apply(dat2[,  rcols], 2,
                                    function(x) round( as.numeric(x), digits = 2))
            else
                dat2[,  rcols] <-  round( as.numeric(dat2[,  rcols]), digits = 2)
                
            dat[[1]] <- dat2
            return(dat)
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
            
            datDT <- DT::datatable(dat[[1]][, input$table_col_list],
                options = list(lengthMenu = list(c(10, 25, 50, 100),
                c("10", "25", "50", "100")),
                pageLength = 25, paging = TRUE, searching = TRUE)) %>%
                getTableStyle(input, dat[[2]], dat[[3]], buttonValues$startDE)
            return(datDT)
        })
        getMostVaried <- reactive({
            dat <- init_data()
            if (!is.null(cols()))
                dat <- init_data()[,cols()]
            getMostVariedList(dat, colnames(dat), input)
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
                getMergedComparison(isolate(dc()), choicecounter$nc, input), 
                choicecounter$nc, input)
            dat[dat$Legend == "Sig", ]
        })
        
        datasetInput <- function(addIdFlag = FALSE){
            tmpDat <- NULL
            sdata <- NULL
            if (input$selectedplot != "QC Heatmap"){
                sdata <- selectedData()
            }else{
                sdata <- isolate(selectedData())
            }
            if (buttonValues$startDE) {
                mergedCompDat <- NULL
                if (input$dataset == "comparisons"){
                    mergedCompDat <- mergedComp()
                }
                tmpDat <- getSelectedDatasetInput(rdata = filt_data(), 
                     getSelected = sdata, getMostVaried = getMostVaried(),
                     mergedCompDat, input = input)
            }
            else{
                tmpDat <- getSelectedDatasetInput(rdata = init_data(), 
                     getSelected = sdata,
                     getMostVaried = getMostVaried(),
                     input = input)
            }
            if(addIdFlag)
                tmpDat <- addID(tmpDat)
            return(tmpDat)
        }
        output$metaFile <-  renderTable({
            read.delim(system.file("extdata", "www", "metaFile.txt",
                package = "debrowser"), header=TRUE, skipNul = TRUE)
        })
        output$countFile <-  renderTable({
            read.delim(system.file("extdata", "www", "countFile.txt",
                package = "debrowser"), header=TRUE, skipNul = TRUE)
        })
        
        output$downloadData <- downloadHandler(filename = function() {
            paste(input$dataset, "csv", sep = ".")
        }, content = function(file) {
            dat <- datForTables()
            dat2 <- removeCols(c("x", "y","Legend", "Size"), dat[[1]])
            if(!("ID" %in% names(dat2)))
                dat2 <- addID(dat2)
            write.table(dat2, file, sep = ",", row.names = FALSE)
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
