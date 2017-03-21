#getMethodDetails
#'
#' get the detail boxes after DE method selected 
#'
#' @param num, panel that is going to be shown
#' @param input, user input
#' @examples
#'     x <- getMethodDetails()
#'
#' @export
#'
#'
getMethodDetails <- function(num = 0, input = NULL) {
    if (num > 0)
        a <- list(
            conditionalPanel(
                (condition <- paste0("input.demethod",num," == 'DESeq2'")),
                getSelectInputBox("fitType", "Fit Type", num, 
                                c("parametric", "local", "mean"), 
                                selectedInput("testType", num, "parametric",
                                                input), 3),
                column(2, textInput(paste0("betaPrior", num), "Beta Prior", 
                                        value = isolate(selectedInput(
                                        "betaPrior", num, "0", input)) )),
                getSelectInputBox("testType", "Test Type", num, 
                            c("Wald", "LRT"),  
                            selectedInput("testType", num, "Wald", input))),
            conditionalPanel(
                (condition <- paste0("input.demethod",num," == 'EdgeR'")),
                getSelectInputBox("edgeR_normfact", "Normalization", num, 
                        c("TMM","RLE","upperquartile","none"), 
                        selectedInput("edgeR_normfact", num, "TMM", input), 3),
                column(2,textInput(paste0("dispersion", num), "Dispersion", 
                                value = isolate(selectedInput("dispersion", 
                                num, "0", input) ))),
                getSelectInputBox("edgeR_testType", "Test Type", num, 
                                  c("exactTest", "glmLRT"), 
                                  selectedInput("edgeR_testType", num,
                                                "exactTest", input))),
            conditionalPanel(
                (condition <- paste0("input.demethod",num," ==  'Limma'")),
                getSelectInputBox("limma_normfact", "Normalization", num, 
                        c("TMM","RLE","upperquartile","none"), 
                        selectedInput("limma_normfact", num, "TMM", input), 3),
                getSelectInputBox("limma_fitType", "Fit Type", num,
                        c("ls", "robust"), 
                        selectedInput("limma_fitType", num, "ls", input)),
                getSelectInputBox("normBetween", "Norm. Bet. Arrays", num,
                        c("none", "scale", "quantile", "cyclicloess",
                        "Aquantile", "Gquantile", "Rquantile","Tquantile"),
                        selectedInput("normBetween", num, "none", input))),
            column(2,textInput(paste0("rowsumfilter", num), "row.sum filter", 
                            value = isolate(selectedInput("rowsumfilter", num,
                            "10", input) ))),
            br())
}

#' getConditionSelector
#'
#' Selects user input conditions to run in DESeq.
#'
#' @param num, panel that is going to be shown
#' @param choices, sample list
#' @param selected, selected smaple list
#' @examples
#'     x <- getConditionSelector()
#'
#' @export
#'
getConditionSelector<- function(num=0, choices = NULL, selected = NULL) {
    if (!is.null(choices))
        a <- list(column(6, selectInput(paste0("condition", num),
                                        label = paste0("Condition ", num),
                                        choices = choices, multiple = TRUE,
                                        selected = selected)))
}

#' getConditionSelectorFromMeta
#'
#' Selects user input conditions to run in DESeq from metadata
#'
#' @param input, input
#' @param index, index
#' @param num, num
#' @param choices, choices
#' @param selected, selected
#' @param loadingJSON, loadingJSON
#' 
#' @examples
#'     x <- getConditionSelectorFromMeta()
#'
#' @export
#'
getConditionSelectorFromMeta <- function(input = NULL, index = 1, num=0, 
    choices = NULL, selected = NULL, loadingJSON = NULL) {
    if(is.null(input)) return(NULL) 
    if (is.null(input$demethod1)) return(NULL) 
    startup_path <- "shiny_saves/startup.rds"     
    if(!is.null(loadingJSON$username)){
        startup_path <- paste0("shiny_saves/", loadingJSON$username ,"/startup.rds")
        if(!file.exists(paste0("shiny_saves/", loadingJSON$username))){
            dir.create(paste0("shiny_saves/", loadingJSON$username))
        }
    }
    
    startup <- list()
    if(file.exists(startup_path)){
        startup <- readRDS(startup_path)
    }

    if(is.null(startup[['bookmark_counter']])){
        startup[['bookmark_counter']] <- 3
    }
    
    # startup[['bookmark_counter']] = 2 when Restoring from Bookmark
    if(startup[['bookmark_counter']] == 2){
        path_to_read <- paste0("shiny_bookmarks/", 
            startup[['startup_bookmark']] , "/input_save.rds")
        restored_input <- list()
        if(file.exists(path_to_read)){
            restored_input <- readRDS(path_to_read)
        }

        selected <- restored_input[[paste0("condition", num)]]
        if(is.null(restored_input[[paste0("condition", num + 1)]])){
            startup[['bookmark_counter']] <- 3
            saveRDS(startup, startup_path)
        }
    } 
    if (is.null(input$file2)){
        a <- list(column(6, selectInput(paste0("condition", num),
            label = paste0("Condition ", num),
            choices = choices, multiple = TRUE,
            selected = selected)))
        return(a)
    }
    
    selected_meta <- selectedInput("conditions_from_meta", index, NULL, 
        input)
    meta_rds_path <- paste0('shiny_bookmarks/',
        startup[['startup_bookmark']] , '/meta_selections.rds')
    
    if(file.exists(meta_rds_path)){
        meta_selections <- readRDS(meta_rds_path)
        current_meta_condition <- paste0("conditions_from_meta", index)
        old_selection <- meta_selections[[current_meta_condition]]
    }
    if(is.null(old_selection)){
        old_selection <- ""
    }
    if (is.null(selected_meta)) selected_meta <- "No Selection"

    if(is.null(startup[['bookmark_counter']])){
        startup[['bookmark_counter']] <- 3
    }
    if(startup[['bookmark_counter']] != 2) {
        if(!is.null(input[[paste0("condition", num)]])){
            selected <- input[[paste0("condition", num)]]
        } 
        meta_choices_all <- NULL
        if (!is.null(selected_meta))
            meta_choices_all <- get_conditions_given_selection(input,
                                        selected_meta)
        if(old_selection != selected_meta){
            if(typeof(meta_choices_all) == "character"){
                meta_choices <- list("There must be exactly 2 groups.")
            } else{
                meta1 <- meta_choices_all[[2 - (num %% 2)]]
                meta_choices <- unlist(meta1, recursive=FALSE)
            }
            selected <- meta_choices
        }
    }

    a <- list(column(6, selectInput(paste0("condition", num),
                                    label = paste0("Condition ", num),
                                    choices = choices
                                    , multiple = TRUE,
                                    selected = selected)))
}

#' selectedInput
#'
#' Selects user input conditions to run in DESeq.
#'
#' @param id, input id
#' @param num, panel that is going to be shown
#' @param default, default text
#' @param input, input params
#' @examples
#'     x <- selectedInput()
#'
#' @export
#'
selectedInput <- function(id = NULL, num = 0, default = NULL, 
                          input = NULL) {
    if (is.null(id)) return(NULL)
    m <- NULL
    if (is.null(input[[paste0(id, num)]]))
        m <- default
    else
        m <- input[[paste0(id, num)]]
    m
}

#' getSelectInputBox
#'
#' Selects user input conditions to run in DESeq.
#'
#' @param id, input id
#' @param name, label of the box
#' @param num, panel that is going to be shown
#' @param choices, sample list
#' @param selected, selected smaple list
#' @param cw, column width
#' @examples
#'     x <- getSelectInputBox()
#'
#' @export
#'
getSelectInputBox <- function(id = NULL, name = NULL, 
                              num = 0, choices = NULL, selected = NULL,
                              cw = 2) {
    if (is.null(id)) return(NULL)
    if (!is.null(choices))
        a <- list(column(cw, selectInput(paste0(id, num),
            label = name,
            choices = choices, multiple = FALSE,
            selected = selected)))
}


#' selectConditions
#'
#' Selects user input conditions, multiple if present, to be
#' used in DESeq.
#'
#' @param Dataset, used dataset 
#' @param choicecounter, total number of comparisons
#' @param loadingJSON, loads from json
#' @param input, input params
#' @note \code{selectConditions}
#' @return the panel for go plots;
#'
#' @examples
#'     x<- selectConditions()
#'
#' @export
#'
selectConditions<-function(Dataset = NULL,
                           choicecounter, input = NULL, loadingJSON = NULL) {
    if (is.null(Dataset)) return(NULL)
    selectedSamples <- function(num){
        if (is.null(input[[paste0("condition", num)]]))
            getSampleNames(input$samples, num %% 2 )
        else
            input[[paste0("condition", num)]]
    }
    nc <- choicecounter$nc

    if (nc >= 0) {
        if(!exists("all_selections")){
            all_selections <- ""
        }
        allsamples <- getSampleNames( input$samples, "all" )
        lapply(seq_len(nc), function(i) {
            if(typeof(input$file2) == "NULL"){
                current_selection <- 'No Selection'
            } else {
                current_selection <- input[[paste0("conditions_from_meta", i)]]
            }
            selected1 <- selectedSamples(2 * i - 1)
            selected2 <- selectedSamples( 2 * i )
            
            to_return <- list(column(12, getMetaSelector(input = input, n = i),

            conditionalPanel((condition <- paste0("input.conditions_from_meta",
                                                    i," != 'No Selection'")),
                    getConditionSelectorFromMeta(input, i,
                        (2 * i - 1), allsamples, selected1, loadingJSON),
                    getConditionSelectorFromMeta(input, i,
                        (2 * i), allsamples, selected2, loadingJSON)
             )
            ),
            
            column(12, 
                   column(1, helpText(" ")),
                   getSelectInputBox("demethod", "DE Method", i, 
                        c("DESeq2", "EdgeR", "Limma"),
                        selectedInput("demethod", i, "DESeq2", input)),
                   getMethodDetails(i, input)))
            
            new_selection <- selectedInput("conditions_from_meta", i, NULL, 
                                           input)
            startup_path <- "shiny_saves/startup.rds"
            if(!is.null(loadingJSON$username)){
                startup_path <- paste0("shiny_saves/", loadingJSON$username ,"/startup.rds")
                if(!file.exists(paste0("shiny_saves/", loadingJSON$username))){
                    dir.create(paste0("shiny_saves/", loadingJSON$username))
                }
            }
            startup <- list()
            if(file.exists(startup_path)){
                startup <- readRDS(startup_path)
            }

            meta_rds_path <- paste0('shiny_bookmarks/',
                startup[['startup_bookmark']] , '/meta_selections.rds')
            
            if(!file.exists(meta_rds_path)){
                if(!file.exists(paste0('shiny_bookmarks/', startup[['startup_bookmark']]))){
                    dir.create(paste0('shiny_bookmarks/', startup[['startup_bookmark']]))
                }
                saveRDS(list(), meta_rds_path)
            }
            meta_selections <- readRDS(meta_rds_path)
            
            current_meta_condition <- paste0("conditions_from_meta", i)
            meta_selections[[current_meta_condition]] <- new_selection
            
            saveRDS(meta_selections, meta_rds_path)

            return(to_return)
        })
    }
}


getMetaSelector <- function(input = NULL, n = 0){		
    metaFile <- input$file2		
    if(!is.null(metaFile)){		
        df <- read.csv(metaFile$datapath, sep = "\t", header=TRUE)
        col_count <- length(colnames(df))		

        list(HTML('<hr style="color: white; border:solid 1px white;">'),
             br(), column(10, selectInput(paste0("conditions_from_meta", 
                 n), label = "Select Meta",
                 choices = as.list(c("No Selection", 
                 colnames(df)[2:col_count])),
                 multiple = FALSE,
                 selected =  selectedInput("conditions_from_meta",
                 n, "Selection 2", input))))
    }
}


# Return the two set of conditions given the selection of meta select box		
get_conditions_given_selection <- function(input = NULL, selection){		
    metaFile <- input$file2		
    if(!is.null(metaFile)){		
        df <- read.csv(metaFile$datapath, sep = "\t", header=TRUE)		
        if(selection == "No Selection"){		
            return(NULL)		
        }		
        if(length(levels(factor(df[,selection]))) != 2){		
            return("There must be exactly 2 groups.")		
        } else {		
            # Assuming the first column has samples		
            sample_col_name <- colnames(df)[1]		
            
            condition1 <- levels(df[,selection])[1]		
            condition2 <- levels(df[,selection])[2]		
            
            # In case the conditions are integers		
            if(is.null(condition2)){		
                condition1 <- levels(factor(df[,selection]))[1]		
                condition2 <- levels(factor(df[,selection]))[2]		
            }		
            
            condition1_filtered <- df[df[,selection] == condition1, ]		
            a <- condition1_filtered[,sample_col_name]		
            
            condition2_filtered <- df[df[,selection] == condition2, ]		
            b <- condition2_filtered[,sample_col_name]		
            
            both_groups <- list(a, b)
            return(both_groups)		
        }		
    } else {		
        print("Meta file does not exist. Please start over and upload.")
        return(NULL)		
    }		
}