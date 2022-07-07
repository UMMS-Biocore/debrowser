#' debrowsercondselect
#'
#' Condition selection
#' This is not a module. Module construction didn't used here, just use it 
#' as functions not in a module.
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param data, count data
#' @param metadata, metadata
#' @return main plot
#'
#' @return panel
#' @export
#'
#' @examples
#'     x <- debrowsercondselect()
#'
debrowsercondselect <- function(input = NULL, output = NULL, session = NULL, data = NULL, metadata = NULL) {
    if (is.null(data)) return(NULL)
    choicecounter <- reactiveVal(0)
    output$conditionSelector <- renderUI({
        selectConditions(data, metadata, choicecounter(), session, input)
    })
    
    observeEvent(input$add_btn, {
        choicecounter(choicecounter() + 1)
    })
    observeEvent(input$rm_btn, {
        if (choicecounter() > 0) 
            choicecounter(choicecounter() - 1)
    })
    list(cc = choicecounter)
}

#' condSelectUI
#' Creates a panel to select samples for each condition
#'
#' @return panel
#' @examples
#'     x <- condSelectUI()
#'
#' @export
#'
condSelectUI<- function () {
    list(
        shinydashboard::box(title = "Comparison Selection",
            solidHeader = TRUE, status = "info",  width = NULL, height = NULL, collapsible = TRUE,
        fluidRow(
            uiOutput("conditionSelector"),
            column(12,actionButtonDE("add_btn", "Add New Comparison",styleclass = "primary"),
                actionButtonDE("rm_btn", "Remove", styleclass = "primary"),
                getHelpButton("method", "http://debrowser.readthedocs.io/en/master/deseq/deseq.html"),
                conditionalPanel(condition = ("output.condReady>0"),
                actionButtonDE("startDE", "Start DE", styleclass = "primary")))
        ))
    )
}
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
getMethodDetails <- function(num = NULL, input = NULL) {
    if (is.null(num)) return(NULL)
    if (num > 0)
        list(
            conditionalPanel(
                (condition <- paste0("input.demethod",num," == 'DESeq2'")),
                getSelectInputBox("fitType", "Fit Type", num, 
                    c("parametric", "local", "mean"), 
                    selectedInput("testType", num, "parametric",
                    input), 3),
                getSelectInputBox("betaPrior", "betaPrior", num, 
                    c(FALSE, TRUE), 
                    selectedInput("betaPrior", num,
                    FALSE, input),2),
                getSelectInputBox("testType", "Test Type", num, 
                    c("LRT", "Wald"),  
                    selectedInput("testType", num, "LRT", input)),
                getSelectInputBox("shrinkage", "Shrinkage", num, 
                    c("None", "apeglm", "ashr", "normal"),
                    selectedInput("shrinkage", num, "None", input))),
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
            br())
}

#' getCovariateDetails
#'
#' get the covariate detail box after DE method selected 
#'
#' @param num, panel that is going to be shown
#' @param input, user input
#' @param metadata, metadata
#' 
#' @examples
#'     x <- getCovariateDetails()
#'
#' @export
#'
getCovariateDetails <- function(num = NULL, input = NULL, metadata = NULL) {
    if (is.null(num)) return(NULL)
    if (num > 0) {
        choices <- as.list(c(colnames(metadata)[2:ncol(metadata)]))
        list(
            getSelectInputBox("covariate", "Covariate", num, choices,
                              selected = selectedInput("covariate", num, NULL, input), 
                              2, multiple = TRUE)
        )
    }
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
getConditionSelector<- function(num=NULL, choices = NULL, selected = NULL) {
    if (is.null(num)) return(NULL)
    if (!is.null(choices))
        list(column(3, selectInput(paste0("condition", num),
            label = paste0("Condition ", num),
            choices = choices, multiple = TRUE,
            selected = selected)))
}

#' getConditionSelectorFromMeta
#'
#' Selects user input conditions to run in DESeq from metadata
#'
#' @param metadata, meta data table
#' @param input, input
#' @param index, index
#' @param num, num
#' @param choices, choices
#' @param selected, selected
#' 
#' @examples
#'     x <- getConditionSelectorFromMeta()
#'
#' @export
#'
getConditionSelectorFromMeta <- function(metadata = NULL, input = NULL, index = 1, num=0, 
    choices = NULL, selected = NULL) {
    if (is.null(metadata)) return(NULL)
     a <- list(column(6, selectInput(paste0("condition", num),
            label = paste0("Condition ", num),
            choices = choices, multiple = TRUE,
            selected = selected)))

     if (!is.null(metadata)){
        selected_meta <- selectedInput("conditions_from_meta", 
            index, NULL, input)
            
        if (is.null(selected_meta)) selected_meta <- "No Selection"
    
        if (selected_meta != "No Selection"){
            old_selection <- ""
            
        if(!is.null(input[[paste0("condition", num)]])){
            selected <- input[[paste0("condition", num)]]
        } 
        grps <- unique(metadata[selected_meta])
        grps <- grps[grps!="NA"]
        grps<- grps[!is.na(grps) ]
        if (length(grps) == 2) {
            meta_choices_all <- NULL
            if (!is.null(selected_meta))
                meta_choices_all <- get_conditions_given_selection(metadata,
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
        }else{
            if(!is.null(input[[paste0("group", num)]])){
                selected <- metadata[metadata[,selected_meta] == input[[paste0("group", num)]], 1]
            }
        }
    
        a <- list(column(6, selectInput(paste0("condition", num),
            label = paste0("Condition ", num),
            choices = choices, multiple = TRUE,
            selected = selected)))
        }
    }
    return(a)
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
#' @param multiple, if multiple choices are available
#' @examples
#'     x <- getSelectInputBox()
#'
#' @export
#'
getSelectInputBox <- function(id = NULL, name = NULL, 
                              num = 0, choices = NULL, selected = NULL, 
                              cw = 2, multiple = FALSE) {
    if (is.null(id)) return(NULL)
    if (!is.null(choices))
        list(column(cw, selectInput(paste0(id, num),
            label = name,
            choices = choices, multiple = multiple,
            selected = selected)))
}


#' selectConditions
#'
#' Selects user input conditions, multiple if present, to be
#' used in DESeq.
#'
#' @param Dataset, used dataset 
#' @param metadata, metadatatable to select from metadata
#' @param choicecounter, choicecounter to add multiple comparisons
#' @param session, session
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
                           metadata = NULL,
                           choicecounter = NULL,
                           session = NULL,
                           input = NULL) {
    if (is.null(Dataset)) return(NULL)
    
    selectedSamples <- function(num){
        if (is.null(input[[paste0("condition", num)]]))
            getSampleNames(colnames(Dataset), num %% 2 )
        else
            input[[paste0("condition", num)]]
    }
    nc <- choicecounter
    
    if (nc >= 0) {
        allsamples <- getSampleNames( colnames(Dataset), "all" )

        lapply(seq_len(nc), function(i) {

            selected1 <- selectedSamples(2 * i - 1)
            selected2 <- selectedSamples( 2 * i )
            to_return <- list(column(12, getMetaSelector(metadata = metadata, input=input, n = i),
                    getGroupSelector(metadata, input, i, (2*i-1)),
                    getGroupSelector(metadata, input, i, (2*i)),
                    getConditionSelectorFromMeta(metadata, input, i,
                        (2 * i - 1), allsamples, selected1),
                    getConditionSelectorFromMeta(metadata, input, i,
                        (2 * i), allsamples, selected2)
            ),
            column(12, 
                   # column(1, helpText(" ")),
                   getSelectInputBox("demethod", "DE Method", i, 
                        c("DESeq2", "EdgeR", "Limma"),
                        selectedInput("demethod", i, "DESeq2", input)),
                   getMethodDetails(i, input)),
            column(12,
                   getCovariateDetails(i, input, metadata = metadata))
            )
            
            # check DE conditions
            if (!is.null(selectedInput("conditions_from_meta", 
                i, NULL, input)) && selectedInput("conditions_from_meta", 
                i, NULL, input) != "No Selection"){
                facts <- levels(factor(metadata[,selectedInput("conditions_from_meta", 
                     i, NULL, input)]))
                facts <- facts[facts != "" & facts != "NA"]
                if (length(facts) < 2) {
                    showNotification("There must be more than 2 groups in the selected condition.", 
                         type = "error")
                    updateSelectInput(session, paste0("conditions_from_meta", i), selected="No Selection" )
                }
            }
            
            # update selected of the covariate
            # if condition is selected, dont let the same column selected as covariate, then remove
            metadata_columns <- colnames(metadata)[2:ncol(metadata)]
            metadata_columns <- metadata_columns[!metadata_columns %in% selectedInput("conditions_from_meta", i, NULL, input)]
            if(!is.null(selectedInput("conditions_from_meta", i, NULL, input)) && !is.null(selectedInput("covariate", i, NULL, input)) &&
               selectedInput("conditions_from_meta", i, NULL, input) != "No Selection"){
                if(selectedInput("conditions_from_meta", i, NULL, input) %in% selectedInput("covariate", i, NULL, input)){
                    selected_meta <- selectedInput("covariate", i, NULL, input)
                    selected_meta <- selected_meta[!selected_meta %in% selectedInput("conditions_from_meta", i, NULL, input)]
                    updateSelectInput(session, paste0("covariate", i), choices = c(metadata_columns), 
                                      selected = c(selected_meta))
                    showNotification("Condition column is included in covariate list, removing!", 
                                     type = "error")
                    return(to_return)
                } 
            }
    
            # check appropriateness of covariates
            if (!is.null(selectedInput("covariate", i, NULL, input))){
                
                # establish metadata with selected samples, conditions and covariates
                selected_samples <- c(selected1,selected2)
                match_selected_samples <- match(selected_samples, colnames(Dataset))
                selected_covariate_names <- selectedInput("covariate",i, NULL, input)
                selected_covariates_metadata <- metadata[match_selected_samples,selected_covariate_names, drop = FALSE]
                selected_treatment <- rep(c("Cond1","Cond2"), c(length(selected1), length(selected2)))

                # loop over all covariates, flag covariates to be removed
                flag_selected <- rep(F,length(selected_covariate_names))
                for(kk in 1:ncol(selected_covariates_metadata)){
                    covariate <- selected_covariates_metadata[,kk]

                    # check if there are at least two groups in metadata
                    if (sum(is.na(covariate)) > 0) {
                        showNotification("Covariate shouldnt have an NA or empty values in any selected sample.",
                                         type = "error")
                        flag_selected[kk] <- T
                    }
                    
                    # check if there are at least two groups in metadata
                    if (length(unique(covariate)) < 2) {
                        showNotification("There must be at least 2 groups in the selected covariate.",
                                         type = "error")
                        flag_selected[kk] <- T
                    }
                    
                    # check if there are confounding covariates with treatment
                    covariate_vs_treatment <- table(covariate, selected_treatment)
                    if (any(covariate_vs_treatment == 0)) {
                        showNotification("Each condition should have at least one of all covariate groups.",
                                         type = "error")
                        flag_selected[kk] <- T
                    }
                }
                
                # remove covariates if an inappropriate flag is found
                if(any(flag_selected)){
                    new_covariate_names <- selected_covariate_names[!flag_selected]
                    selected_covariate_names <- selectedInput("covariate",i, NULL, input)
                    selected_covariate_names <- selected_covariate_names[selected_covariate_names %in% new_covariate_names]
                    updateSelectInput(session, paste0("covariate", i), selected= c(selected_covariate_names))   
                }
            }
            
            return(to_return)
        })
    }
}

#' getGroupSelector
#' Return the groups 
#'
#' @param metadata, meta data table
#' @param input, input params
#' @param index, index
#' @param num, num
#' @return meta select box
#'
#' @examples
#'     x<-getGroupSelector()
#' @export
#'
getGroupSelector <- function(metadata = NULL, input = NULL, index = 1, num=0) {
    a <- NULL
    selected_meta <- selectedInput("conditions_from_meta", 
        index, NULL, input)
    if (is.null(selected_meta) || selected_meta == "No Selection") return(NULL) 
    grps <- unique(metadata[selected_meta])
    grps <- grps[grps!="NA"]
    grps<- grps[!is.na(grps) ]
    if (length(grps) > 2) {
        grps_choices <- c("No Selection", grps)
        a <- list(column(6, selectInput(paste0("group", num),
            label = paste0("Group ", num),
            choices = grps_choices, 
            selected =  selectedInput("group",
                num, NULL, input),
            multiple = FALSE)))
    }
    return(a)
}

#' getMetaSelector
#'
#' Return the sample selection box using meta data table
#'
#' @param metadata, meta data table
#' @param input, input params
#' @param n, the box number
#' @return meta select box
#'
#' @examples
#'     x<-getMetaSelector()
#' @export
#'
getMetaSelector <- function(metadata = NULL, input = NULL, n = 0){		
    if(!is.null(metadata)){		
        df <- metadata
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

#' get_conditions_given_selection
#'
#' Return the two set of conditions given the selection of meta select box
#'
#' @param metadata, meta data table
#' @param selection, selection
#' @return meta select box
#'
#' @examples
#'     x<-get_conditions_given_selection()
#' @export
#'
get_conditions_given_selection <- function(metadata = NULL, selection = NULL){		
    if(is.null(metadata)) return(NULL)		
    df <- metadata
    if(selection == "No Selection"){		
        return(NULL)	
    }
    facts <- levels(factor(df[,selection]))
    facts <- facts[facts != "" & facts != "NA"]
    if(length(facts) != 2){		
        return(NULL)	
    } else {
        # Assuming the first column has samples		
        sample_col_name <- colnames(df)[1]		
        
        condition1 <- facts[1]		
        condition2 <- facts[2]		
        
        # In case the conditions are integers		
        if(is.null(condition2)){		
            condition1 <- factor(condition1)		
            condition2 <- factor(condition2)	
        }

        condition1_filtered <- df[df[,selection] == condition1, ]		
        a <- condition1_filtered[,sample_col_name]		

        condition2_filtered <- df[df[,selection] == condition2, ]		
        b <- condition2_filtered[,sample_col_name]	

        both_groups <- list(a, b)
        return(both_groups)		
    }
}

#' getSampleNames
#'
#' Prepares initial samples to fill condition boxes.  
#' it reads the sample names from the data and splits into two. 
#'
#' @param cnames, sample names in the header of a dataset
#' @param part, c(1,2). 1=first half and 2= second half
#' @return sample names.
#'
#' @examples
#'     x<-getSampleNames()
#' @export
#'
getSampleNames <- function(cnames = NULL, part = 1) {
    if (is.null(cnames)) return(NULL)
    
    startpos <- 1
    endpos <- length(cnames)
    if (part == 1)
        endpos <- floor(endpos / 2) 
    else if (part == 0)
        startpos <- floor(endpos / 2) + 1
    
    cn <- cnames[startpos:endpos]
    m <- as.list(NULL)
    for (i in seq(cn)) {
        m[[i]] <- cn[i]
    }
    m
}

#' prepDataContainer
#'
#' Prepares the data container that stores values used within DESeq.
#'
#' @param data, loaded dataset
#' @param counter, the number of comparisons
#' @param input, input parameters
#' @param meta, loaded metadata
#' @return data
#' @export
#'
#' @examples
#'     x <- prepDataContainer()
#'
prepDataContainer <- function(data = NULL, counter=NULL, 
                              input = NULL, meta = NULL) {
    if (is.null(data)) return(NULL)
    
    inputconds <- reactiveValues(demethod_params = list(), conds = list(), dclist = list())
    inputconds$conds <- list()
    for (cnt in seq(1:(2*counter))){
        inputconds$conds[cnt] <- list(isolate(input[[paste0("condition",cnt)]]))
    }
    #Get parameters for each method
    inputconds$demethod_params <- NULL
    for (cnt in seq(1:counter)){
        covariate <- isolate(paste(input[[paste0("covariate",cnt)]], collapse = "|"))
        covariate <- ifelse(covariate == "", "NoCovariate", covariate)
        if (isolate(input[[paste0("demethod",cnt)]]) == "DESeq2"){
            inputconds$demethod_params[cnt] <- paste(
                isolate(input[[paste0("demethod",cnt)]]),
                covariate,
                isolate(input[[paste0("fitType",cnt)]]),
                isolate(input[[paste0("betaPrior",cnt)]]),
                isolate(input[[paste0("testType",cnt)]]),
                isolate(input[[paste0("shrinkage",cnt)]]), sep=",")
        }
        else if (isolate(input[[paste0("demethod",cnt)]]) == "EdgeR"){
            inputconds$demethod_params[cnt]<- paste(
                isolate(input[[paste0("demethod",cnt)]]),
                covariate,
                isolate(input[[paste0("edgeR_normfact",cnt)]]),
                isolate(input[[paste0("dispersion",cnt)]]),
                isolate(input[[paste0("edgeR_testType",cnt)]]), sep=",")
        }
        else if (isolate(input[[paste0("demethod",cnt)]]) == "Limma"){
            inputconds$demethod_params[cnt] <- paste(
                isolate(input[[paste0("demethod",cnt)]]),
                covariate,
                isolate(input[[paste0("limma_normfact",cnt)]]),
                isolate(input[[paste0("limma_fitType",cnt)]]),
                isolate(input[[paste0("normBetween",cnt)]]), sep=",")
        }
    }
    
    for (i in seq(1:counter))
    {
        conds <- c(rep(paste0("Cond", 2*i-1), 
                       length(inputconds$conds[[2*i-1]])), 
                   rep(paste0("Cond", 2*i), length(inputconds$conds[[2*i]])))
        cols <- c(paste(inputconds$conds[[2*i-1]]), 
                  paste(inputconds$conds[[2*i]]))
        params <- unlist(strsplit(inputconds$demethod_params[i], ","))
        withProgress(message = 'Running DE Algorithms', detail = inputconds$demethod_params[i], value = 0, {
            initd <- callModule(debrowserdeanalysis, paste0("DEResults",i), data = data, metadata = meta, 
                  columns = cols, conds = conds, params = params)
            if (!is.null(initd$dat()) && nrow(initd$dat()) > 1){
                inputconds$dclist[[i]] <- list(conds = conds, cols = cols, init_data=initd$dat(), 
                    demethod_params = inputconds$demethod_params[i])
            }else{
                return(NULL)
            }
            incProgress(1/counter)
        })
    }

    if(length(inputconds$dclist) < 1) return(NULL)

    inputconds$dclist
}
