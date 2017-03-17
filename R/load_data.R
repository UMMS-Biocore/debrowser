#' load_data.
#'
#' Loads user selected data to be used for DESeq
#'
#' @param input, input values
#' @param session, if data is going to be loaded from json
#' @return data
#' @export
#'
#' @examples
#'     x<-load_data ()
load_data <- function (input = NULL, session = NULL) {
    if (is.null(input)) return(NULL)
    loaddemo <- reactiveValues(demo = NULL, demodata = NULL)
    loaddemo <- eventReactive(input$demo, {
        m <- c()
        m$demo <- TRUE
        load(system.file("extdata", "demo", "demodata.Rda",
            package = "debrowser"))
        m$demodata <- demodata
        m
    })
    
    query <- parseQueryString(session$clientData$url_search)
    jsonobj<-query$jsonobject
    existing_json_path <- paste0("shiny_bookmarks/", 
        parseQueryString(session$clientData$url_search)[["_state_id_"]],
                                 "/file1.JSON")
    if(file.exists(existing_json_path)){
        jsonobj <- existing_json_path
    }
    if (!is.null(jsonobj) && (jsonobj != "saved"))
    {
        jsondata<-data.frame(fromJSON(jsonobj, simplifyDataFrame = TRUE),
                             stringsAsFactors = TRUE)
        rownames(jsondata)<-jsondata[, 1]
        jsondata<-jsondata[,c(2:ncol(jsondata))]
        jsondata[,c(2:ncol(jsondata))] <- sapply(
            jsondata[,c(2:ncol(jsondata))], as.numeric)
        return(jsondata)
    }
    if (is.null(input$file1) && is.null(loaddemo()$demo)) {
    # User has not uploaded a file yet
        return(NULL)
    }
    else if (is.null(input$file1) && loaddemo()$demo == TRUE) {
        return(loaddemo()$demodata)
    }
    # Restore from file 1
    bookmark_file1_path <- paste0("shiny_bookmarks/", 
        parseQueryString(session$clientData$url_search)[["_state_id_"]],
        "/file1.tsv")
    if(file.exists(bookmark_file1_path)){
        to_read_from <- bookmark_file1_path
    }
    else {
        inFile1 <- input$file1
        to_read_from <- inFile1$datapath
    }
    try(m <- read.table(to_read_from, sep = "\t",
        header = TRUE, row.names = 1))
    m
}

#' Correct Batch Effect
#'
#' Batch effect correction
#' @param idata, data
#' @param input, input values
#' @return data
#' @export
#'
#' @examples
#'     x<-correctBatchEffect ()
correctBatchEffect <- function (idata = NULL, input = NULL) {
    if (is.null(idata)) return(NULL)
    inFile2 <- input$file2
    try(metadata <- read.table(inFile2$datapath, sep = "\t",
         header = TRUE, row.names = 1))
    batch <- metadata[, input$batchselect]
    columns <- rownames(metadata)
    meta <- data.frame(cbind(columns, batch))
    
    datacor <- data.frame(idata[, columns])
    datacor[, columns] <- apply(datacor[, columns], 2,
                                function(x) as.integer(x))
    
    datacor[, columns] <- apply(datacor[, columns], 2,
                                function(x) return(x + runif(1, 0, 0.01)))
    
    modcombat = model.matrix(~1, data = meta)
    
    combat_blind = ComBat(dat=datacor, batch=batch)
    
    a <- cbind(idata[rownames(combat_blind), 2], combat_blind)
    
    a[, columns] <- apply(a[, columns], 2, function(x) ifelse(x<0, 0, x))
    colnames(a[, 1]) <- colnames(idata[, 1])
    a
}

