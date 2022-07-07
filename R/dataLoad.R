#' debrowserdataload
#'
#' Module to load count data and metadata
#' 
#' @param input, input variables
#' @param output, output objects
#' @param session, session 
#' @param nextpagebutton, the name of the next page button after loading the data
#' @return main plot
#'
#' @return panel
#' @export
#'
#' @examples
#'     x <- debrowserdataload()
#'
debrowserdataload <- function(input = NULL, output = NULL, session = NULL, nextpagebutton = NULL) {
    if (is.null(input)) return(NULL)
    ldata <- reactiveValues(count=NULL, meta=NULL)
    loadeddata <- reactive({
        ret <- NULL
        if(!is.null(ldata$count)){
            ldata$count <- ldata$count[,sapply(ldata$count, is.numeric)]
            ret <- list(count = ldata$count, meta = ldata$meta)
        }
        return(ret)
    })
    output$dataloaded <- reactive({
        return(!is.null(loadeddata()))
    })
    outputOptions(output, "dataloaded", 
        suspendWhenHidden = FALSE)
    observe({
    query <- parseQueryString(session$clientData$url_search)
    jsonobj<-query$jsonobject
        
    # To test json load;
    # It accepts three parameters:
    # 1. jsonobject=https%3A%2F%2Fdolphin.umassmed.edu%2Fpublic%2Fapi%2F%3Fsource%3Dhttps%3A%2F%2Fbioinfo.umassmed.edu%2Fpub%2Fdebrowser%2Fadvanced_demo.tsv%26format%3DJSON
    # 2. meta=meta=https%3A%2F%2Fdolphin.umassmed.edu%2Fpublic%2Fapi%2F%3Fsource%3Dhttps%3A%2F%2Fbioinfo.umassmed.edu%2Fpub%2Fdebrowser%2Fsimple_meta.tsv%26format%3DJSON
    # 3. title=no
    # The finished product of the link will look like this without metadata:
    # 
    # https://127.0.0.1:3427/debrowser/R/?jsonobject=https%3A%2F%2Fdolphin.umassmed.edu%2Fpublic%2Fapi%2F%3Fsource%3Dhttps%3A%2F%2Fbioinfo.umassmed.edu%2Fpub%2Fdebrowser%2Fadvanced_demo.tsv%26format%3DJSON&title=no
    #        
    #  With metadata
    #
    #http://127.0.0.1:3427/?jsonobject=https%3A%2F%2Fdolphin.umassmed.edu%2Fpublic%2Fapi%2F%3Fsource%3Dhttps%3A%2F%2Fbioinfo.umassmed.edu%2Fpub%2Fdebrowser%2Fsimple_demo.tsv%26format%3DJSON&meta=https%3A%2F%2Fdolphin.umassmed.edu%2Fpublic%2Fapi%2F%3Fsource%3Dhttps%3A%2F%2Fbioinfo.umassmed.edu%2Fpub%2Fdebrowser%2Fsimple_meta.tsv%26format%3DJSON
    #
    #
    if (!is.null(jsonobj))
    {
        raw <- RCurl::getURL(jsonobj, .opts = list(ssl.verifypeer = FALSE),
             crlf = TRUE)
        data <- fromJSON(raw, simplifyDataFrame = TRUE)
        colnames(data) <- gsub("\\s+|\\.|\\-", "_", colnames(data))
        jsondata<-data.frame(data,stringsAsFactors = TRUE)
        
        rownames(jsondata)<-jsondata[, 1]
        jsondata<-jsondata[,c(3:ncol(jsondata))]
        jsondata[,c(1:ncol(jsondata))] <- sapply(
            jsondata[,c(1:ncol(jsondata))], as.numeric)
        jsondata <- jsondata[,sapply(jsondata, is.numeric)]
        ldata$count <- jsondata
        
        metadatatable <- NULL
        jsonmet <-query$meta
        if(!is.null(jsonmet)){
            raw <- RCurl::getURL(jsonmet, .opts = list(ssl.verifypeer = FALSE),
                crlf = TRUE)
            data <- fromJSON(raw, simplifyDataFrame = TRUE)
            data[,1] <- gsub("\\s+|\\.|\\-", "_", data[,1])
            
            metadatatable<-data.frame(data,
                stringsAsFactors = TRUE)
            
        }else{
            metadatatable <- cbind(colnames(ldata$count), 1)
            colnames(metadatatable) <- c("Sample", "Batch")
        }
        ldata$meta <- metadatatable
        input$Filter
    }
    })
    observeEvent(input$demo, {
        load(system.file("extdata", "demo", "demodata.Rda",
                         package = "debrowser"))

        ldata$count <- demodata
        ldata$meta <- metadatatable
    })
    observeEvent(input$demo2, {
        load(system.file("extdata", "demo", "demodata2.Rda",
                         package = "debrowser"))
        ldata$count <- demodata
        ldata$meta <- metadatatable
    })
    
    observeEvent(input$uploadFile, {
        if (is.null(input$countdata)) return (NULL)
        checkRes <- checkCountData(input)
        
        if (checkRes != "success"){
            showNotification(checkRes, type = "error")
            return(NULL)
        }
        counttable <-as.data.frame(
            try(
                read.delim(input$countdata$datapath, 
                header=T, sep=input$countdataSep, 
            row.names=1, strip.white=TRUE ), TRUE))
        colnames(counttable) <- gsub("\\s+|\\.|\\-", "_", colnames(counttable))
        counttable <- counttable[,sapply(counttable, is.numeric)]
        metadatatable <- c()
        if (!is.null(input$metadata$datapath)){
            metadatatable <- as.data.frame(
            try(
                read.delim(input$metadata$datapath, 
                header=TRUE, sep=input$metadataSep, strip.white=TRUE), TRUE))

            metadatatable[,1] <- gsub("\\s+|\\.|\\-", "_", metadatatable[,1])
            checkRes <- checkMetaData(input, counttable)
            if (checkRes != "success"){
                  showNotification(checkRes, type = "error")
                  return(NULL)
            }
            counttable <- counttable[, metadatatable[,1]]
        }
        else{
            metadatatable <- cbind(colnames(counttable), 1)
            colnames(metadatatable) <- c("Sample", "Batch")
        }
        if (is.null(counttable)) 
            {stop("Please upload the count file")}
        ldata$count <- counttable
        ldata$meta <- metadatatable
    })
    output$nextButton <- renderUI({
        actionButtonDE(nextpagebutton, label = nextpagebutton, styleclass = "primary")
    })
    observe({
        getSampleDetails(output, "uploadSummary", "sampleDetails", loadeddata())
    })
  list(load=loadeddata)
}

#' dataLoadUI
#' 
#' Creates a panel to upload the data
#'
#' @param id, namespace id
#' @return panel
#' @examples
#'     x <- dataLoadUI("load")
#'
#' @export
#'
dataLoadUI<- function (id) {
  ns <- NS(id)
  list(conditionalPanel(condition =  paste0("!output['", ns("dataloaded"),"']"), fluidRow(
             fileUploadBox(id, "countdata", "Count Data"),
             fileUploadBox(id, "metadata", "Metadata")
        ),
        fluidRow(column(12,
        actionButtonDE(ns("uploadFile"), label = "Upload", styleclass = "primary"), 
        actionButtonDE(ns("demo"),  label = "Load Demo (Vernia et. al)", styleclass = "primary"),
        actionButtonDE(ns("demo2"),  label = "Load Demo (Donnard et. al)", styleclass = "primary")))),
        fluidRow(column(12,
        conditionalPanel(condition = paste0("output['", ns("dataloaded"),"']"),
        uiOutput(ns("nextButton"))
        ))
        ), br(),
  fluidRow(
    shinydashboard::box(title = "Upload Summary",
        solidHeader = T, status = "info",
        width = 12, 
        fluidRow(
          column(12, 
              tableOutput(ns("uploadSummary"))
          )),
        fluidRow(
          column(12,div(style = 'overflow: scroll', 
              DT::dataTableOutput(ns("sampleDetails")))
          )
        )
    )
  ))
}

#' fileUploadBox
#'
#' File upload module
#' @param id, namespace id
#' @param inputId, input file ID
#' @param label, label
#' @note \code{fileUploadBox}
#' @return radio control
#'
#' @examples
#'    
#'     x <- fileUploadBox("meta", "metadata", "Metadata")
#'
#' @export
#'
fileUploadBox <- function(id = NULL, inputId = NULL, label = NULL) {
ns <- NS(id)
shinydashboard::box(title = paste0(label, " File"),
    solidHeader = TRUE, status = "info",
    width = 6,
    helpText(paste0("Upload your '", label," File'")),
    fileInput(inputId=ns(inputId), 
        label=NULL, 
        accept=fileTypes()
    ),
    sepRadio(id, paste0(inputId, "Sep")))
}

#' sepRadio
#'
#' Radio button for separators
#'
#' @param id, module id
#' @param name, name
#' @note \code{sepRadio}
#' @return radio control
#'
#' @examples
#'    
#'     x <- sepRadio("meta", "metadata")
#'
#' @export
#'
sepRadio <- function(id, name) {
  ns <- NS(id)
  radioButtons(inputId=ns(name), 
               label="Separator",
               choices=c(Comma=',',
                         Semicolon=';',
                         Tab='\t'
               ),
               selected='\t'
  )
}

#' fileTypes
#'
#' Returns fileTypes that are going to be used in creating fileUpload UI
#'
#' @note \code{fileTypes}
#' @return file types
#'
#' @examples
#'     x <- fileTypes()
#'
#' @export
#'
fileTypes <- function() {
  c('text/tab-separated-values',
    'text/csv',
    'text/comma-separated-values',
    'text/tab-separated-values',
    '.txt',
    '.csv',
    '.tsv')
}

#' checkCountData
#'
#' Returns if there is a problem in the count data.
#'
#' @note \code{checkCountData}
#' @param input, inputs
#' @return error if there is a problem about the loaded data
#
#' @examples
#'     x <- checkCountData()
#'
#' @export
#'
checkCountData <- function(input = NULL){
    if (is.null(input$countdata$datapath)) return(NULL)
    tryCatch({
        data <- read.table(input$countdata$datapath, sep=input$countdataSep)
        if (ncol(data) < 3) return ("Error: Please check if you chose the right separator!")
        dups <- data[duplicated(data[,1], fromLast = TRUE),1]
        if (length(dups)>1) return (paste0("Error: There are duplicate entried in  the rownames. (", 
            paste0(dups, collapse=","),")"))

        return("success")
    }, error = function(err) {
        return (paste0("Error(Count file):",toString(err)))
    }, warning = function(war) {
        return(paste0("Warning(Count file):",toString(err)))
    })
}


#' checkMetaData
#'
#' Returns if there is a problem in the count data.
#'
#' @note \code{checkMetaData}
#' @param input, input
#' @param counttable, counttable
#' @return error if there is a problem about the loaded data
#
#' @examples
#'     x <- checkMetaData()
#'
#' @export
#'
checkMetaData <- function(input = NULL, counttable = NULL){
    if (is.null(counttable) || is.null(input$metadata$datapath)) return(NULL)
     tryCatch({
        metadatatable <- read.table(input$metadata$datapath, sep=input$metadataSep, header=T)
        if (ncol(metadatatable) < 2) return ("Error: Please check if you chose the right separator!")
        met <- as.vector(metadatatable[order(as.vector(metadatatable[,1])), 1])
        met <- gsub("\\s+|\\.|\\-", "_", met)
        count <- as.vector(colnames(counttable)[order(as.vector(colnames(counttable)))])
        difference <- base::setdiff(met, count)
        if (length(difference)>0){
            return(paste0("Colnames doesn't match with the metada table(", paste0(difference,sep=","), ")"))
        }
        return("success")
    }, error = function(err) {
        return (paste0("Error(Matadata file):",toString(err)))
    }, warning = function(war) {
        return (paste0("Warning(Matadata file):",toString(war)))
    })
}
