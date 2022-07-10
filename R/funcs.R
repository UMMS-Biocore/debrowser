#' getSampleDetails
#' 
#' get sample details
#'
#' @param output, output
#' @param summary, summary output name
#' @param details, details ouput name
#' @param data, data 
#' @return panel
#' @examples
#'     x <- getSampleDetails()
#'
#' @export
#'
getSampleDetails<- function (output = NULL, summary = NULL, details = NULL, data = NULL) {
    if (is.null(data)) return(NULL)
    
    output[[summary]]<- renderTable({ 
        countdata <-  data$count
        samplenums <- length(colnames(countdata))
        rownums <- dim(countdata)[1]
        result <- rbind(samplenums, rownums)
        rownames(result) <- c("# of samples", "# of rows (genes/regions)")
        colnames(result) <- "Value"
        result
    },digits=0, rownames = TRUE, align="lc")
    
    output[[details]] <- DT::renderDataTable({ 
        dat <- colSums(data$count)
        dat <- cbind(names(dat), dat)
        dat[, c("dat")] <-  format(
            round( as.numeric( dat[,  c("dat")], digits = 2)),
            big.mark=",",scientific=FALSE)
        
        if (!is.null(data$meta)){
            met <- data$meta
            dat <- cbind(met, dat[,"dat"])
            rownames(dat) <- NULL
            colnames(dat)[ncol(dat)] <- "read counts"
        }else{
            rownames(dat) <- NULL
            colnames(dat) <- c("samples", "read counts")
        }
        dat
    })
}

#' selectGroupInfo
#'
#' Group info column selection. This can be used in batch effect
#' or coloring the groups in the plots.
#' @param metadata, metadata
#' @param input, input values
#' @param selectname, name of the select box
#' @param label, label of the select box
#' @note \code{selectGroupInfo}
#' @examples
#'     x <- selectGroupInfo()
#' @export
#'
selectGroupInfo <- function(metadata = NULL, input = NULL,
                              selectname = "groupselect",
                              label = "Group info") {
    if (is.null(metadata)) return (NULL)
    lst.choices <- as.list(c("None", colnames(metadata)))
    selectInput(selectname, label = label,
                choices = lst.choices,
                selected = 1)
}


#' addID
#'
#' Adds an id to the data frame being used.
#'
#' @param data, loaded dataset
#' @return data
#' @export
#'
#' @examples
#'     x <- addID()
#'
addID <- function(data = NULL) {
    if (is.null(data)) return (NULL)
    dat1 <- data.frame(data)
    dat1 <- cbind(rownames(data), data)
    colnames(dat1) <- c("ID", colnames(data))
    dat1
}

#' getVariationData
#'
#' Adds an id to the data frame being used.
#'
#' @param inputdata, dataset 
#' @param cols, columns
#' @param conds, conditions
#' @param key, gene or region name
#' @return plotdata
#' @export
#'
#' @examples
#'     x <- getVariationData()
#'
getVariationData <- function(inputdata = NULL, 
    cols = NULL, conds = NULL, key = NULL) {
    if (is.null(inputdata)) return (NULL)
    # Pick out the gene with this ID
    vardata <- inputdata[key, ]
    bardata <- as.data.frame(cbind(key, cols,
        t(vardata[, cols]), as.character(conds)) )
    colnames(bardata) <- c("genename", "libs", "count", "conds")
    bardata$count <- as.numeric(as.character(bardata$count))
    bardata$conds <- factor(bardata$conds)
    data <- rbind(bardata[bardata$conds == levels(bardata$conds)[1], ],
        bardata[bardata$conds == levels(bardata$conds)[2], ])
    data$conds  <- factor(data$conds, levels = unique(data$conds))
    data
}

#' getBSTableUI
#' prepares a Modal to put a table
#' @param name, name 
#' @param label, label
#' @param trigger, trigger button for the modal
#' @param size, size of the modal
#' @param modal, modal yes/no
#' @return the modal
#'
#' @examples
#'     x<- getBSTableUI()
#'
#' @export
getBSTableUI<-function(name = NULL,  label = NULL, trigger = NULL, size="large", modal = NULL){
    if (is.null(name)) return (NULL)
    ret <- div(style = "display:block;overflow-y:auto; overflow-x:auto;",
               wellPanel( DT::dataTableOutput(name)))
    if (!is.null(modal) && modal)
        ret <- shinyBS::bsModal(name, label, trigger, size = size, ret)
    ret
}

#' getTableDetails
#' 
#' get table details
#' To be able to put a table into two lines are necessary;
#' into the server part;
#' getTableDetails(output, session, "dataname", data, modal=TRUE)
#' into the ui part;
#' uiOutput(ns("dataname"))
#'   
#' @param output, output
#' @param session, session
#' @param tablename, table name
#' @param data, matrix data
#' @param modal, if it is true, the matrix is going to be in a modal
#' @return panel
#' @examples
#'     x <- getTableDetails()
#'
#' @export
#'
getTableDetails <- function(output  = NULL, session  = NULL, tablename  = NULL, data = NULL, modal = NULL){
    if (is.null(data)) return(NULL)
    tablenameUI <-  paste0(tablename,"Table")
    output[[paste(tablename, "Download")]] <- downloadHandler(
        filename = function() {
            paste0(tablename,".tsv")
        },
        content = function(file) {
            if(!("ID" %in% names(data)))
                data <- addID(data)
            write.table(data, file, sep = "\t", row.names = FALSE)
        }
    )
    
    output[[tablename]] <- renderUI({
        ret <- getBSTableUI( session$ns(tablenameUI), "Show Data", paste0("show",tablename), modal = modal) 
        if (!is.null(modal) && modal)
           ret <- list( downloadButton(session$ns(paste(tablename, "Download")), "Download"),
               actionButtonDE(paste0("show",tablename), "Show Data", styleclass = "primary", icon="show"),
               ret)
        ret    
    })
    
    output[[tablenameUI]] <- DT::renderDataTable({
        if (!is.null(data)){
            DT::datatable(data, extensions = 'Buttons',
            options = list( server = TRUE,
            dom = "Blfrtip",
            buttons = 
              list("copy", list(
                  extend = "collection"
                  , buttons = c("csv", "excel", "pdf")
                  , text = "Download"
              ) ), # end of buttons customization
            
            # customize the length menu
            lengthMenu = list( c(10, 20,  50, -1) # declare values
                               , c(10, 20, 50, "All") # declare titles
            ), # end of lengthMenu customization
            pageLength = 10))
        }
    })
}

#' push
#'
#' Push an object to the list.
#'
#' @param l, that are going to push to the list
#' @param ..., list object
#' @return combined list
#'
#' @export
#'
#' @examples
#'     mylist <- list()
#'     newlist <- push ( 1, mylist )
push <- function(l, ...) c(l, list(...))

#' round_vals
#'
#' Plot PCA results.
#'
#' @param l, the value
#' @return round value
#' @export
#'
#' @examples
#'     x<-round_vals(5.1323223)
round_vals <- function(l) {
    l <- round(as.numeric(l), digits = 2)
    parse(text = l)
}

#' Buttons including Action Buttons and Event Buttons
#'
#' Creates an action button whose value is initially zero, and increments by one
#' each time it is pressed.
#'
#' @param inputId Specifies the input slot that will be used to access the
#'   value.
#' @param label The contents of the button--usually a text label, but you could
#'   also use any other HTML, like an image.
#' @param styleclass The Bootstrap styling class of the button--options are
#'   primary, info, success, warning, danger, inverse, link or blank
#' @param size The size of the button--options are large, small, mini
#' @param block Whehter the button should fill the block
#' @param icon Display an icon for the button
#' @param css.class Any additional CSS class one wishes to add to the action
#'   button
#' @param ... Other argument to feed into shiny::actionButton
#'
#' @export
#'
#' @examples
#'     actionButtonDE("goDE", "Go to DE Analysis")
#'
actionButtonDE <- function(inputId, label, styleclass = "", size = "",
        block = FALSE, icon = NULL, css.class = "", ...) {
    if (styleclass %in% c("primary", "info", "success", "warning",
        "danger", "inverse", "link")) {
        btn.css.class <- paste("btn", styleclass, sep = "-")
    } else btn.css.class = ""
    
    if (size %in% c("large", "small", "mini")) {
        btn.size.class <- paste("btn", size, sep = "-")
    } else btn.size.class = ""
    
    if (block) {
        btn.block = "btn-block"
    } else btn.block = ""
    
    if (!is.null(icon)) {
        icon.code <- HTML(paste0("<i class='fa fa-", icon, "'></i>"))
    } else icon.code = ""
    tags$button(id = inputId, type = "button", class = paste("btn action-button",
        btn.css.class, btn.size.class, btn.block, css.class, collapse = " "),
        icon.code, label, ...)
}


#' getNormalizedMatrix
#'
#' Normalizes the matrix passed to be used within various methods
#' within DEBrowser.  Requires edgeR package
#'
#' @note \code{getNormalizedMatrix}
#' @param M, numeric matrix
#' @param method, normalization method for edgeR. default is TMM
#' @return normalized matrix
#'
#' @examples
#'     x <- getNormalizedMatrix(mtcars)
#'
#' @export
#'
getNormalizedMatrix <- function(M = NULL, method = "TMM") {
    if (is.null(M) ) return (NULL)
    M[is.na(M)] <- 0
    norm <- M
    if (!(method == "none" || method == "MRN")){
        norm.factors <- edgeR::calcNormFactors(M, method = method)
        norm <- edgeR::equalizeLibSizes(edgeR::DGEList(M,
            norm.factors = norm.factors))$pseudo.counts
    }else if(method == "MRN"){
        columns <- colnames(M)
        conds <- columns
        coldata <- prepGroup(conds, columns)
        M[, columns] <- apply(M[, columns], 2,
            function(x) as.integer(x))
        dds <- DESeqDataSetFromMatrix(countData = as.matrix(M),
            colData = coldata, design = ~group)
        dds <- estimateSizeFactors(dds)
        norm <- counts(dds, normalized=TRUE)
    }
    return(norm)
}

#' getCompSelection
#'
#' Gathers the user selected comparison set to be used within the
#' DEBrowser.
#' @param name, the name of the selectInput
#' @param count, comparison count
#' @note \code{getCompSelection}
#' @examples
#'     x <- getCompSelection(name="comp", count = 2)
#' @export
#'
getCompSelection <- function(name = NULL, count = NULL) {
    a <- NULL
    if (count>1){
        a <- list(selectInput(name,
            label = "Choose a comparison:",
            choices = c(1:count) ))
    }
    a
}
#' getHelpButton
#' prepares a helpbutton for to go to a specific site in the documentation
#'
#' @param name, name that are going to come after info
#' @param link, link of the help
#' @return the info button
#'
#' @examples
#'     x<- getHelpButton()
#'
#' @export
getHelpButton<-function(name = NULL, link = NULL){
    if (is.null(name)) return(NULL)
    btn <- actionButtonDE(paste0("info_",name),"",icon="info",
        styleclass="info", size="small")
    
    HTML(paste0("<a id=\"info_",name,"\" href=\"",link,"\" target=\"_blank\">",
       btn,"</a>"))
    
}

#' getDomains
#'
#' Get domains for the main plots.
#'
#' @param filt_data, data to get the domains
#' @return domains
#' @export
#'
#' @examples
#'     x<-getDomains()
getDomains <- function(filt_data = NULL){
    if (is.null(filt_data)) return (NULL)
    a <- unique(filt_data$Legend)
    a <- a[a != ""]
    if (length(a) == 1)
        a <- c(a, "NA")
    a
}

#' getColors
#'
#' get colors for the domains 
#'
#' @param domains, domains to be colored
#' @return colors
#' @export
#'
#' @examples
#'     x<-getColors()
#'
getColors <- function(domains = NULL){
    if (is.null(domains)) return (NULL)
    colors <- c()
    for ( dn in seq(1:length(domains)) ){
        if (domains[dn] == "NS" || domains[dn] == "NA")
            colors <- c(colors, "#aaa")
        else if (domains[dn] == "Up")
            colors <- c(colors, "green")
        else if (domains[dn] == "Down")
            colors <- c(colors, "red")
        else if (domains[dn] == "MV")
            colors <- c(colors, "orange")
        else if (domains[dn] == "GS")
            colors <- c(colors, "blue")
    } 
    colors
}

#' getKEGGModal
#' prepares a modal for KEGG plots
#'
#' @return the info button
#'
#' @examples
#'     x<- getKEGGModal()
#'
#' @export
getKEGGModal<-function(){
    bsModal("modalExample", "KEGG Pathway", "KeggPathway", size = "large",
            div(style = "display:block;overflow-y:auto; overflow-x:auto;",imageOutput("KEGGPlot")))
}

#' getTableModal
#' prepares table modal for KEGG
#'
#' @return the info button
#'
#' @examples
#'     x<- getTableModal()
#'
#' @export
getTableModal<-function(){
    bsModal("modalTable", "Genes in the category", "GeneTableButton", size = "large",
            div(style = "display:block;overflow-y:auto; overflow-x:auto;",
                wellPanel( DT::dataTableOutput("GOGeneTable"))))
}

#' setBatch
#' to skip batch effect correction batch variable set with the filter results
#'
#' @param fd, filtered data
#' @return fd data
#'
#' @examples
#'    
#'     x <- setBatch()
#'
#' @export
#'
setBatch <- function(fd = NULL){
    if(!is.null(fd)){ 
        batchdata <- reactiveValues(count=NULL, meta = NULL)
        batchdata$count <-  fd$filter()$count
        batchdata$meta <-  fd$filter()$meta
        batcheffectdata <- reactive({
            ret <- NULL
            if(!is.null(batchdata$count)){
                ret <- batchdata
            }
            return(ret)
        })
        list(BatchEffect=batcheffectdata)
    }
}
#' getTabUpdateJS
#' prepmenu tab and discovery menu tab updates
#'
#' @return the JS for tab updates
#'
#' @examples
#'     x<- getTabUpdateJS()
#'
#' @export
getTabUpdateJS<-function(){
    tags$script(HTML( "
                      $(function() {
                      $('#methodtabs').attr('selectedtab', '2')
                      $($('#methodtabs >')[0]).attr('id', 'dataprepMethod')
                      $($('#menutabs >')[0]).attr('id', 'dataprepMenu')
                      for(var i=1;i<=5;i++){
                      $($('#methodtabs >')[i]).attr('id', 'discoveryMethod')
                      }
                      $($('#menutabs >')[1]).attr('id', 'discoveryMenu')
                      $(document).on('click', '#dataprepMethod', function () {
                      if($('#dataprepMenu').attr('class')!='active'){   
                      $('#dataprepMenu').find('a').click()
                      }
                      });
                      $(document).on('click', '#dataprepMenu', function () {
                      if($('#dataprepMethod').attr('class')!='active'){   
                      $('#dataprepMethod').find('a').click()
                      }
                      });
                      $(document).on('click', '#discoveryMethod', function () {
                      $('#methodtabs').attr('selectedtab', $(this).index())
                      if($('#discoveryMenu').attr('class')!='active'){   
                      $('#discoveryMenu').find('a').click()
                      }
                      });
                      $('#discoveryMenu > ').css('display', 'none');
                      $(document).on('click', '#goMain', function () {
                      $('#discoveryMenu > ').css('display', 'block');
                      });
                      $(document).on('click', '#discoveryMenu', function () {
                      $($('#methodtabs >')[ $('#methodtabs').attr('selectedtab')]).find('a').click()
                      });
                      //hide buttons on entrance
                      $('.sidebar-menu > ').css('display', 'none');
                      $('.sidebar-menu > :nth-child(1)').css('display', 'inline');
                      $('.sidebar-menu > :nth-child(2)').css('display', 'inline');
                      $(document).on('click', '#Filter', function () {
                      $('.sidebar-menu > :nth-child(2)').css('display', 'inline');
                      $('.sidebar-menu > :nth-child(3)').css('display', 'inline');
                      $('.sidebar-menu > :nth-child(4)').css('display', 'none');
                      $('.sidebar-menu > :nth-child(5)').css('display', 'none');
                      $('.sidebar-menu > :nth-child(6)').css('display', 'none');
                      $('.sidebar-menu > :nth-child(7)').css('display', 'none');
                      });
                      $(document).on('click', '#Batch', function () {
                      $('.sidebar-menu > :nth-child(4)').css('display', 'inline');
                      $('.sidebar-menu > :nth-child(5)').css('display', 'none');
                      $('.sidebar-menu > :nth-child(6)').css('display', 'none');
                      $('.sidebar-menu > :nth-child(7)').css('display', 'none');
                      });
                      $(document).on('click', '#goDEFromFilter', function () {
                      $('.sidebar-menu > :nth-child(5)').css('display', 'inline');
                      $('.sidebar-menu > :nth-child(6)').css('display', 'none');
                      $('.sidebar-menu > :nth-child(7)').css('display', 'none');
                      });
                      $(document).on('click', '#goDE', function () {
                      $('.sidebar-menu > :nth-child(5)').css('display', 'inline');
                      $('.sidebar-menu > :nth-child(6)').css('display', 'none');
                      $('.sidebar-menu > :nth-child(7)').css('display', 'none');
                      });
                      $(document).on('click', '#startDE', function () {
                      $('.sidebar-menu > :nth-child(6)').css('display', 'inline');
                      $('.sidebar-menu > :nth-child(7)').css('display', 'inline');
                      $('.sidebar-menu > :nth-child(2)').css('display', 'none');
                      });
                      })
                      "))
}
#' getPCAcontolUpdatesJS
#' in the prep menu we have two PCA plots to show how batch effect correction worked. 
#' One set of PCA input controls updates two PCA plots with this JS.
#' @return the JS for tab updates
#'
#' @examples
#'     x<- getTabUpdateJS()
#'
#' @export
getPCAcontolUpdatesJS<-function(){
    tags$script(HTML("  
                        var nameInputs = ['pcselx', 'pcsely'];
                        $.each(nameInputs, function (el) {
                                              $(function () {
                                              $(document).on('change keyup', '#batcheffect-beforeCorrectionPCA-'+nameInputs[el], function () {
                                              var value = $(this).val();
                                              $('#batcheffect-afterCorrectionPCA-'+nameInputs[el]).val(value)
                                              $('#batcheffect-afterCorrectionPCA-'+nameInputs[el]).trigger('change');
                                              });
                                              });
                        });
                        
                        var nameDropdowns = ['textonoff', 'legendonoff', 'legendSelect', 'color_pca','shape_pca'];
                        $.each(nameDropdowns, function (el) {
                            $(function () {
                                $(document).on('change', '#batcheffect-beforeCorrectionPCA-'+nameDropdowns[el], function () {
                                    var value = $(this).val();
                                    $('#batcheffect-afterCorrectionPCA-'+nameDropdowns[el])[0].selectize.setValue(value)
                                    $('#batcheffect-afterCorrectionPCA-'+nameDropdowns[el]).trigger('change');
                                });
                            });
                        });
                     $($('#batcheffect-pcacontrols')[0]).css('display', 'none');                     
                     $(document).on('click','#batcheffect-submitBatchEffect',function () {
                     setTimeout(function () { $($($('#batcheffect-pcacontrols')[0]).children()[1]).children().trigger('click')
                     setTimeout(function () { $($($('#batcheffect-pcacontrols')[0]).children()[0]).children().trigger('click')}, 1000);
                     }, 1000); })
                     "))
}

.initial <- function() {
    req <- function(...){
    reqFun <- function(pack) {
        if(!suppressWarnings(suppressMessages(require(pack, character.only = TRUE)))) {
            message(paste0("unable to load package ", pack))
            require(pack, character.only = TRUE)
        }
    }
    lapply(..., reqFun)
    }
    packs <- c("debrowser", "plotly", "shiny", "jsonlite", "shinyjs", "shinydashboard", "shinyBS")
    req(packs)
}

.onAttach <- function(libname, pkgname) {
    pkgVersion <- packageDescription("debrowser", fields="Version")
    msg <- paste0("DEBrowser v", pkgVersion, "  ",
                  "For help: https://debrowser.readthedocs.org/", "\n\n")
    
    citation <- paste0("If you use DEBrowser in published research, please cite:\n\n",
                       "Alper Kucukural, Onur Yukselen, Deniz M. Ozata, Melissa J. Moore, Manuel Garber\n", 
                       "DEBrowser: Interactive Differential Expression Analysis and Visualization Tool for Count Data\n",
                       "BMC Genomics 2019 20:6\n\ndoi:0.1186/s12864-018-5362-x\n")
    
    packageStartupMessage(paste0(msg, citation))
    .initial()

}