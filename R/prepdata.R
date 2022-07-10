#' getSamples
#'
#' Gathers the sample names to be used within DEBrowser.
#'
#' @param cnames, names of the  samples
#' @param index, starting column in a tab separated file
#' @return choices
#' @export
#'
#' @examples
#'     x <- getSamples()
#'
getSamples <- function (cnames = NULL, index = 1) { 
    m <- NULL
    if (!is.null(cnames)) {
        cn <- cnames[index:length(cnames)]
        m <- as.list(NULL)
        for (i in seq(cn)) {
            m[i] <- cn[i]
        }
    }
    m
}

#' prepDEOutput
#'
#' Prepares the output data from DE analysis to be used within
#' DEBrowser
#'
#' @param data, loaded dataset
#' @param cols, columns
#' @param conds, conds
#' @param inputconds, inputconds
#' @param i, selected comparison number
#' @param input, input
#' @return data
#' @export
#'
#' @examples
#'     x <- prepDEOutput()
#'
prepDEOutput <- function(data = NULL, cols = NULL, 
    conds = NULL, inputconds=NULL, i=NULL, input = NULL) {
    if (is.null(data)) return (NULL)
    if (length(cols) != length(conds)) return(NULL)
    params <- inputconds$demethod_params[i]
    de_res <- runDE(data, cols, conds, params)
    de_res <- data.frame(de_res)
}


#' applyFilters
#'
#' Applies filters based on user selected parameters to be
#' displayed within the DEBrowser.
#'
#' @param filt_data, loaded dataset
#' @param cols, selected samples
#' @param conds, seleced conditions
#' @param input, input parameters
#' @return data
#' @export
#'
#' @examples
#'     x <- applyFilters()
#'
applyFilters <- function(filt_data = NULL, cols = NULL, conds=NULL,
    input = NULL){
    if (is.null(input$padj) || is.null(input$foldChange) 
        || is.null(filt_data)) return(NULL)
    compselect <- 1
    if (!is.null(input$compselect) ) 
        compselect <- as.integer(input$compselect)
    x <- paste0("Cond", 2*compselect - 1) 
    y <- paste0("Cond", 2*compselect)
    norm_data <- getNormalizedMatrix(filt_data[, cols], 
        input$norm_method)
    g <- data.frame(cbind(cols, conds))
    if (length(as.vector(g[g$conds == x, "cols"])) > 1 )
        filt_data$x <- log10(rowMeans(norm_data[, 
            as.vector(g[g$conds == x, "cols"])]) + 0.1)
    else
        filt_data$x <- log10(norm_data[, 
             as.vector(g[g$conds == x, "cols"])] + 0.1)
    if (length(as.vector(g[g$conds == y, "cols"])) > 1 )
        filt_data$y <- log10(rowMeans(norm_data[, 
             as.vector(g[g$conds == y, "cols"])]) + 0.1)
    else
        filt_data$y <- log10(norm_data[, 
             as.vector(g[g$conds == y, "cols"])] + 0.1)
    filt_data[,cols] <- norm_data
    
    padj_cutoff <- as.numeric(input$padj)
    foldChange_cutoff <- as.numeric(input$foldChange)
    m <- filt_data
    # Add column which says whether a gene significant or not
    m$Legend <- character(nrow(m))
    m$Size <- character(nrow(m))
    m[, "Size"] <- "40"
    m$Legend <- "NS"
    if (input$dataset == "up" || input$dataset == "up+down" || input$dataset == "selected") 
        m$Legend[m$foldChange >= foldChange_cutoff &
               m$padj <= padj_cutoff] <- "Up"
    if (input$dataset == "down" || input$dataset == "up+down" || input$dataset == "selected")
        m$Legend[m$foldChange <= (1 / foldChange_cutoff) &
               m$padj <= padj_cutoff] <- "Down"
    if (input$dataset == "most-varied" && !is.null(cols)) {
        most_varied <- getMostVariedList(m, cols, input)
        m[rownames(most_varied), c("Legend")] <- "MV"
    }
    if (!is.null(input$genesetarea) && input$genesetarea != ""
        && input$methodtabs == "panel1") {
        genelist <- getGeneSetData(m, c(input$genesetarea))
        m[rownames(genelist), "Legend"] <- "GS"
        m[rownames(genelist), "Size"] <- "100"
        tmp <- m["Legend"=="GS", ]
        tmp1 <- m["Legend"!="GS", ]
        m <- rbind(tmp1, tmp)
    }
    m
}
#' getSelectedDatasetInput
#'
#' Gathers the user selected dataset output to be displayed.
#'
#' @param rdata, filtered dataset
#' @param getSelected, selected data
#' @param getMostVaried, most varied data
#' @param mergedComparison, merged comparison data
#' @param input, input parameters
#' @return data
#' @export
#'
#' @examples
#'     x <- getSelectedDatasetInput()
#'
getSelectedDatasetInput<-function(rdata = NULL, getSelected = NULL, 
    getMostVaried = NULL, mergedComparison = NULL,
    input = NULL) {
    if (is.null(rdata)) return (NULL)
    m <- rdata
    if (input$dataset == "up") {
        m <- getUp(rdata)
    } else if (input$dataset == "down") {
        m <- getDown(rdata)
    } else if (input$dataset == "up+down") {
        m <- getUpDown(rdata)
    } else if (input$dataset == "alldetected") {
        m <- rdata
    } else if (input$dataset == "selected" && !is.null(input$selectedplot)) {
        m <- getSelected
    } else if (input$dataset == "most-varied") {
        m <- rdata[rownames(getMostVaried), ]
    } else if (input$dataset == "comparisons") {
        m <- mergedComparison
    } else if (input$dataset == "searched") {
        m <- getSearchData(rdata, input)
    }
    m
}


#' getMostVariedList
#'
#' Calculates the most varied genes to be used for specific plots
#' within the DEBrowser.
#'
#' @param datavar, loaded dataset
#' @param cols, selected columns
#' @param input, input 
#' @return data
#' @export
#'
#' @examples
#'     x <- getMostVariedList()
#'
getMostVariedList <- function(datavar = NULL, cols = NULL, input = NULL){
    if (is.null(datavar)) return (NULL)
    topn <- as.integer(as.numeric(input$topn))
    filtvar <- datavar[rowSums(datavar[,cols]) >
        as.integer(as.numeric(input$mincount)),]
    cv<-cbind(apply(filtvar, 1, function(x) 
        (sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE))), 1)
    colnames(cv)<-c("coeff", "a")
    cvsort<-cv[order(cv[,1],decreasing=TRUE),]
    topindex<-nrow(cvsort)
    if (topindex > topn) topindex <- topn
    cvsort_top <- head(cvsort, topindex)
    selected_var <- data.frame(datavar[rownames(cvsort_top),])
}


#' getSearchData
#'
#' search the geneset in the tables and return it
#'
#' @param dat, table data
#' @param input, input params
#' @return data
#' @export
#'
#' @examples
#'     x <- getSearchData()
#'
getSearchData <- function(dat = NULL, input = NULL)
{
  if (is.null(dat)) return(NULL)
  if (input$genesetarea != ""){
    dat <- getGeneSetData(dat, c(input$genesetarea))
  }
  dat
}

#' getGeneSetData
#'
#' Gathers the specified gene set list to be used within the
#' DEBrowser.
#'
#' @param data, loaded dataset
#' @param geneset, given gene set
#' @return data
#' @export
#'
#' @examples
#'     x <- getGeneSetData()
#'
getGeneSetData <- function(data = NULL, geneset = NULL) {
    if (is.null(data)) return (NULL)
    
    geneset1 <- unique(unlist(strsplit(geneset, split="[:;, \t\n\t]")))
    geneset2 <- geneset1[geneset1 != ""]
    if(length(geneset2) > 3)
        geneset2 <- paste0("^", geneset2, "$")
    
    dat1 <- as.data.frame(data)
    if(!("ID" %in% names(dat1)))
        dat2 <- addID(dat1)
    else
        dat2 <- dat1

    dat2$ID<-factor(as.character(dat2$ID))

    geneset4 <- unique(as.vector(unlist(lapply(toupper(geneset2), 
        function(x){ sapply(dat2[(grepl(x, toupper(dat2[,"ID"]))), "ID"], 
                            as.character) }))))
    retset <- data.frame(dat2[geneset4, ])
    retset
}

#' getUp
#' get up regulated data
#'
#' @param filt_data, filt_data
#' @return data
#' @export
#'
#' @examples
#'     x <- getUp()
#'
getUp <- function(filt_data = NULL){
    if(is.null(filt_data)) return(NULL)
    filt_data[
        filt_data[, "Legend"] == "Up" | 
        filt_data[, "Legend"] == "GS", ]
}
#' getDown
#' get down regulated data
#'
#' @param filt_data, filt_data
#' @return data
#' @export
#'
#' @examples
#'     x <- getDown()
#'
getDown <- function(filt_data = NULL){
    if(is.null(filt_data)) return(NULL)
    filt_data[
        filt_data[, "Legend"] == "Down"|
        filt_data[, "Legend"] == "GS", ]
}

#' getUpDown
#' get up+down regulated data
#'
#' @param filt_data, filt_data
#' @return data
#' @export
#'
#' @examples
#'     x <- getUpDown()
#'
getUpDown <- function(filt_data = NULL){
    if(is.null(filt_data)) return(NULL)
    filt_data[
        filt_data[, "Legend"] == "Up" | 
        filt_data[, "Legend"] == "Down"|
        filt_data[, "Legend"] == "GS", ]
}

#' getDataForTables
#' get data to fill up tables tab
#'

#' @param input, input parameters
#' @param init_data, initial dataset
#' @param filt_data, filt_data
#' @param selected, selected genes
#' @param getMostVaried, most varied genes
#' @param mergedComp, merged comparison set
#' @param explainedData, pca gene set
#' @return data
#' @export
#'
#' @examples
#'     x <- getDataForTables()
#'
getDataForTables <- function(input = NULL, init_data = NULL,
    filt_data = NULL, selected = NULL,
    getMostVaried = NULL,  mergedComp = NULL,
    explainedData = NULL){
    if (is.null(init_data )) return(NULL)
    if (is.null(filt_data)) filt_data <- init_data
    pastr <- "padj"
    fcstr <- "foldChange"
    dat <- NULL
    if (input$dataset == "alldetected"){
            dat <- getSearchData(filt_data, input)
    }
    else if (input$dataset == "up+down"){
        if (!is.null(filt_data))
            dat <- getSearchData(getUpDown(filt_data), input)
    }
    else if (input$dataset == "up"){
        if (!is.null(filt_data))
            dat <- getSearchData(getUp(filt_data), input)
    }
    else if (input$dataset == "down"){
        if (!is.null(filt_data))
            dat <- getSearchData(getDown(filt_data), input)
    }
    else if (input$dataset == "selected"){
        dat <- getSearchData(selected, input)
    }
    else if (input$dataset == "most-varied"){
        if (!is.null(filt_data)){
            d <- filt_data[rownames(getMostVaried),]
        }else{
            d <- init_data[rownames(getMostVaried),]
        }
        dat <- getSearchData(d, input)
    }
    else if (input$dataset == "comparisons"){
        if (is.null(mergedComp)) return(NULL)
        fcstr<-colnames(mergedComp)[grepl("foldChange", colnames(mergedComp))]
        pastr<-colnames(mergedComp)[grepl("padj", colnames(mergedComp))]
        dat <- getSearchData(mergedComp, input)
    }
    else if (input$dataset == "searched"){
        dat <- getSearchData(init_data, input)
    }
    list(dat, pastr, fcstr)
}


#' getMergedComparison
#'
#' Gathers the merged comparison data to be used within the
#' DEBrowser.
#' @param dc, data container
#' @param nc, the number of comparisons
#' @param input, input params
#' @return data
#' @export
#'
#' @examples
#'     x <- getMergedComparison()
#'
getMergedComparison <- function(dc = NULL, nc = NULL, input = NULL){
    if (is.null(dc)) return (NULL)
    mergeresults <- c()
    mergedata <- c()
    allsamples <- c()
    for ( ni in seq(1:nc)) {
        tmp <- dc[[ni]]$init_data[,c("foldChange", "padj")]

        samples <- dc[[ni]]$cols
        tt <- paste0("C", (2*ni-1),".vs.C",(2*ni))
        fctt <- paste0("foldChange.", tt)
        patt <-  paste0("padj.", tt)
        colnames(tmp) <- c(fctt,  patt)
        if(ni == 1){
            allsamples <- samples
            mergeresults <- tmp
            mergedata <- dc[[ni]]$init_data[,samples]
        }
        else{
            mergeresults[,fctt] <- character(nrow(tmp))
            mergeresults[,patt] <- character(nrow(tmp))
            mergeresults[rownames(tmp),c(fctt, patt)] <- tmp[,c(fctt, patt)]
            mergeresults[rownames(tmp),patt] <- tmp[,patt]
            mergeresults[is.na(mergeresults[,fctt]),fctt] <- 1 
            mergeresults[is.na(mergeresults[,patt]),patt] <- 1 
            remaining_samples <- dc[[ni]]$cols[!(samples %in% colnames(mergedata))]
            allsamples <- unique(c(allsamples, remaining_samples))
            mergedata <- cbind(mergedata,  dc[[ni]]$init_data[,remaining_samples])
            colnames(mergedata) <- allsamples
        }
    }
    mergedata[,allsamples] <- getNormalizedMatrix(mergedata[,allsamples], input$norm_method)
    cbind(mergedata, mergeresults)
}

#' applyFiltersToMergedComparison
#'
#' Gathers the merged comparison data to be used within the
#' DEBrowser.
#'
#' @param merged, merged data 
#' @param nc, the number of comparisons
#' @param input, input params
#' @return data
#' @export
#'
#' @examples
#'     x <- applyFiltersToMergedComparison()
#'
applyFiltersToMergedComparison <- function (merged = NULL, 
    nc = NULL, input = NULL)
{
    if (is.null(merged)) return (NULL)
    padj_cutoff <- as.numeric(input$padj)
    foldChange_cutoff <- as.numeric(input$foldChange)
    if (is.null(merged$Legend)){
        merged$Legend <- character(nrow(merged))
        merged$Legend <- "NS"
    }
    for ( ni in seq(1:nc)) {
        tt <- paste0("C", (2*ni-1),".vs.C",(2*ni))
        merged[which(as.numeric(merged[,c(paste0("foldChange.", tt))]) >= 
            foldChange_cutoff & as.numeric(merged[,c(paste0("padj.", tt))]) <= 
            padj_cutoff), "Legend"] <- "Sig"
        merged[which(as.numeric(merged[,c(paste0("foldChange.", tt))]) <= 
            1/foldChange_cutoff & as.numeric(merged[,c(paste0("padj.", tt))]) <= 
            padj_cutoff), "Legend"] <- "Sig"
    }
    print(head(merged))
    merged 
}

#' removeCols
#'
#' remove unnecessary columns
#'
#' @param cols, columns that are going to be removed from data frame
#' @param dat, data
#' @return data
#' @export
#'
#' @examples
#'     x <- removeCols()
#'
removeCols <- function( cols = NULL, dat = NULL) {
    if (is.null(dat)) return (NULL)
    for (colnum in seq(1:length(cols))){
         if (cols[colnum] %in% colnames(dat) )
              dat[, cols[colnum]]<- NULL
    }
    dat
}