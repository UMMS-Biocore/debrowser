#' getGeneList
#'
#' Gathers the gene list to use for GOTerm analysis.
#"
#' @note \code{GOTerm}
#'
#' @export
#'
#' @note \code{getGeneList}
#' symobol to ENTREZ ID conversion
#' @param genes, gene list
#' @param org, orgranism for gene symbol entrez ID conversion
#' @return ENTREZ ID list
#'
#' @examples
#'     x <- getGeneList(c('OCLN', 'ABCC2'))
#'
getGeneList <- function(genes = NULL, org = "org.Hs.eg.db") {
    # Get the entrez gene identifiers that are mapped to a gene symbol
    installpack(org)
    allkeys <- AnnotationDbi::keys(eval(parse(text = org)), 
        keytype="SYMBOL")
    existinggenes <- unique(as.vector(unlist(lapply(toupper(genes), 
        function(x){ allkeys[x == toupper(allkeys)] }))))
    mapped_genes <- mapIds(eval(parse(text = org)), keys = existinggenes, 
        column="ENTREZID", keytype="SYMBOL",
        multiVals = "first")
    genelist <- unique(as.vector(unlist(mapped_genes)))
    genelist
}

#' getEnrichGO
#'
#' Gathers the Enriched GO Term analysis data to be used within the
#' GO Term plots.
#'
#' @note \code{getEnrichGO}
#' @param genelist, gene list
#' @param pvalueCutoff, p value cutoff
#' @param org, the organism used
#' @param ont, the ontology used
#' @return Enriched GO
#' @examples
#'     x <- getEnrichGO()
#'
#' @export
#'
getEnrichGO <- function(genelist = NULL, pvalueCutoff = 0.01,
    org = "org.Hs.eg.db", ont="CC") {
    if (is.null(genelist)) return(NULL)
    res <- c()
    installpack(org)
    res$enrich_p <- clusterProfiler::enrichGO(gene = genelist, OrgDb = org,
    # res$enrich_p <- enrichGO(gene = genelist, organism = "human",
            ont = ont, pvalueCutoff = pvalueCutoff)
                             
    res$p <- barplot(res$enrich_p, title = paste("Enrich GO", ont),
        font.size = 12)
    res$table <- NULL
    if (!is.null(nrow(res$enrich_p@result)) )
        res$table <- res$enrich_p@result[,c("ID", "Description", 
            "GeneRatio", "pvalue", "p.adjust", "qvalue")]
    return(res)
}

#' getEnrichKEGG
#'
#' Gathers the Enriched KEGG analysis data to be used within the
#' GO Term plots.
#'
#' @note \code{getEnrichKEGG}
#' @param genelist, gene list
#' @param org, the organism used
#' @param pvalueCutoff, the p value cutoff
#' @return Enriched KEGG
#' @examples
#'     x <- getEnrichKEGG()
#' @export
#'
getEnrichKEGG <- function(genelist = NULL, pvalueCutoff = 0.01,
    org = "org.Hs.eg.db") {
    if (is.null(genelist)) return(NULL)
    res <- c()
    res$enrich_p <- enrichKEGG(gene = genelist, organism = getOrganism(org),
        pvalueCutoff = pvalueCutoff)
    res$p <- barplot(res$enrich_p, title =
        paste("KEGG Enrichment: p-value=", pvalueCutoff))
    res$table <- NULL
    if (!is.null(nrow(res$enrich_p@result)) )
        res$table <- res$enrich_p@result[,c("ID", "Description", 
            "GeneRatio", "pvalue", "p.adjust", "qvalue")]
    return(res)
}

#' clusterData
#'
#' Gathers the Cluster analysis data to be used within the
#' GO Term plots.
#'
#' @note \code{clusterData}
#' @param dat, the data to cluster
#' @return clustered data
#' @examples
#'     mycluster <- clusterData()
#'
#' @export
#'
clusterData <- function(dat = NULL) {
    if (is.null(dat)) return(NULL)
    ret <- list()
    itemlabels <- rownames(dat)
    norm_data <- getNormalizedMatrix(dat)
    mydata <- na.omit(norm_data)  # listwise deletion of missing
    mydata <- scale(mydata)  # standardize variables

    wss <- (nrow(mydata) - 1) * sum(apply(mydata, 2, var))
    for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers = i)$withinss)
    plot(1:15, wss, type = "b",
        xlab = "Number of Clusters",
        ylab = "Within groups sum of squares")
    k <- 0
    for (i in 1:14) {
        if ( ( wss[i] / wss[i + 1] ) > 1.2 ) {
            k <- k + 1
        }
    }
    # K-Means Cluster Analysis
    fit <- kmeans(mydata, k)  # 5 cluster solution
    # get cluster means
    aggregate(mydata, by = list(fit$cluster), FUN = mean)
    # append cluster assignment
    mydata_cluster <- data.frame(mydata, fit$cluster)

    # distance <- dist(mydata, method = 'euclidean')
    # distance matrix fit <- hclust(distance,
    # method='ward.D2') plot(fit, cex = 0.1)
    # display dendogram groups <- cutree(fit, k=k) rect.hclust(fit,
    # k=k, border='red')
    return(mydata_cluster)
}

#' compareClust
#'
#' Compares the clustered data to be displayed within the GO Term
#' plots.
#'
#' @note \code{compareClust}
#' @param dat, data to compare clusters
#' @param ont, the ontology to use
#' @param org, the organism used
#' @param fun, fun
#' @param title, title of the comparison
#' @param pvalueCutoff, pvalueCutoff
#' @return compared cluster
#' @examples
#'     x <- compareClust()
#'   
#' @export
#' 
compareClust <- function(dat = NULL, ont = "CC", org = "org.Hs.eg.db",
    fun = "enrichGO", title = "Ontology Distribution Comparison",
    pvalueCutoff = 0.01) {
        if (is.null(dat)) return(NULL)
        installpack(org)
        res <- c()
        genecluster <- list()
        k <- max(dat$fit.cluster)
        for (i in 1:k) {
            clgenes <- rownames(dat[dat$fit.cluster == i, ])
            genelist <- getGeneList(clgenes, org)
            genecl <- list()
            genecl <- push(genecl, genelist)
            genecluster[c(paste("X", i, sep = ""))] <- genecl
        }
        res$table <- NULL
        p <- tryCatch({
            title <- paste(fun, title)
            xx <- c()
            if (fun == "enrichKEGG"){
                xx <- compareCluster(genecluster, fun = fun, 
                    organism = getOrganism(org),
                    pvalueCutoff = pvalueCutoff)
            } else if (fun == "enrichDO") {
                 installpack("DOSE")
                 xx <- compareCluster(genecluster, fun = fun,
                     pvalueCutoff = pvalueCutoff) 
            } else {
                title <- paste(ont, title)
                xx <- compareCluster(genecluster, fun = fun,
                    ont = ont, OrgDb = org, pvalueCutoff = pvalueCutoff)
                #ont = ont, organism = "human", pvalueCutoff = pvalueCutoff)
            }
            if (!is.null(xx@compareClusterResult) )
                res$table <- xx@compareClusterResult[,
                c("Cluster", "ID", "Description", "GeneRatio", "BgRatio", 
                  "pvalue", "p.adjust", "qvalue")]
            res$p <- dotplot(xx, title = title)
        })
        res
}
#' getEnrichDO
#'
#' Gathers the Enriched DO Term analysis data to be used within the
#' GO Term plots.
#'
#' @note \code{getEnrichDO}
#' @param genelist, gene list
#' @param pvalueCutoff, the p value cutoff
#' @return enriched DO
#' @examples
#'     x <- getEnrichDO()
#' @export
#'
getEnrichDO <- function(genelist = NULL, pvalueCutoff = 0.01) {
    if (is.null(genelist)) return(NULL)
    res <- c()
    res$enrich_p <- enrichDO(gene = genelist, ont = "DO",
        pvalueCutoff = pvalueCutoff)

    res$p <- barplot(res$enrich_p, title = "Enrich DO", font.size = 12)
    res$table <- NULL
    if (!is.null(nrow(res$enrich_p@result)) )
         res$table <- res$enrich_p@result[,c("ID", "Description", 
            "GeneRatio", "pvalue", "p.adjust", "qvalue")]
    res
}
