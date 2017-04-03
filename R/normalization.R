#' getNormalizedMatrix
#'
#' Normalizes the matrix passed to be used within various methods
#' within DEBrowser.  Requires edgeR package
#'
#' @note \code{getGoPanel}
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
    if (method != "none"){
        M <- M[rowSums(M)>0, ]
        if (is.null(M) ) return (NULL)
        norm.factors <- edgeR::calcNormFactors(M, method = method)
        norm <- edgeR::equalizeLibSizes(edgeR::DGEList(M,
            norm.factors = norm.factors))$pseudo.counts
    }
    return(norm)
}
