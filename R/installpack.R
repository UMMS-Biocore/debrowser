#' installpack
#'
#' install packages if they don't exist
#' display.
#'
#' @param package_name, package name to be installed
#' @note \code{installpack}
#'
#' @examples
#'     x <- installpack()
#'
#' @export
#'
installpack <- function(package_name = NULL) {
    if (is.null(package_name)) return(NULL)
   
    if (!loadpack(package_name))
    {
       txt <- paste0("Please install ",package_name, " to use this function.")
       print(txt)
       showNotification(txt, type="error")
       return(FALSE)
    }else{
        loadpack(package_name)
    }
    return(TRUE)
}
#' loadpack
#'
#' load packages 
#'
#' @param package_name, package name to be loaded
#' @note \code{loadpack}
#'
#' @examples
#'     x <- loadpack()
#'
#' @export
#'
loadpack <- function (package_name = NULL){
    if(isTRUE(package_name %in% .packages(all.available=TRUE))) {
        eval(parse(text = sprintf("require(\"%s\")", package_name)))
        return (TRUE)
    }
    return(FALSE)
}
