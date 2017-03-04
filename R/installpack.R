#' installpack
#'
#' install packages if they don't exist
#' display.
#'
#' @param package_name, package name to be installed
#' @param github, if github = true
#' @note \code{installpack}
#'
#' @examples
#'     x <- installpack()
#'
#' @export
#'
installpack <- function(package_name = NULL, github = FALSE) {
    if (is.null(package_name)) return(NULL)
   
    if (!loadpack(package_name))
    {
       if (github != FALSE) { 
            devtools::install_github(github)
       } else {
           update.packages(ask= FALSE) #update installed packages.
           eval (parse(text = sprintf("install.packages(\"%s\", 
               dependencies = TRUE)",  package_name)))
           source("http://bioconductor.org/biocLite.R")
           biocLite(character(), ask=FALSE) #update installed packages.
           eval(parse(text = sprintf("biocLite(\"%s\")", package_name)))
           eval(parse(text = sprintf("require(\"%s\")", package_name)))
       }
    }
    loadpack(package_name)
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

#' loadpacks
#'
#' load initial packages 
#'
#' @note \code{loadpack}
#'
#' @examples
#'     x <- loadpacks()
#'
#' @export
#'
loadpacks <- function (){
    installpack("googleAuthR", "UMMS-Biocore/googleAuthR")   
    installpack("googleID", "UMMS-Biocore/googleID")
    installpack("shinyStore", "trestletech/shinyStore")
    return(TRUE)
}