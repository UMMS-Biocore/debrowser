#' getDownloadSection
#'
#' download section button and dataset selection box in the
#' menu for user to download selected data.
#'
#' @param flag, to show the download selection
#' @param type, main vs. QC section
#'
#' @note \code{getDownloadSection}
#' @return the panel for download section in the menu;
#'
#' @examples
#'     x<- getDownloadSection()
#'
#' @export
#'
getDownloadSection <- function(flag = FALSE, choices=NULL) {
    a <- NULL
    if (flag){
        a <- list(conditionalPanel( (condition <- "input.methodtabs!='panel0'"),
                shinydashboard::menuItem("Select Plot Options", icon = icon("star-o"),                       
                    selectInput("dataset", "Choose a dataset:",
                    choices = choices), 
                    selectInput("norm_method", "Normalization Method:",
                        choices <- c("TMM", "RLE", "upperquartile", "none")),
                    downloadButton("downloadData", "Download Data"),
                    conditionalPanel(condition = "input.dataset=='most-varied'",
                    textInput("topn", "top-n", value = "500" ), 
                    textInput("mincount", "total min count", value = "10" )),
                    textareaInput("genesetarea","Search", 
                                  "", rows = 5, cols = 35),
                    helpText("Regular expressions can be used\n
                             Ex: ^Al => Al.., Al$ => ...al")
                )
            ))
    }
    a
}
