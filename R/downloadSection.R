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
getDownloadSection <- function(flag = FALSE, type = "main") {
    a <- NULL
    if (flag){
        if (type == "main")
            choices <- c("up+down", "up", "down", "selected",
                "comparisons", "alldetected", "most-varied", "pcaset")
        else 
            choices <- c("most-varied", "alldetected", "selected", "pcaset")
        a <- list(conditionalPanel( (condition <- "input.methodtabs!='panel0'"),
                shinydashboard::menuItem("Main_Plots_Option2", icon = icon("star-o"),                       
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
