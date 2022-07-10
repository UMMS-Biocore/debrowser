#' startDEBrowser
#'
#' Starts the DEBrowser to be able to run interactively.
#'
#' @note \code{startDEBrowser}
#' @return the app
#'
#' @examples
#'     startDEBrowser()
#'
#' @export
#'
startDEBrowser <- function(){
    if (interactive()) {
        #the upload file size limit is 30MB
        options( shiny.maxRequestSize = 30 * 1024 ^ 2, warn = -1,
                 shiny.sanitize.errors = TRUE)
        addResourcePath(prefix = "demo", directoryPath =
                        system.file("extdata", "demo", 
                        package = "debrowser"))
        addResourcePath(prefix = "www", directoryPath =
                        system.file("extdata", "www", 
                        package = "debrowser"))
        environment(deServer) <- environment()

        app <- shinyApp( ui = shinyUI(deUI),
                    server = shinyServer(deServer))
        runApp(app)
    }
}

#' startHeatmap
#'
#' Starts the DEBrowser heatmap
#'
#' @note \code{startHeatmap}
#' @return the app
#'
#' @examples
#'     startHeatmap()
#'
#' @export
#'
startHeatmap <- function(){
    if (interactive()) {
        #the upload file size limit is 30MB
        options( shiny.maxRequestSize = 30 * 1024 ^ 2, warn = -1,
                 shiny.sanitize.errors = TRUE)
        addResourcePath(prefix = "demo", directoryPath =
                            system.file("extdata", "demo", 
                                        package = "debrowser"))
        addResourcePath(prefix = "www", directoryPath =
                            system.file("extdata", "www", 
                                        package = "debrowser"))
        environment(heatmapServer) <- environment()

        app <- shinyApp( ui = shinyUI(heatmapUI),
                         server = shinyServer(heatmapServer))
        runApp(app)
    }
}
