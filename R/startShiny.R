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
        options( shiny.maxRequestSize = 30 * 1024 ^ 2)
        addResourcePath(prefix = "demo", directoryPath =
                        system.file("extdata", "demo", 
                        package = "debrowser"))
        addResourcePath(prefix = "www", directoryPath =
                        system.file("extdata", "www", 
                        package = "debrowser"))
        environment(deServer) <- environment()
        #shinyAppDir(system.file(package="debrowser"))
        app <- shinyApp( ui = shinyUI(deUI),
                    server = shinyServer(deServer), 
                    enableBookmarking = "server")
        runApp(app)
    }
}
