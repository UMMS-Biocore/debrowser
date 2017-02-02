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
        
        .GlobalEnv$.bookmark.startup <- system(paste0("ls -t1 shiny_bookmarks",
            " |  head -n 1"), intern=TRUE)
        on.exit(rm(.bookmark.startup, envir=.GlobalEnv))
        
        .GlobalEnv$.bm.counter <- 0
        on.exit(rm(.bm.counter, envir=.GlobalEnv))
        
        app <- shinyApp( ui = shinyUI(deUI),
                    server = shinyServer(deServer), 
                    enableBookmarking = "server")
        runApp(app)
    }
}
