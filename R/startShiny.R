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
        
        dir.create(file.path("shiny_saves"), showWarnings = FALSE)
        
        # Clean up the bookmark directory of unnamed bookmarks - begin
        l <- list.files("shiny_bookmarks")
        if(length(l) > 0){
            last_bookmark_id <- system(paste0("ls -t1 shiny_bookmarks",
                                              " |  head -n 1"), intern=TRUE)
            l_without_last <- setdiff(l, list(last_bookmark_id))
            if(file.exists("shiny_saves/past_saves.txt")){
                named_bookmarks <- scan("shiny_saves/past_saves.txt", what="",
                                        sep="\n")
                bookmarks_to_delete <- setdiff(l_without_last, named_bookmarks)
            } else {
                bookmarks_to_delete <- l_without_last
            }
            lapply(bookmarks_to_delete, function(x) 
                if(x != ""){
                    #unlink(paste0("shiny_bookmarks/", x), recursive = TRUE)
                }
            )
        }
        # Clean up the bookmark directory of unnamed bookmarks - end
        
        
        startup_obj <- list()
        # To restore, set counter to 0 and bookmark to the last bookmark dir
        # To start normally, set the counter to 1 and bookmark to empty string
        startup_obj$bookmark_counter <- 1
        startup_obj$startup_bookmark <- ""
        saveRDS(startup_obj, "shiny_saves/startup.rds")
        
        # to assign username as 'local' from startDEBrowser() call
        .GlobalEnv$.startdebrowser.called <- "1"
        on.exit(rm(.startdebrowser.called, envir=.GlobalEnv))
            
        app <- shinyApp( ui = shinyUI(deUI),
                    server = shinyServer(deServer), 
                    enableBookmarking = "server")
        runApp(app)
    }
}
