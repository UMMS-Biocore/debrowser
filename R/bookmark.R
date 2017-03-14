#' removeBookmark
#'
#' remove saved state 
#'
#' @param ID, prev url
#' @param username, username
#' @examples
#'     x <- removeBookmark()
#'
#' @export
#'
#'
removeBookmark <- function(ID = NULL, username = NULL){
    if (is.null(ID)) return(NULL)
    saves_path <- "shiny_saves/past_saves.txt"
    if(!is.null(username) && username != ""){
        saves_path <- paste0("shiny_saves/", username, "/past_saves.txt")
    }
    current_file <- readLines(saves_path)
    my_new_file = current_file[-ID]
    to_unlink <- paste0("shiny_bookmarks/", current_file[ID])
    unlink(to_unlink, recursive = TRUE)
    fileConn<-file(saves_path)
    writeLines(my_new_file, fileConn)
    close(fileConn)
}

#' get_state_id
#'
#' Helper to copy the bookmark to a user named directory 
#'
#' @param prev_url, prev url
#' @examples
#'     x <- get_state_id()
#'
#' @export
#'
#'
get_state_id <- function(prev_url = NULL){
    if (is.null(prev_url)) return(NULL)
    query_list <- c()
    query_string <- paste0("?", strsplit(prev_url, "?",
                                         fixed = TRUE)[[1]][2])
    query_list <- parseQueryString(query_string)
    return(query_list[["_state_id_"]])
}


#' copy2newDirectory
#'
#' To copy the bookmarked folder into a user named directory  
#'
#' @param new_state_id, new state id
#' @param username, username
#' @param session, session
#' @examples
#'     x <- copy2newDirectory()
#'
#' @export
#'
#'
copy2newDirectory <- function(new_state_id = NULL, username = NULL, 
    session = NULL){
    if (is.null(new_state_id)) return(NULL)
    query_list <- parseQueryString(session$clientData$url_search)
    user_addition <- ""
    startup_path <- "shiny_saves/startup.rds"
    f_path <- "shiny_saves/past_saves.txt"
    if(!is.null(username) && (username != "")){
        new_state_id <- paste0(username, "0u0",
                               new_state_id)
        user_addition <- paste0("&username=", username)
        f_path <- paste0("shiny_saves/", username, "/past_saves.txt")
        startup_path <- paste0("shiny_saves/", 
            username ,"/startup.rds")
    }
    
    # Get the state id from the query string
    bookmark_dir <- "shiny_bookmarks/"
    old_state_id <- system(paste0("ls -t1 shiny_bookmarks",
                                  " |  head -n 1"), intern=TRUE)
    
    if(!dir.exists(paste0(bookmark_dir, new_state_id))){
        
        if(file.rename(paste0(bookmark_dir, old_state_id), 
                       paste0(bookmark_dir, new_state_id))){
            
            if(!is.null(query_list$jsonobject)){
                download.file(query_list$jsonobject, paste0(bookmark_dir,
                    new_state_id, "/file1.JSON"))
            }
            updateQueryString(paste0("?_state_id_=", new_state_id, user_addition))
            startup <- readRDS(startup_path)
            startup[['startup_bookmark']] <- new_state_id
            saveRDS(startup, startup_path)
            write(new_state_id,file=f_path,
                  append=TRUE)
            return(42)
        }
        else{
            return(13)
        }
        
    } else {
        return(35)
    }
}
