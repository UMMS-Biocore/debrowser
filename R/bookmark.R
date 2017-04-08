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
    past_state_path <- "shiny_saves/past_state.txt"
    if(!is.null(username) && (username != "")){
        new_state_id <- paste0(username, "0u0",
                               new_state_id)
        user_addition <- paste0("&username=", username)
        f_path <- paste0("shiny_saves/", username, "/past_saves.txt")
        startup_path <- paste0("shiny_saves/", 
            username ,"/startup.rds")
        past_state_path <- paste0("shiny_saves/", 
            username, "/past_state.txt")
    }
    
    bookmark_dir <- "shiny_bookmarks/"
    # Get the old state id from the past state file
    conn <- file(past_state_path,open="r")
    old_state_id <- readLines(conn)
    close(conn)
    
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

#' getJsonObj
#'
#' getJsonVars  
#'
#' @param session, session
#' @param input, input
#' @param access_token, access_token
#' @examples
#'     x <- getJsonObj()
#'
#' @export
#'
getJsonObj <- function(session = NULL, input = NULL,
    access_token = NULL)
{
    if (is.null(session)) return (NULL)
    loadingJSON <- reactiveValues(username = "")
    
    user_details <- reactive({
        if(!is.null(access_token)){
            with_shiny(get_user_info, shiny_access_token = access_token)
        }
    })
    
    json_obj <- parseQueryString(session$clientData$url_search)[['jsonobject']]
    # coming from json
    if(!is.null(json_obj) && (json_obj != "")){
        loadingJSON$username <- parseQueryString(session$clientData$url_search)[['username']]
        updateStore(session, "text",
            isolate(loadingJSON$username))
    } else{
        user_email <- user_details()$emails$value
        username_from_email <- gsub("[[:punct:]]", "", user_email)
        # just logged in via google
        if(!is.null(user_email) && (username_from_email != "")){
            loadingJSON$username <- username_from_email
            updateStore(session, "text",
                isolate(loadingJSON$username))
        } else{
            # Check local storage
            if(!is.null(input$store$text) && (input$store$text != "")){
                start_state <- parseQueryString(session$clientData$url_search)[['start']]
                if(!is.null(start_state) && (start_state != "") &&
                   start_state == "true"){
                    loadingJSON$username <- input$store$text
                }
                else{
                    state_id_current <- parseQueryString(session$clientData$url_search)[['_state_id_']]
                    if(is.null(state_id_current) || (state_id_current == "") ||
                       grepl(input$store$text, state_id_current)){
                        # Own bookmark
                        loadingJSON$username <- input$store$text
                    }
                    else
                    {
                        loadingJSON$username <- "Local"
                    }
                }
            }
        }
    }
    loadingJSON
}


#' bookmarkUI
#'
#' bookmark UI  
#'
#' @param id, id
#' @examples
#'     x <- bookmarkUI()
#'
#' @export
#'
bookmarkUI <- function(id = NULL) {
    if (is.null(id)) return (NULL)
    ns <- NS(id)

    list(conditionalPanel(condition = paste0("((input.goDE) || ",
        "(output.restore_DE > 0)) && (!input.startDE)"),
        style = "padding: 27px;",
        textInput(ns("bookmark_special_name"), "Name your save:",
        value = "", placeholder = "At Least 5 Characters"),
        actionButton(ns("name_bookmark"), "Submit!"),
        textOutput(ns("bookmark_length_error"))),
        conditionalPanel(condition <- paste0("input.methodtabs=='panel0'"),
        htmlOutput(ns("new_bookmark")),
        uiOutput(ns("past_named_bookmarks")),
        lapply(20:1, function(i) {
          uiOutput(ns(paste0('bookmark', i)))
    }))
    )
}

#' bookmarkServer
#'
#' bookmark Server functions
#'
#' @param input, input
#' @param output, output
#' @param session, session
#' @param loadingJSON, loadingJSON
#' @examples
#'     x <- bookmarkServer()
#'
#' @export
#'
bookmarkServer <- function(input = NULL, output  = NULL, 
    session  = NULL, loadingJSON = NUL) {
    if (is.null(input)) return (NULL)
    bookmark_list <- reactive({
        username <- loadingJSON$username
        past_saves_path <- "shiny_saves/past_saves.txt"
        if(!is.null(username) && username != ""){
            past_saves_path <- paste0("shiny_saves/", username, "/past_saves.txt")
        }
        if (file.exists(past_saves_path)){
            if (file.size(past_saves_path) > 0){
                query <- parseQueryString(session$clientData$url_search)
                json_addition <- ""
                if(!is.null(query$jsonobject)){
                    json_addition <- "&jsonobject=saved"
                }
                conn <- file(past_saves_path,open="r")
                lines <- readLines(conn)
                bookmark_count <- length(lines)
                output$past_named_bookmarks <- renderText({"History:"})
                lapply(1:bookmark_count, function(i) {
                    a <- strsplit(lines[i], "0u0")
                    if(length(a[[1]]) == 2){
                        to_show <- a[[1]][2]
                        user_addition <- paste0("&username=", a[[1]][1])
                    } else{
                        to_show <- lines[i]
                        user_addition <- ""
                    }
                    
                    output[[paste0('bookmark', i)]] <- renderUI({
                        list(
                            a(paste0(to_show), class="bm_id",
                            href= paste0("?_state_id_=", lines[i],
                            user_addition, json_addition)),
                            a(href="#", id=paste0("bm-remove_bm", i), 
                            class="removebm",
                            img(src="www/images/delete_button.png"),
                            onclick=paste0('removeBookmark("',  
                            lines[i], '", "',loadingJSON$username,'")'))
                        )
                    })
                })
                close(conn)
            }
        } else {
            output$past_named_bookmarks <- renderUI({list()})
        }
    })

    ###############################################################
    #           Bookmark on every single user input               #
    ###############################################################
    observe({
        startup_path <- "shiny_saves/startup.rds"
        if (!is.null(loadingJSON$username) && loadingJSON$username != ""){
            startup_path <- paste0("shiny_saves/", 
                loadingJSON$username ,"/startup.rds")
        }
        startup <- list()
        if(file.exists(startup_path)){
            startup <- readRDS(startup_path)
        }
        if(is.null(startup[['bookmark_counter']])){
            startup[['bookmark_counter']] <- 3
        }
        if(startup[['bookmark_counter']] == 0){
            startup[['bookmark_counter']] <- 1
            saveRDS(startup, startup_path)
            session$sendCustomMessage(type = 'testmessage',
                message = list(new_url = paste0("?_state_id_=",
                startup[['startup_bookmark']]), controller = input$controller))
        }
        bookmark_list()
    })
    
    ###############################################################
    #         To save user chosen name as bookmark id             #
    ###############################################################
    observeEvent(input$name_bookmark, {
        session$doBookmark()
        username <- loadingJSON$username
        chosen_name <- input$bookmark_special_name
        if(nchar(chosen_name) < 5){
            to_display <- "You must type in at least 5 characters."
        } else if(!grepl('^[A-Za-z0-9]+$', chosen_name)){
            to_display <- "You can only use numbers and English letters."
        } else if(grepl("0u0", chosen_name)){
            to_display <- "You cannot use '0u0' in the name."
        } else {
            output$bookmark_length_error <- renderText({""})
            result <- copy2newDirectory(chosen_name, loadingJSON$username, session)
            if(result == 35){
                to_display <- paste0(chosen_name, " is already saved.")
            } else {
                if (result == 42) {
                    shinyjs::hide("bm-bookmark_special_name")
                    shinyjs::hide("bm-name_bookmark")
                    #shinyjs::hide("message_for_loading")
                    user_addition <- ""
                    if(!is.null(username) && (username != "")){
                        user_addition <- paste0("&username=", username)
                    }
                    query_list <- parseQueryString(session$clientData$url_search)
                    chosen_link <- chosen_name
                    if(!is.null(loadingJSON$username) && (loadingJSON$username != "")){
                        chosen_link <- paste0(loadingJSON$username, "0u0",
                                              chosen_name)
                    }
                    
                    old_bookmark_id <- parseQueryString(session$clientData$url_search)[["_state_id_"]]
                    old_json_path <- paste0("shiny_bookmarks/", old_bookmark_id, "/file1.JSON")
                    if(file.exists(old_json_path)){
                        file.copy(old_json_path, paste0("shiny_bookmarks/", chosen_link, "/file1.JSON"))
                    }
                    old_tsv_path <- paste0("shiny_bookmarks/",
                                           old_bookmark_id, "/file1.tsv")
                    if(file.exists(old_tsv_path)){
                        file.copy(old_tsv_path, paste0("shiny_bookmarks/", 
                                                       chosen_link, "/file1.tsv"), overwrite = TRUE)
                    }
                    query <- parseQueryString(session$clientData$url_search)
                    json_addition <- ""
                    if(!is.null(query$jsonobject)){
                        json_addition <- "&jsonobject=saved"
                    }
                    
                    bm_link <- paste0('<p style="margin-left: 27px;">New Save:</p><a style="margin: 27px;" ',
                                      ' href="?_state_id_=',
                                      chosen_link, user_addition, 
                                      json_addition, '">', chosen_name, '</a>')
                    
                    output$new_bookmark <- renderText({bm_link})
                    shinyjs::show("save_state")
                    to_display <- paste0("Successfully saved. ",
                                         "URL updated with your choice to access later.")
                } else {
                    to_display <- "Something went wrong with the save."
                }
            }
        }
        output$bookmark_length_error <- renderText({ to_display })
    })
    output$isRestoring <- reactive({
        startup_path <- "shiny_saves/startup.rds"
        if(!is.null(loadingJSON()$username) && loadingJSON()$username != ""){
            startup_path <- paste0("shiny_saves/", 
                                   loadingJSON()$username ,"/startup.rds")
        }
        startup <- readRDS(startup_path)
        return(startup[['bookmark_counter']] == 2)
    })
    # Read values from state$values when we restore
    onRestore(function(state) {
        log_out <- parseQueryString(session$clientData$url_search)[['logout']]
        if(!is.null(log_out) && (log_out != "")){
            updateStore(session, "text",
                        isolate(""))
        } else {
            query_list <- parseQueryString(session$clientData$url_search)
            dir_to_create <- paste0("shiny_saves/", loadingJSON$username)
            if(!file.exists(dir_to_create)){
                dir.create(dir_to_create)
            }
            startup_path <- "shiny_saves/startup.rds"
            if(!is.null(loadingJSON$username) && loadingJSON$username != ""){
                startup_path <- paste0("shiny_saves/", 
                   loadingJSON$username ,"/startup.rds")
            }
            startup <- list()
            startup[['bookmark_counter']] <- 2
            startup[['startup_bookmark']] <- query_list[["_state_id_"]]
            
            if(!is.null(query_list[["_state_id_"]])){
                saveRDS(startup, startup_path)
                saveRDS(state$values$input_save, paste0("shiny_bookmarks/", 
                     query_list[["_state_id_"]] , "/input_save.rds"))
            }
        }
        #Restoring 
    })
    
}
