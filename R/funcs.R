#' push
#'
#' Push an object to the list.
#'
#' @param l, that are going to push to the list
#' @param ..., list object
#' @return combined list
#'
#' @export
#'
#' @examples
#'     mylist <- list()
#'     newlist <- push ( 1, mylist )
push <- function(l, ...) c(l, list(...))

#' round_vals
#'
#' Plot PCA results.
#'
#' @param l, the value
#' @return round value
#' @export
#'
#' @examples
#'     x<-round_vals(5.1323223)
round_vals <- function(l) {
    l <- round(as.numeric(l), digits = 2)
    parse(text = l)
}

#' getDomains
#'
#' Get domains for the main plots.
#'
#' @param filt_data, data to get the domains
#' @return domains
#' @export
#'
#' @examples
#'     x<-getDomains()
getDomains <- function(filt_data = NULL){
    if (is.null(filt_data)) return (NULL)
    a <- unique(filt_data$Legend)
    a <- a[a != ""]
    if (length(a) == 1)
        a <- c(a, "NA")
    a
}

#' getColors
#'
#' get colors for the domains 
#'
#' @param domains, domains to be colored
#' @return colors
#' @export
#'
#' @examples
#'     x<-getColors()
#'
getColors <- function(domains = NULL){
    if (is.null(domains)) return (NULL)
    colors <- c()
    for ( dn in seq(1:length(domains)) ){
    if (domains[dn] == "NS" || domains[dn] == "NA")
        colors <- c(colors, "#aaa")
    else if (domains[dn] == "Up")
        colors <- c(colors, "green")
    else if (domains[dn] == "Down")
        colors <- c(colors, "red")
    else if (domains[dn] == "MV")
        colors <- c(colors, "orange")
    else if (domains[dn] == "GS")
        colors <- c(colors, "blue")
    } 
    colors
}

#' Get the logged in user's email and other info
#' 
#' @param id ID of the person to get the profile data for. 'me' to get current user.
#' 
#' @return A People resource
#' 
#' https://developers.google.com/+/web/api/rest/latest/people#resource-representations
#' 
#' @seealso https://developers.google.com/+/web/api/rest/latest/people
#' 
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' options(googleAuthR.scopes.selected = 
#'    c("https://www.googleapis.com/auth/userinfo.email",
#'      "https://www.googleapis.com/auth/userinfo.profile"))
#'                                         
#' googleAuthR::gar_auth()
#' 
#' ## default is user logged in
#' user <- get_user_info()
#' }
#' 
get_user_info <- function(id = "me"){
    
    
    url <- sprintf("https://www.googleapis.com/plus/v1/people/%s", id)
    
    g <- googleAuthR::gar_api_generator(url, "GET")
    
    req <- g()
    
    req$content
    
}

#' Whitelist check
#' 
#' After a user logs in, check to see if they are on a whitelist
#' 
#' @param user_info the object returned by \link{get_user_info}
#' @param whitelist A character vector of emails on whitelist
#' 
#' @return TRUE if on whitelist or no whitelist, FALSE if not
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' options(googleAuthR.scopes.selected = 
#'    c("https://www.googleapis.com/auth/userinfo.email",
#'      "https://www.googleapis.com/auth/userinfo.profile"))
#'                                         
#' googleAuthR::gar_auth()
#' 
#' ## default is user logged in
#' user <- get_user_info()
#' 
#' the_list <- whitelist(user, c("your@email.com", 
#'                               "another@email.com", 
#'                               "yet@anotheremail.com"))
#' 
#' if(the_list){
#'   message("You are on the list.")
#' } else {
#'   message("If you're not on the list, you're not getting in.")
#'}
#' 
#' 
#' 
#' }
whitelist <- function(user_info, whitelist = NULL){
    
    if(user_info$kind != "plus#person"){
        stop("Invalid user object used for user_info")
    }
    
    out <- FALSE
    
    if(is.null(whitelist)){
        message("No whitelist found")
        out <- TRUE
    }
    
    check <- user_info$emails$value
    
    if(is.null(check)){
        stop("No user email found")
    }
    
    if(any(check %in% whitelist)){
        message(check, " is in whitelist ")
        out <- TRUE
    } else {
        message(check, " is NOT on whitelist")
    }
    
    out
    
}

