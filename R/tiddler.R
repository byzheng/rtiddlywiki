#' Get all tiddlers
#'
#' @param filter filter identifying tiddlers to be returned (optional, defaults to "[all[tiddlers]!is[system]sort[title]]")
#' @param exclude comma delimited list of fields to excluded from the returned tiddlers (optional, defaults to "text")
#' @return all tiddlers information in JSON format
#' @export
#' @examples
#' if (FALSE) {
#' #' Get all tiddlers
#' get_tiddlers()
#' }
get_tiddlers <- function(filter = NULL,
                       exclude = NULL) {
    query <- list()
    if (!is.null(filter)) {
        query$filter = filter
    }

    if (!is.null(exclude)) {
        query$exclude = exclude
    }


    response <- request(httr::GET, '/recipes/default/tiddlers.json', query = query)
    httr::stop_for_status(response)
    response <- httr::content(response)
    response

}


#' Get a tiddler
#'
#' @param title  title of the tiddler to retrieve
#' @return tiddler information in JSON format
#' @export
#' @examples
#' if (FALSE) {
#' get_tiddler("GettingStarted")
#' }
get_tiddler <- function(title) {
    response <- request(httr::GET, paste0('/recipes/default/tiddlers/',
                                          title))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response

}


#' Put a tiddler
#'
#' @param title tiddler title
#' @param text tiddler text
#' @param type tiddler type
#' @param tags tiddler tags
#' @param fields a named vector for tiddler fields
#' @return null if success
#' @export
#' @examples
#' if (FALSE) {
#' title <- "New tiddler"
#' text <- c("!! Section",
#'           "This is a new tiddler")
#' type <- "text/vnd.tiddlywiki"
#' tags <- c("Tag1", "Tag 2")
#' fields <- c("F1" = "V1", "F2" = "V2")
#' put_tiddler(title = title,
#'             text = text,
#'             type = type,
#'             tags = tags,
#'             fields = fields)
#' }
put_tiddler <- function(title, text,
                        type = c("text/vnd.tiddlywiki",
                                 "text/x-tiddlywiki",
                                 "text/x-markdown",
                                 "text/html",
                                 "text/plain"),
                        tags = NULL,
                        fields = NULL) {
    body <- .tiddler_json(title = title, text = text, type = type, tags = tags)
    response <- request(httr::PUT,
                        path = paste0('/recipes/default/tiddlers/',
                                          title),
                        body = body,
                        config = httr::add_headers(`x-requested-with` = "TiddlyWiki"),
                        encode = 'json')
    httr::stop_for_status(response)
    response <- httr::content(response)
    response

}


#' Get a rendered tiddler
#'
#' @param title  title of the tiddler to retrieve
#' @return tiddler rendering in html
#' @export
get_rendered_tiddler <- function(title) {
    #response <- request(httr::GET, utils::URLencode(title))
    #httr::stop_for_status(response)
    #response <- httr::content(response)
    #response
}

