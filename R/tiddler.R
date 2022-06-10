#' Get all tiddlers
#'
#' @param filter filter identifying tiddlers to be returned (optional, defaults to "[all[tiddlers]!is[system]sort[title]]")
#' @param exclude comma delimited list of fields to excluded from the returned tiddlers (optional, defaults to "text")
#' @return all tiddlers information in JSON format
#' @export
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
get_tiddler <- function(title) {
    response <- request(httr::GET, paste0('/recipes/default/tiddlers/',
                                          utils::URLencode(title)))
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

