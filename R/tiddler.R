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
                                          title))
    httr::stop_for_status(response)
    response <- httr::content(response)
    response

}


#' Put a tiddler
#'
#' @param title tiddler title
#' @param text contents of tiddler
#' @param tags tags of tiddler
#' @return null if success
#' @export
put_tiddler <- function(title, text, tags = NULL) {

    created <- as.character(as.numeric(format(Sys.time(), "%Y%m%d%H%M%OS3")) * 1000)
    if (is.null(tags)) {
        if (!is.vector(tags)) {
            stop("tags should be a vector.")
        }
        if (length(tags) < 1) {
            stop("tags should have at least one value.")
        }
        tags <- paste(paste0("[[", tags, "]]"), collapse = " ")
    }
    if (is.null(title) || length(title) != 1 || !is.character(title)) {
        stop("title should be string with one item.")
    }
    if (is.null(text)) {
        stop("text should have values")
    }
    text <- paste(text, collapse = "\r\n")
    body <- list(title = jsonlite::unbox(title),
                 text = jsonlite::unbox(text),
                 type = jsonlite::unbox("text/x-markdown"),
                 created = jsonlite::unbox(created),
                 created = jsonlite::unbox(created),
                 tags = tags,
                 revision = jsonlite::unbox("0"),
                 bag = jsonlite::unbox("default"))
    response <- request(httr::PUT,
                        path = paste0('/recipes/default/tiddlers/',
                                          title),
                        body = jsonlite::toJSON(body, auto_unbox = FALSE,
                                                null = 'null'),
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

