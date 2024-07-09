#' Get all tiddlers
#'
#' @param filter filter identifying tiddlers to be returned (optional, defaults to "[all[tiddlers]!is[system]sort[title]]")
#' @param exclude comma delimited list of fields to excluded from the returned tiddlers (optional, defaults to "text")
#' @param recipe string defining which recipe to read from (optional, defaults to "default")
#' @return all tiddlers information in JSON format
#' @export
#' @examples
#' \dontrun{
#' #' Get all tiddlers
#' get_tiddlers()
#' }
get_tiddlers <- function(filter = NULL,
                         exclude = NULL,
                         recipe = "default") {
    query <- list()
    if (!is.null(filter)) {
        query$filter = filter
    }

    if (!is.null(exclude)) {
        query$exclude = exclude
    }


    response <- request(httr::GET, '/recipes/', recipe, '/tiddlers.json', query = query)
    httr::stop_for_status(response)
    response <- httr::content(response)
    response

}


#' Get a tiddler
#'
#' @param title  title of the tiddler to retrieve
#' @param recipe string defining which recipe to read from (optional, defaults to "default")
#' @return tiddler information in JSON format
#' @export
#' @examples
#' \dontrun{
#' get_tiddler("GettingStarted")
#' }
get_tiddler <- function(title, recipe = "default") {
    response <- request(httr::GET, paste0('/recipes/', recipe, '/tiddlers/',
                                          title))
    #httr::stop_for_status(response)
    response <- httr::content(response)
    if (length(response) == 0) {
        return(NULL)
    }
    # tags
    response$tags <- split_field(response$tags)
    response
}

#' Get rendered tiddler
#'
#' @param title  title of the tiddler to retrieve
#' @return tiddler body as rendered
#' @export
#' @examples
#' \dontrun{
#' get_rendered_tiddler("GettingStarted")
#' }
get_rendered_tiddler <- function(title) {
    response <- request(httr::GET, paste0('/', title))

    #httr::stop_for_status(response)
    response <- httr::content(response, as = "text")
    if (length(response) == 0) {
        return(NULL)
    }
    response
}

#' Put a tiddler
#'
#' @param title tiddler title
#' @param text tiddler text
#' @param type tiddler type
#' @param tags tiddler tags which is merged with existing tags
#' @param fields a named vector for tiddler fields which is merged with existing tags
#' @param recipe string defining which recipe to write to (optional, defaults to "default")
#' @return null if success
#' @export
#' @examples
#' \dontrun{
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
                        fields = NULL,
                        recipe = "default") {
    type <- match.arg(type)
    # Check existing tiddler
    old_tiddler <- get_tiddler(title)
    if (!is.null(old_tiddler)) {
        if (missing(text)) {
            if (is.null(old_tiddler$text)) {
                text <- ""
            } else {
                text <- old_tiddler$text
            }
        }
        if (missing(type)) {
            type <- old_tiddler$type
        }
        tags <- unique(c(tags, old_tiddler$tags))
        if (!is.null(old_tiddler$fields)) {
            fields <- utils::modifyList(old_tiddler$fields, as.list(fields))
        }
    } else {
        old_tiddler <- list(type = type)
    }
    new_tiddler <- list(title = title, text = text, type = type, tags = tags,
                        fields = fields)
    new_tiddler <- utils::modifyList(old_tiddler, new_tiddler)

    new_tiddler$modified <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y%m%d%H%M%S000")
    if (is.null(new_tiddler$created)) {
        new_tiddler$created <- new_tiddler$modified
    }
    body <- tiddler_json2(new_tiddler)
    response <- request(httr::PUT,
                        path = paste0('/recipes/', recipe, '/tiddlers/', title),
                        body = body,
                        config = httr::add_headers(`x-requested-with` = "TiddlyWiki"),
                        encode = 'json')
    httr::stop_for_status(response)
    response <- httr::content(response)
}



#' Delete a tiddler
#'
#' @param title  title of the tiddler to retrieve
#' @param bag string defining which recipe to write to (optional, defaults to "default")
#' @return no return values
#' @export
#' @examples
#' \dontrun{
#' delete_tiddler("GettingStarted")
#' }
delete_tiddler <- function(title, bag = "default") {
    response <- request(httr::DELETE,
                        path = paste0('/bags/', bag, '/tiddlers/', title),
                        config = httr::add_headers(`x-requested-with` = "TiddlyWiki"))
    httr::stop_for_status(response)
    response <- httr::content(response)
    return(invisible())
}
