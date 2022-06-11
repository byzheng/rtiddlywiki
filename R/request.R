


#' Perform a request to TiddlyWiki WebServer
#'
#' @param method The method in the httr package, e.g. GET, POST
#' @param path The path of request
#' @param query The query of request
#' @param ... Other arguments of request
#'
#' @return The contents of response
#' @export
request <- function(method,
                           path = '/',
                           query = list(),
                           ...) {
    # Remove the leading "/" if it has one.
    path <- utils::URLencode(gsub('^/*(.*)$', "\\1", path))

    host <- TW_OPTIONS("host")


    url <- httr::modify_url(host,
                      path = gsub("/+", "/",
                                  paste(httr::parse_url(host)$path, path, sep = "/")))
    response <- method(url, query = query, ...)
    response
}

