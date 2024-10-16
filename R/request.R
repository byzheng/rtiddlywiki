


#' Perform a request to TiddlyWiki WebServer
#'
#' @param method The method in the httr package, e.g. GET, POST
#' @param path The path of request
#' @param query The query of request
#' @param ... Other arguments of request
#'
#' @return The contents of response
request <- function(method,
                           path = '/',
                           query = list(),
                           ...) {
    httr::set_config(httr::config(ssl_verifypeer = 0L))
    # Remove the leading "/" if it has one.
    path <- utils::URLencode(gsub('^/*(.*)$', "\\1", path))
    host <- TW_OPTIONS("host")


    config <- list()

    http_x_auth_key <- TW_OPTIONS("http_x_auth_key")

    if (nchar(http_x_auth_key) > 0) {
        config <- httr::add_headers("X-Auth-Key" = http_x_auth_key)
    }
    url <- httr::modify_url(host,
                      path = gsub("/+", "/",
                                  paste(httr::parse_url(host)$path, path, sep = "/")))
    response <- method(url, config = config, query = query, ...)
    response
}

