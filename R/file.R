#' Get a file
#'
#' @param path  file path
#' @return data retrieved from file
#' @export
get_file <- function(path) {
    response <- request(httr::GET, paste0('/files/',
                                          utils::URLencode(path)))
    httr::stop_for_status(response)
    response <- httr::content(response)

}


