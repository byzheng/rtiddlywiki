
#' Test whether server can be connected
#'
#' @return TRUE if test server is available.
is_test_tw <- function() {
    status <- request(httr::GET, "status")
    if (httr::status_code(status) == "200") {
        return(TRUE)
    }
    return(FALSE)
}

