
#' Test whether server can be connected
#'
#' @return TRUE if test server is available.
is_test_tw <- function() {
    x <- try({
        status <- request("GET", "status")
        if (!is.null(status)) {
            return(TRUE)
        }
    })
    if (inherits(x, "try-error")) {
        return(FALSE)
    }
    return(FALSE)
}

