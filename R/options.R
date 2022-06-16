# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   03:40 PM Saturday, 09 June 2018
# * Copyright: AS IS



# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
TW_OPTIONS <- settings::options_manager(
    host = "http://127.0.0.1:8080/"
)


#' Set or get options for my package
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{host}}{\code{character} host of tiddlywiki}
#' }
#'
#' @export
tw_options <- function(...){
    # protect against the use of reserved words.
    settings::stop_if_reserved(...)
    TW_OPTIONS(...)
}

#' Reset global options for pkg
#'
#' @export
tw_reset <- function() {
    settings::reset(TW_OPTIONS)
}