
#' Generate tiddler in json format
#'
#' @param title tiddler title
#' @param text tiddler text
#' @param type tiddler type
#' @param tags a vector for tiddler tags
#' @param fields a named vector for tiddler fields.
#'
#' @return New tiddler in json format
.tiddler_json <- function(title, text,
                          type = c("text/vnd.tiddlywiki",
                                   "text/x-tiddlywiki",
                                   "text/x-markdown",
                                   "text/html",
                                   "text/plain"),
                          tags = NULL, fields = NULL) {
    match.arg(type)
    if (!is.null(tags)) {
        if (!is.vector(tags)) {
            stop("tags should be a vector.")
        }
        if (length(tags) < 1) {
            stop("tags should have at least one value.")
        }
        tags <- paste(paste0("[[", tags, "]]"), collapse = " ")
    }
    if (!is.null(fields)) {
        f_names <- names(fields)
        if (is.null(f_names) | sum(nchar(f_names) == 0) > 0) {
            stop("fields should be a named vector")
        }
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
                 type = jsonlite::unbox(type),
                 # created = jsonlite::unbox(created),
                 # created = jsonlite::unbox(created),
                 tags = jsonlite::unbox(tags))
    if (!is.null(fields)) {
        f_names <- names(fields)
        if (is.null(f_names) | sum(nchar(f_names) == 0) > 0) {
            stop("fields should be a named vector")
        }
        for (i in seq(along = fields)) {
            body[[f_names[i]]] <- jsonlite::unbox(as.character(fields[i]))
        }
    }

    jsonlite::toJSON(body, auto_unbox = FALSE,
                     null = 'null', pretty = TRUE)
}
