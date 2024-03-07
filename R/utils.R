
#' Generate tiddler in json format
#'
#' @param title tiddler title
#' @param text tiddler text
#' @param type tiddler type
#' @param tags a vector for tiddler tags
#' @param fields a named vector for tiddler fields.
#' @param format export format as json or list
#'
#' @return New tiddler in json format
#' @export
tiddler_json <- function(title, text,
                         type = c("text/vnd.tiddlywiki",
                                  "text/x-tiddlywiki",
                                  "text/x-markdown",
                                  "text/html",
                                  "text/plain"),
                         tags = NULL, fields = NULL,
                         format = c("json", "list")) {
    type <- match.arg(type, several.ok = FALSE)
    format <- match.arg(format, several.ok = FALSE)
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
            body$fields[[f_names[i]]] <- jsonlite::unbox(as.character(fields[i]))
        }
    }

    if (format == "list") {
        return(body)
    }

    jsonlite::toJSON(body, auto_unbox = FALSE,
                     null = 'null', pretty = TRUE)
}


split_field <- function(v) {
    stopifnot(length(v) == 1)
    if (is.null(v)) {
        return(NULL)
    }
    v1 <- stringi::stri_extract_all(v,
                                       regex = c("\\[\\[[a-zA-Z0-9 ]+\\]\\]"))[[1]]
    if (length(v1) == 1 && is.na(v1)) {
        v1 <- NULL
    }
    v1 <- gsub("(\\[|\\])", "", v1)
    v2 <- strsplit(gsub("\\[\\[[a-zA-Z0-9 ]+\\]\\]", "", v), " +")[[1]]
    if (length(v2) == 1 && is.na(v2)) {
        v2 <- NULL
    }
    values <- unique(c(v1, v2))
    pos <- nchar(values) > 0
    values[pos]
}
