
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


#' Split tiddlywiki field into values
#'
#' @param s a string
#'
#' @return an vector of values
#' @export
split_field <- function(s) {
    stopifnot(!is.null(s))
    stopifnot(length(s) == 1)
    if (nchar(s) == 0) {
        return(NULL)
    }

    s <- stringi::stri_trim_both(s)
    v <- c()
    s_i <- s
    while(TRUE) {
        pos_s <- 1
        spliter <- " "
        if (grepl("^\\[\\[", s_i)) {
            spliter <- "\\]\\]( |$)"
            pos_s <- 3
        }
        pos_n <- stringi::stri_locate_first_regex(s_i, spliter)

        pos_n <- as.numeric(pos_n)
        if (sum(is.na(pos_n)) > 0) {
            pos_n <- nchar(s_i)
        } else {
            pos_n <- pos_n[1] - 1
        }
        #print(pos_n)
        v_i <- substr(s_i, pos_s, pos_n)
#        print(v_i)
        v <- c(v, v_i)
        pos_s <- pos_n + pos_s + 1
        if (pos_s > nchar(s_i)) {
            break
        }
        s_i <- substr(s_i, pos_s, nchar(s_i))
        #print(s_i)
        if (nchar(s_i) == 0) {
            break
        }
    }
    v
}
