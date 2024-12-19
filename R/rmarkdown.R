
.get_title <- function(text) {
    pattern <- "^title: *(.+) *$"

    pos <- grepl(pattern = pattern, text)
    if (sum(pos) != 0) {
        title <- gsub(pattern, "\\1", text[pos])
        title <- gsub("^\"", "", title)
        title <- gsub("\"$", "", title)
    } else {
        title <- NULL
    }
    title
}

.pretty_link <- function(text) {
#    pattern <- "\\\\\\[\\\\\\[([a-zA-Z0-9 |]+)\\\\\\]\\\\\\]"
    pattern <- "\\\\\\[\\\\\\[([^\\[]+)\\\\\\]\\\\\\]"
    gsub(pattern, "\\[\\[\\1\\]\\]", text)
}



.markdown_link <- function(text) {
    #    pattern <- "\\\\\\[\\\\\\[([a-zA-Z0-9 |]+)\\\\\\]\\\\\\]"
    pattern <- "(\\[[a-zA-Z0-9 ]+\\]\\(#)([a-zA-Z0-9 ]+)(\\))"
    pos <- grep(pattern, text)
    f <- function(x) {
        link <- gsub(pattern, "\\2", x)
        link <- utils::URLencode(link)
        gsub(pattern, paste0("\\1", link, "\\3"), x)
    }
    i <- 1
    for (i in seq(along = pos)) {
        t_i <- text[pos[i]]
        t_i <- stringr::str_replace_all(t_i, pattern, function(m) f(m))
        text[pos[i]] <- t_i
    }
    text
}


#' Format for converting from R Markdown to another tiddler markdown
#'
#' @param host the host of tiddlywiki web server
#' @param tags tiddler tags
#' @param fields a named vector for tiddler fields
#' @param use_bookdown logical. Use bookdown to generate markdown file.
#' @param overwrite whether to overwrite the existing tiddler.
#' @param variant variant for md_document
#' @param pandoc_args pandoc_args for md_document
#' @param ... Other argument pass to md_document
#' @return R Markdown output format to pass to render()
#' @export
#'
#' @examples
#' \dontrun{
#' library(rmarkdown)
#' render("input.Rmd")
#' }
tiddler_document <- function(host = NULL,
                             tags = NULL,
                             fields = NULL,
                             use_bookdown = FALSE,
                             overwrite = FALSE,
                             variant = "gfm",
                             pandoc_args = "--wrap=none",
                             ...) {
    # Get md document
    if (use_bookdown) {
        output <- bookdown::markdown_document2(...)
    } else {
        output <- rmarkdown::md_document(variant = variant, pandoc_args = pandoc_args, ...)
    }
    # Define post processor function
    post_processor <- function(metadata, input_file, output_file, clean, verbose) {
        # save(list = ls(), file = "tmp.Rdata")
        # stop("AAAAAAAAAAAA")
        # print(input_file)
        # print(output_file)
        # file.copy(input_file, paste0(input_file, ".bck"))
        # file.copy(output_file, paste0(output_file, ".bck"))
        title <- readLines(input_file)
        title <- .get_title(title)
        if (is.null(title)) {
            title <- tools::file_path_sans_ext(basename(output_file))
        }
        text <- readLines(output_file)
        # for macro in tiddlywiki
        text <- gsub("&lt;<", "<<", text)
        text <- gsub("&lt;&lt;", "<<", text)
        text <- gsub(">&gt;", ">>", text)
        text <- gsub("&gt;&gt;", ">>", text)
        text <- gsub("\u201C", '"', text)
        text <- gsub("\u201D", '"', text)
        text <- gsub("\\\\<", '<', text)
        text <- gsub("\\\\>", '>', text)
        text <- gsub("\\\\\\$", "\\$", text)
        # Update image file path
        pattern <- c("^(\\!\\[\\]\\()(.+\\.\\w{3})(\\)).*$",
                     "^(<img +src=\")(.+\\.\\w{3})(\".*/>)$")
        i <- 1
        for (i in seq(along = pattern)) {
            # save(list = ls(), file = "a.Rdata")
            # stop()
            # vars <- load("examples/a.Rdata")
            pos_i <- grep(pattern[i], text)
            if (length(pos_i) == 0) {
                next
            }
            j <- 1
            for (j in seq_along(pos_i)) {
                img_files_raw <- gsub(pattern[i], "\\2", text[pos_i[j]])
                #img_files_raw <- file.path("examples", img_files_raw)

                pos_img <- file.exists(img_files_raw)
                if (sum(!pos_img) > 0) {
                    stop("Image files do not exist: ",
                         paste(img_files_raw[!pos_img], collapse = ", "))
                }
                img_uri <- knitr::image_uri(img_files_raw)
                text[pos_i[j]] <- gsub(pattern[i], paste0("\\1", img_uri, "\\3"), text[pos_i[j]])
            }
        }
        # save(list = ls(), file = "a.Rdata")
        # stop()
        # vars <- load("examples/a.Rdata")
        text <- .pretty_link(text)
        text <- paste(text, collapse = "\n")
        body <- tiddler_json(title = title,
                              text = text,
                              type = "text/x-markdown",
                              tags = tags,
                              fields = fields)

        output_file <- file.path(dirname(output_file),
                                 paste0(tools::file_path_sans_ext(basename(output_file)), ".json"))
        writeLines(body, output_file)
        # Push into server if host is specified
        if (!is.null(host)) {
            tw_options(host = host)
            res <- get_tiddler(title = title)
            if (length(res) > 0 && !overwrite) {
                stop("Existed tiddler with title: ", title)
            } else if (length(res) > 0 && overwrite) {
                warning("Existed tiddler with title: ", title)
            }
            put_tiddler(title = title,
                        text = text,
                        type = "text/x-markdown",
                        tags = tags,
                        fields = fields)
        }
        output_file
    }
    output$post_processor <- post_processor
    return(output)
}
