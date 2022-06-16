
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
#' @param path The folder of tiddlywiki. Temp solution as no PUT file api in tiddlywiki WebServer.
#' @param tags tiddler tags
#' @param use_bookdown logical. Use bookdown to generate markdown file.
#' @param ... Other argument pass to md_document
#' @return R Markdown output format to pass to render()
#' @export
#'
#' @examples
#' if (FALSE) {
#' library(rmarkdown)
#' render("input.Rmd")
#' }
tiddler_document <- function(host = NULL, path = NULL, tags = NULL,
                             use_bookdown = FALSE, ...) {

    # Get md document
    if (use_bookdown) {
        output <- bookdown::markdown_document2(...)
    } else {
        output <- rmarkdown::md_document(...)
    }
    # Define post processor function
    post_processor <- function(metadata, input_file, output_file, clean, verbose) {
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
        # Update image file path
        pattern <- c("^(\\!\\[\\]\\()(.+\\.\\w{3})(\\))$",
                     "^(<img +src=\")(.+\\.\\w{3})(\".*/>)$")
        i <- 1
        for (i in seq(along = pattern)) {
            pos_i <- grep(pattern[i], text)
            if (length(pos_i) > 0) {
                img_files_raw <- gsub(pattern[i], "\\2", text[pos_i])

                pos_img <- file.exists(img_files_raw)
                if (sum(!pos_img) > 0) {
                    stop("Image files do not exist: ",
                         paste(img_files_raw[!pos_img], collapse = ", "))
                }
                # Copy files to node.js server. Will update if there are PUT file API.
                if (!is.null(path)) {
                    target_files <- file.path(path, "files",
                                              img_files_raw)
                    dir_names <- unique(dirname(target_files))
                    for (j in seq(along = dir_names)) {
                        if (!dir.exists(dir_names[j])) {
                            dir.create(dir_names[j], recursive = TRUE)
                        }
                    }

                    file.copy(img_files_raw,
                              target_files,
                              overwrite = TRUE)
                }
                text[pos_i] <- gsub(pattern[i], "\\1./files/\\2\\3", text[pos_i])
            }
        }
        text <- .pretty_link(text)
        body <- .tiddler_json(title = title,
                              text = text,
                              type = "text/x-markdown",
                              tags = tags)

        output_file <- file.path(dirname(output_file),
                                 paste0(tools::file_path_sans_ext(basename(output_file)), ".json"))
        writeLines(body, output_file)
        # Push into server if host is specified
        if (!is.null(host)) {
            tw_options(host = host)
            put_tiddler(title = title,
                        text = text,
                        type = "text/x-markdown",
                        tags = tags)
        }
        output_file
    }
    output$post_processor <- post_processor
    return(output)
}
