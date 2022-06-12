

#' Format for converting from R Markdown to another tiddler markdown
#'
#' @param host the host of tiddlywiki web server
#' @param path The folder of tiddlywiki. Temp solution as no PUT file api in tiddlywiki WebServer.
#' @param tags tiddler tags
#' @return R Markdown output format to pass to render()
#' @export
#'
#' @examples
#' if (FALSE) {
#' library(rmarkdown)
#' render("input.Rmd")
#' }
tiddler_document <- function(host = NULL, path = NULL, tags = NULL) {

    # Get md document
    output <- rmarkdown::md_document()
    # Define post processor function
    post_processor <- function(metadata, input_file, output_file, clean, verbose) {
        file.copy(input_file, paste0(input_file, '.bck'))
        title <- readLines(input_file)
        pattern <- "^title: *\"(.+)\"$ *"
        pos <- grepl(pattern = pattern, title)
        if (sum(pos) != 0) {
            title <- gsub(pattern, "\\1", title[pos])
        } else {
            title <- tools::file_path_sans_ext(basename(output_file))
        }
        text <- readLines(output_file)
        # Update image file path
        pattern <- "^(\\!\\[\\]\\()(.+\\.png)(\\))$"

        pos <- grepl(pattern, text)
        if (sum(pos) > 0) {
            img_files_raw <- gsub(pattern, "\\2", text[pos])
            img_files_new <- img_files_raw
            pos_img <- file.exists(img_files_new)
            if (sum(!pos_img) > 0) {
                stop("Image files do not exist: ",
                     paste(img_files_new[!pos_img], collapse = ", "))
            }
            # Copy files to node.js server. Will update if there are PUT file API.
            if (!is.null(path)) {
                target_files <- file.path(path, "files",
                                          img_files_raw)
                dir_names <- unique(dirname(target_files))
                for (i in seq(along = dir_names)) {
                    if (!dir.exists(dir_names[i])) {
                        dir.create(dir_names[i], recursive = TRUE)
                    }
                }

                file.copy(img_files_new,
                          target_files,
                          overwrite = TRUE)
            }
            text[pos] <- gsub(pattern, "\\1./files/\\2\\3", text[pos])

        }
        body <- .tiddler_json(title = title,
                              text = text,
                              type = "text/x-markdown",
                              tags = tags)

        output_file <- file.path(dirname(output_file), paste0(title, ".json"))
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
