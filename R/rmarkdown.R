

#' Format for converting from R Markdown to another tiddler markdown
#'
#' @param path The folder of tiddlywiki
#' @return R Markdown output format to pass to render()
#' @export
#'
#' @examples
#' if (FALSE) {
#' library(rmarkdown)
#' render("input.Rmd", tiddler_document())
#' }
tiddler_document <- function(path = ".") {

    # Get md document

    output <- rmarkdown::md_document()
    # Define post processor function
    post_processor <- function(metadata, input_file, output_file, clean, verbose) {
        file.copy(input_file, paste0(input_file, '.bck'))
        print(input_file)
        print(output_file)
        print(clean)
        print(verbose)
        print(path)
        output_file
    }
    output$post_processor <- post_processor
    return(output)
}
