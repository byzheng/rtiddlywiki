# Generate tabs in the tiddlywiki

.unique_name <- function(n = 10) {
    stringi::stri_rand_strings(1, n, '[A-Z]')
}


#' Generate HTML Tabs with Dynamic Content
#'
#' This function creates a tabbed interface where each tab has dynamically generated content.
#'
#' @param names A character vector of tab labels.
#' @param fun A function that generates the content for each tab. It must take an index (`i`) as the first argument.
#' @param groupname A unique string to group the radio inputs (default is generated automatically).
#' @param checked The index of the tab that should be pre-selected (default is `1`).
#' @param ... Additional arguments passed to `fun`.
#'
#' @return An `htmltools::tagList` containing the tabbed interface.
#' @examples
#' tab_labels <- c("Tab1", "Tab2", "Tab3")
#'
#' tab_content_fun <- function(i, extra_text = "") {
#'   htmltools::tagList(
#'     htmltools::tags$p(paste("Content for tab:", tab_labels[i], extra_text)),
#'     htmltools::tags$img(src = paste0("plot_", i, ".png"), width = "100%")
#'   )
#' }
#'
#' tabs(tab_labels, tab_content_fun, checked = 2, extra_text = "Additional details")
#'
#' @export
tabs <- function(names, fun, groupname = .unique_name(), checked = 1, ...) {

    if (!is.character(names) || length(names) == 0) {
        stop("`names` must be a non-empty character vector.")
    }

    if (!is.function(fun)) {
        stop("`fun` must be a function that generates HTML content.")
    }

    if (!is.character(groupname) || length(groupname) != 1) {
        stop("`groupname` must be a single string.")
    }

    if (!is.numeric(checked) || length(checked) != 1 || checked < 1 || checked > length(names)) {
        stop("`checked` must be a single numeric value between 1 and the number of tabs.")
    }

    buttons <- c()
    contents <- c()

    for (i in seq_along(names)) {
        id <- paste0(groupname, "-", names[i])
        is_checked <- if(i == checked) "checked" else NULL

        buttons_i <- htmltools::tagList(
            htmltools::tags$input(type = "radio", id = id, name = groupname, checked = is_checked),
            htmltools::tags$label(`for` = id, names[i])
        )
        buttons <- c(buttons, buttons_i)

        content_i <- fun(i, ...)
        content_i <- htmltools::tagList(
            htmltools::tags$div(content_i)
        )
        contents <- c(contents, content_i)
    }

    # Combine everything
    res <- htmltools::div(
        class = "tabs",
        htmltools::div(class = "tab-buttons tc-tab-buttons", buttons),
        htmltools::div(class = "tab-content", contents)
    )
    return(structure(
        res,
        class = c("tw_html", "tw_tabs", class(res))
    ))
}

