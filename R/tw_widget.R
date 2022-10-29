
#' Create a tiddlywiki widget from htmlwidget
#'
#' @param widget an object of htmlwidget
#'
#' @return a new tiddlywiki widget
#' @export
#'
#' @examples
#' library(leaflet)
#' content <- paste(sep = "<br/>",
#'                  "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
#'                  "606 5th Ave. S",
#'                  "Seattle, WA 98138"
#' )
#'
#' widget <- leaflet() %>% addTiles() %>%
#'     addPopups(-122.327298, 47.597131, content,
#'                   options = popupOptions(closeButton = FALSE)
#'     )
#' tw_widget(widget)

tw_widget <- function(widget) {

    w_class <- class(widget)
    if (!("htmlwidget" %in% w_class)) {
        stop("Not a htmlwidget object")
    }

    if (length(w_class) != 2) {
        stop("Expect with two classes")
    }

    w_type <- w_class[w_class != "htmlwidget"]

    w_class[w_class == "htmlwidget"] <- "html-widget"

    # Save to html file
    temp_file <- tempfile(fileext = "html")
    htmlwidgets::saveWidget(widget = widget, file = temp_file, selfcontained = FALSE)
    temp_doc <- rvest::read_html(temp_file)
    xpath <- paste(sprintf("contains(@class, '%s')", w_class), collapse = " and ")
    xpath <- sprintf("//div[%s]", xpath)
    h_node <- rvest::html_element(temp_doc, xpath = xpath)
    h_id <- rvest::html_attr(h_node, "id")
    xpath <- sprintf("//script[@data-for='%s' and @type='application/json']", h_id)
    script_node <- rvest::html_element(temp_doc, xpath = xpath)
    json <- rvest::html_text(script_node)
    new_widget <- sprintf('<$htmlwidgets type="%s" uuid="%s" data="""%s"""/>',
                         w_type, h_id, json)
    file.remove(temp_file)
    cat(new_widget)
    new_widget
}
