# Create a tiddlywiki widget from htmlwidget

Create a tiddlywiki widget from htmlwidget

## Usage

``` r
tw_widget(widget, is_cat = FALSE)
```

## Arguments

- widget:

  an object of htmlwidget

- is_cat:

  whether to show results on screen

## Value

a new tiddlywiki widget

## Examples

``` r
library(leaflet)
if (FALSE) { # \dontrun{
content <- paste(sep = "<br/>",
                 "<b><a href='http://www.samurainoodle.com'>Samurai Noodle</a></b>",
                 "606 5th Ave. S",
                 "Seattle, WA 98138"
)

widget <- leaflet() %>% addTiles() %>%
    addPopups(-122.327298, 47.597131, content,
                  options = popupOptions(closeButton = FALSE)
    )
tw_widget(widget)
} # }
```
