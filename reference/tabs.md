# Generate HTML Tabs with Dynamic Content

This function creates a tabbed interface where each tab has dynamically
generated content.

## Usage

``` r
tabs(names, fun, groupname = .unique_name(), checked = 1, ...)
```

## Arguments

- names:

  A character vector of tab labels.

- fun:

  A function that generates the content for each tab. It must take an
  index (\`i\`) as the first argument.

- groupname:

  A unique string to group the radio inputs (default is generated
  automatically).

- checked:

  The index of the tab that should be pre-selected (default is \`1\`).

- ...:

  Additional arguments passed to \`fun\`.

## Value

An \`htmltools::tagList\` containing the tabbed interface.

## Examples

``` r
if (FALSE) { # \dontrun{
tab_labels <- c("Tab1", "Tab2", "Tab3")

tab_content_fun <- function(i, extra_text = "") {
    htmltools::tagList(
        htmltools::tags$p(paste("Content for tab:", tab_labels[i], extra_text)),
        htmltools::tags$img(src = paste0("plot_", i, ".png"), width = "100%")
    )
}

tabs(tab_labels, tab_content_fun, checked = 2, extra_text = "Additional details")
} # }
```
