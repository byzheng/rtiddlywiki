# Create HTML Tabs for Multiple Objects

This function generates a tabbed HTML interface for displaying multiple
R objects such as \`ggplot2\` plots and data frames. Each tab can
include custom arguments for rendering (e.g., image width or table
formatting).

## Usage

``` r
create_tabs(...)
```

## Arguments

- ...:

  Named arguments where each name defines a tab label. Each argument can
  be either:

  - A single object (\`ggplot\`, \`data.frame\`, etc.)

  - A list with an \`object\` element and additional named arguments
    passed to the relevant rendering function.

## Value

A \`htmltools::tag\` object representing the full tab interface. Can be
printed in R Markdown documents or displayed interactively in RStudio
Viewer.

## Details

Supported object types:

- \`ggplot\` objects — rendered as base64-encoded images using
  \`save_base64()\`

- \`data.frame\` or \`tibble\` — rendered using \`kable_html()\`

Additional arguments passed inside the list are used by the relevant
rendering function.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# Simple plot and table
p <- ggplot(cars) + geom_point(aes(speed, dist))
df <- head(cars)

# Basic usage
create_tabs(
  plot = p,
  table = df
)

# With custom rendering arguments
create_tabs(
  plot = list(object = p, width = 4),
  table = list(object = df, digits = 2)
)
} # }
```
