# Format for converting from R Markdown to another tiddler markdown

Format for converting from R Markdown to another tiddler markdown

## Usage

``` r
tiddler_document(
  host = NULL,
  remote = FALSE,
  preview = FALSE,
  tags = NULL,
  fields = NULL,
  use_bookdown = FALSE,
  overwrite = FALSE,
  variant = "gfm",
  pandoc_args = "--wrap=none",
  ...
)
```

## Arguments

- host:

  the host of tiddlywiki web server

- remote:

  whether put into remote TiddlyWiki Node.js Server

- preview:

  whether to send \`open_tiddler\` command to ws server (tw-livebridge)
  to preview in browser

- tags:

  tiddler tags

- fields:

  a named vector for tiddler fields

- use_bookdown:

  logical. Use bookdown to generate markdown file.

- overwrite:

  whether to overwrite the existing tiddler.

- variant:

  variant for md_document

- pandoc_args:

  pandoc_args for md_document

- ...:

  Other argument pass to md_document

## Value

R Markdown output format to pass to render()

## Examples

``` r
if (FALSE) { # \dontrun{
library(rmarkdown)
render("input.Rmd")
} # }
```
