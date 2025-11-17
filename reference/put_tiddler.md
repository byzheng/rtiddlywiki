# Put a tiddler

Put a tiddler

## Usage

``` r
put_tiddler(
  title,
  text,
  type = c("text/vnd.tiddlywiki", "text/x-tiddlywiki", "text/x-markdown", "text/html",
    "text/plain", "application/json"),
  tags = NULL,
  fields = NULL,
  recipe = TW_OPTIONS("recipe")
)
```

## Arguments

- title:

  tiddler title

- text:

  tiddler text

- type:

  tiddler type

- tags:

  tiddler tags which is merged with existing tags

- fields:

  a named vector for tiddler fields which is merged with existing tags

- recipe:

  string defining which recipe to write to (optional, defaults to
  "default")

## Value

No return value

## Examples

``` r
if (FALSE) { # \dontrun{
title <- "New tiddler"
text <- c("!! Section",
          "This is a new tiddler")
type <- "text/vnd.tiddlywiki"
tags <- c("Tag1", "Tag 2")
fields <- c("F1" = "V1", "F2" = "V2")
put_tiddler(title = title,
            text = text,
            type = type,
            tags = tags,
            fields = fields)
} # }
```
