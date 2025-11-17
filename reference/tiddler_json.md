# Generate tiddler in json format

Generate tiddler in json format

## Usage

``` r
tiddler_json(
  title,
  text,
  type = c("text/vnd.tiddlywiki", "text/x-tiddlywiki", "text/x-markdown", "text/html",
    "text/plain", "application/json"),
  tags = NULL,
  fields = NULL,
  format = c("json", "list")
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

  a vector for tiddler tags

- fields:

  a named vector for tiddler fields.

- format:

  export format as json or list

## Value

New tiddler in json format
