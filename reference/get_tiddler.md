# Get a tiddler

Get a tiddler

## Usage

``` r
get_tiddler(title, recipe = TW_OPTIONS("recipe"))
```

## Arguments

- title:

  title of the tiddler to retrieve

- recipe:

  string defining which recipe to read from (optional, defaults to
  "default")

## Value

tiddler information in JSON format

## Examples

``` r
if (FALSE) { # \dontrun{
get_tiddler("GettingStarted")
} # }
```
