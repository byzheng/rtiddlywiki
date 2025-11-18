# Convert data.frame into table of TiddlyWiki

Convert data.frame into table of TiddlyWiki

## Usage

``` r
tw_table(df, collapse = "\n")
```

## Arguments

- df:

  data.frame object

- collapse:

  an optional character string to separate the results.

## Value

character string for table in TiddlyWiki

## Examples

``` r
cars |>
    dplyr::slice(1:10) |>
    tw_table()
#> [1] "| speed| dist|\n|     4|    2|\n|     4|   10|\n|     7|    4|\n|     7|   22|\n|     8|   16|\n|     9|   10|\n|    10|   18|\n|    10|   26|\n|    10|   34|\n|    11|   17|"
```
