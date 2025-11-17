# Read a TiddlyWiki Table into a Data Frame

This function parses a table written in TiddlyWiki format and converts
it into an R data frame. It can optionally treat the first row as a
header.

## Usage

``` r
read_table(table, header = TRUE)
```

## Arguments

- table:

  A character string representing the TiddlyWiki table.

- header:

  A logical value indicating whether the first row should be treated as
  column headers. Default is TRUE.

## Value

A data frame containing the parsed table data.

## Examples

``` r
table <- "|!Cell1 |!Cell2 |\n|Cell3 |Cell4 |"
df <- read_table(table, header = TRUE)
print(df)
#>   Cell1 Cell2
#> 1 Cell3 Cell4
```
