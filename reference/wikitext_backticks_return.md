# backticks

backticks for wikitext

## Usage

``` r
wikitext_backticks_return(x)
```

## Arguments

- x:

  character vector

## Value

By default this function outputs (see: `cat`) the result. Call the
function ending in `.return` to catch the result instead.

## Examples

``` r
wikitext_backticks('FOO')
#> `FOO`
wikitext_backticks_return('FOO')
#> [1] "`FOO`"
```
