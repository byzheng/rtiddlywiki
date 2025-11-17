# Set or get options for my package

Set or get options for my package

## Usage

``` r
tw_options(...)
```

## Arguments

- ...:

  Option names to retrieve option values or `[key]=[value]` pairs to set
  options.

## Value

the default and modified options.

## Supported options

The following options are supported host: Host of tiddlywiki recipe:
Recipes are named lists of bags, ordered from lowest priority to highest
bag: Bags have access controls that determines which users can read or
write to them

## Examples

``` r
tw_options(host = "http://127.0.0.1:8080/")
```
