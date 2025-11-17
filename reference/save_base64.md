# Save ggplot into base64

Save ggplot into base64

## Usage

``` r
save_base64(plot, width = NULL, height = NULL, dpi = NULL, ...)
```

## Arguments

- plot:

  object for ggplot2 or a function for plot

- width:

  image width

- height:

  image height

- dpi:

  image resolution

- ...:

  Other arguments for plot function

## Value

character string for base64 image

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
p <- cars |>
    ggplot() +
    geom_point(aes(speed, dist))
p |> save_base64()
} # }
```
