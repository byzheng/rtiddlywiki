# Convert Data Frame to HTML Table using kable

Convert Data Frame to HTML Table using kable

## Usage

``` r
kable_html(df, ...)
```

## Arguments

- df:

  A data frame to be converted to an HTML table.

- ...:

  Other arguments to be passed to \`knitr::kable\`.

## Value

A htmltools object containing the HTML representation of the table.

## Examples

``` r
kable_html(cars[1:10,])
#> <table>
#>  <thead>
#>   <tr>
#>    <th style="text-align:right;"> speed </th>
#>    <th style="text-align:right;"> dist </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:right;"> 4 </td>
#>    <td style="text-align:right;"> 2 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 4 </td>
#>    <td style="text-align:right;"> 10 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 7 </td>
#>    <td style="text-align:right;"> 4 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 7 </td>
#>    <td style="text-align:right;"> 22 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 8 </td>
#>    <td style="text-align:right;"> 16 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 9 </td>
#>    <td style="text-align:right;"> 10 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 10 </td>
#>    <td style="text-align:right;"> 18 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 10 </td>
#>    <td style="text-align:right;"> 26 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 10 </td>
#>    <td style="text-align:right;"> 34 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 11 </td>
#>    <td style="text-align:right;"> 17 </td>
#>   </tr>
#> </tbody>
#> </table>
```
