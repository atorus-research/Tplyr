# Set the display for missing strings

Controls how missing counts are handled and displayed in the layer

## Usage

``` r
set_missing_count(e, fmt = NULL, sort_value = NULL, denom_ignore = FALSE, ...)
```

## Arguments

- e:

  A `count_layer` object

- fmt:

  An f_str object to change the display of the missing counts

- sort_value:

  A numeric value that will be used in the ordering column. This should
  be numeric. If it is not supplied the ordering column will be the
  maximum value of what appears in the table plus one.

- denom_ignore:

  A boolean. Specifies Whether or not to include the missing counts
  specified within the ... parameter within denominators. If set to
  TRUE, the values specified within ... will be ignored.

- ...:

  Parameters used to note which values to describe as missing. Generally
  NA and "Missing" would be used here. Parameters can be named character
  vectors where the names become the row label.

## Value

The modified layer

## Examples

``` r
library(magrittr)
library(dplyr)
  mtcars2 <- mtcars %>%
mutate_all(as.character)
mtcars2[mtcars$cyl == 6, "cyl"] <- NA

tplyr_table(mtcars2, gear) %>%
  add_layer(
    group_count(cyl) %>%
      set_missing_count(f_str("xx ", n), Missing = NA)
  ) %>%
  build()
#> # A tibble: 3 × 6
#>   row_label1 var1_3        var1_4        var1_5      ord_layer_index ord_layer_1
#>   <chr>      <chr>         <chr>         <chr>                 <int>       <dbl>
#> 1 4          " 1 (  6.7%)" " 8 ( 66.7%)" " 2 ( 40.0…               1           1
#> 2 8          "12 ( 80.0%)" " 0 (  0.0%)" " 2 ( 40.0…               1           2
#> 3 Missing    " 2 "         " 4 "         " 1 "                     1           3
```
