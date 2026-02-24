# Set values the denominator calculation will ignore

\`r lifecycle::badge("defunct")\`

This is generally used for missing values. Values like "", NA, "NA" are
common ways missing values are presented in a data frame. In certain
cases, percentages do not use "missing" values in the denominator. This
function notes different values as "missing" and excludes them from the
denominators.

## Usage

``` r
set_denom_ignore(e, ...)
```

## Arguments

- e:

  A `count_layer` object

- ...:

  Values to exclude from the percentage calculation. If you use
  \`set_missing_counts()\` this should be the name of the parameters
  instead of the values, see the example below.

## Value

The modified layer object

## Examples

``` r
library(magrittr)
mtcars2 <- mtcars
mtcars2[mtcars$cyl == 6, "cyl"] <- NA
mtcars2[mtcars$cyl == 8, "cyl"] <- "Not Found"

tplyr_table(mtcars2, gear) %>%
  add_layer(
    group_count(cyl) %>%
      set_missing_count(f_str("xx ", n), Missing = c(NA, "Not Found"))
      # This function is currently deprecated. It was replaced with an
      # argument in set_missing_count
      # set_denom_ignore("Missing")
  ) %>%
  build()
#> # A tibble: 2 × 6
#>   row_label1 var1_3        var1_4        var1_5      ord_layer_index ord_layer_1
#>   <chr>      <chr>         <chr>         <chr>                 <int>       <dbl>
#> 1 4          " 1 (  6.7%)" " 8 ( 66.7%)" " 2 ( 40.0…               1           1
#> 2 Missing    "14 "         " 4 "         " 3 "                     1           3
```
