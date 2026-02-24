# Set variables used in pct denominator calculation

This function is used when calculating pct in count or shift layers. The
percentages default to the treatment variable and any column variables
but can be calculated on any variables passed to target_var, treat_var,
by, or cols.

## Usage

``` r
set_denoms_by(e, ...)
```

## Arguments

- e:

  A count/shift layer object

- ...:

  Unquoted variable names

## Value

The modified layer object

## Examples

``` r
library(magrittr)

# Default has matrix of treatment group, additional columns,
# and by variables sum to 1
tplyr_table(mtcars, am) %>%
  add_layer(
    group_shift(vars(row=gear, column=carb), by=cyl) %>%
      set_format_strings(f_str("xxx (xx.xx%)", n, pct))
  ) %>%
  build()
#> # A tibble: 9 × 17
#>   row_label1 row_label2 var1_0_1    var1_0_2 var1_0_3 var1_0_4 var1_0_6 var1_0_8
#>   <chr>      <chr>      <chr>       <chr>    <chr>    <chr>    <chr>    <chr>   
#> 1 4          3          "  1 (33.3… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 2 4          4          "  0 ( 0.0… "  2 (6… "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 3 4          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 4 6          3          "  2 (50.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 5 6          4          "  0 ( 0.0… "  0 ( … "  0 ( … "  2 (5… "  0 ( … "  0 ( …
#> 6 6          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 7 8          3          "  0 ( 0.0… "  4 (3… "  3 (2… "  5 (4… "  0 ( … "  0 ( …
#> 8 8          4          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 9 8          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> # ℹ 9 more variables: var1_1_1 <chr>, var1_1_2 <chr>, var1_1_3 <chr>,
#> #   var1_1_4 <chr>, var1_1_6 <chr>, var1_1_8 <chr>, ord_layer_index <int>,
#> #   ord_layer_1 <dbl>, ord_layer_2 <dbl>

tplyr_table(mtcars, am) %>%
  add_layer(
    group_shift(vars(row=gear, column=carb), by=cyl) %>%
      set_format_strings(f_str("xxx (xx.xx%)", n, pct)) %>%
      set_denoms_by(cyl, gear) # Row % sums to 1
  ) %>%
  build()
#> # A tibble: 9 × 17
#>   row_label1 row_label2 var1_0_1    var1_0_2 var1_0_3 var1_0_4 var1_0_6 var1_0_8
#>   <chr>      <chr>      <chr>       <chr>    <chr>    <chr>    <chr>    <chr>   
#> 1 4          3          "  1 (100.… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 2 4          4          "  0 ( 0.0… "  2 (2… "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 3 4          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 4 6          3          "  2 (100.… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 5 6          4          "  0 ( 0.0… "  0 ( … "  0 ( … "  2 (5… "  0 ( … "  0 ( …
#> 6 6          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 7 8          3          "  0 ( 0.0… "  4 (3… "  3 (2… "  5 (4… "  0 ( … "  0 ( …
#> 8 8          4          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 9 8          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> # ℹ 9 more variables: var1_1_1 <chr>, var1_1_2 <chr>, var1_1_3 <chr>,
#> #   var1_1_4 <chr>, var1_1_6 <chr>, var1_1_8 <chr>, ord_layer_index <int>,
#> #   ord_layer_1 <dbl>, ord_layer_2 <dbl>

tplyr_table(mtcars, am) %>%
  add_layer(
    group_shift(vars(row=gear, column=carb), by=cyl) %>%
      set_format_strings(f_str("xxx (xx.xx%)", n, pct)) %>%
      set_denoms_by(cyl, gear, am) # % within treatment group sums to 1
  ) %>%
  build()
#> # A tibble: 9 × 17
#>   row_label1 row_label2 var1_0_1    var1_0_2 var1_0_3 var1_0_4 var1_0_6 var1_0_8
#>   <chr>      <chr>      <chr>       <chr>    <chr>    <chr>    <chr>    <chr>   
#> 1 4          3          "  1 (100.… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 2 4          4          "  0 ( 0.0… "  2 (1… "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 3 4          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 4 6          3          "  2 (100.… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 5 6          4          "  0 ( 0.0… "  0 ( … "  0 ( … "  2 (1… "  0 ( … "  0 ( …
#> 6 6          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 7 8          3          "  0 ( 0.0… "  4 (3… "  3 (2… "  5 (4… "  0 ( … "  0 ( …
#> 8 8          4          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 9 8          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> # ℹ 9 more variables: var1_1_1 <chr>, var1_1_2 <chr>, var1_1_3 <chr>,
#> #   var1_1_4 <chr>, var1_1_6 <chr>, var1_1_8 <chr>, ord_layer_index <int>,
#> #   ord_layer_1 <dbl>, ord_layer_2 <dbl>
```
