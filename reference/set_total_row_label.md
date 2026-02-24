# Set the label for the total row

The row label for a total row defaults to "Total", however this can be
overriden using this function.

## Usage

``` r
set_total_row_label(e, total_row_label)
```

## Arguments

- e:

  A `count_layer` object

- total_row_label:

  A character to label the total row

## Value

The modified `count_layer` object

## Examples

``` r
# Load in pipe
library(magrittr)

t <- tplyr_table(mtcars, gear) %>%
  add_layer(
    group_count(cyl) %>%
      add_total_row() %>%
      set_total_row_label("Total Cyl")
  )
build(t)
#> # A tibble: 4 × 6
#>   row_label1 var1_3        var1_4        var1_5      ord_layer_index ord_layer_1
#>   <chr>      <chr>         <chr>         <chr>                 <int>       <dbl>
#> 1 4          " 1 (  6.7%)" " 8 ( 66.7%)" " 2 ( 40.0…               1           1
#> 2 6          " 2 ( 13.3%)" " 4 ( 33.3%)" " 1 ( 20.0…               1           2
#> 3 8          "12 ( 80.0%)" " 0 (  0.0%)" " 2 ( 40.0…               1           3
#> 4 Total Cyl  "15 (100.0%)" "12 (100.0%)" " 5 (100.0…               1           4
```
