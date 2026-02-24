# Set counts to be distinct by some grouping variable.

In some situations, count summaries may want to see distinct counts by a
variable like subject. For example, the number of subjects in a
population who had a particular adverse event. `set_distinct_by` allows
you to set the by variables used to determine a distinct count.

## Usage

``` r
set_distinct_by(e, distinct_by)
```

## Arguments

- e:

  A `count_layer/shift_layer` object

- distinct_by:

  Variable(s) to get the distinct data.

## Value

The layer object with

## Details

When a `distinct_by` value is set, distinct counts will be used by
default. If you wish to combine distinct and not distinct counts, you
can choose which to display in your
[`f_str()`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
objects using `n`, `pct`, `distinct_n`, and `distinct_pct`.
Additionally, denominators may be presented using `total` and
`distinct_total`

## Examples

``` r
#Load in pipe
library(magrittr)

tplyr_table(mtcars, gear) %>%
  add_layer(
    group_count(cyl) %>%
      set_distinct_by(carb)
  ) %>%
  build()
#> # A tibble: 3 × 6
#>   row_label1 var1_3        var1_4        var1_5      ord_layer_index ord_layer_1
#>   <chr>      <chr>         <chr>         <chr>                 <int>       <dbl>
#> 1 4          " 1 ( 25.0%)" " 2 ( 66.7%)" " 1 ( 25.0…               1           1
#> 2 6          " 1 ( 25.0%)" " 1 ( 33.3%)" " 1 ( 25.0…               1           2
#> 3 8          " 3 ( 75.0%)" " 0 (  0.0%)" " 2 ( 50.0…               1           3
```
