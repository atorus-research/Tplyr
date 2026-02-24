# Set the label for the missing subjects row

Set the label for the missing subjects row

## Usage

``` r
set_missing_subjects_row_label(e, missing_subjects_row_label)
```

## Arguments

- e:

  A `count_layer` object

- missing_subjects_row_label:

  A character to label the total row

## Value

The modified `count_layer` object

## Examples

``` r
t <- tplyr_table(mtcars, gear) %>%
  add_layer(
    group_count(cyl) %>%
      add_missing_subjects_row() %>%
      set_missing_subjects_row_label("Missing")
  )
#> Warning:    Population data was not set separately from the target data.
#>  Missing subject counts may be misleading in this scenario.
#>  Did you mean to use `set_missing_count() instead?
build(t)
#> # A tibble: 4 × 6
#>   row_label1 var1_3        var1_4        var1_5      ord_layer_index ord_layer_1
#>   <chr>      <chr>         <chr>         <chr>                 <int>       <dbl>
#> 1 4          " 1 (  6.7%)" " 8 ( 66.7%)" " 2 ( 40.0…               1           1
#> 2 6          " 2 ( 13.3%)" " 4 ( 33.3%)" " 1 ( 20.0…               1           2
#> 3 8          "12 ( 80.0%)" " 0 (  0.0%)" " 2 ( 40.0…               1           3
#> 4 Missing    "   (  0.0%)" "   (  0.0%)" "   (  0.0…               1           4
```
