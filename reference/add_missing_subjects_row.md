# Add a missing subject row into a count summary.

This function calculates the number of subjects missing from a
particular group of results. The calculation is done by examining the
total number of subjects potentially available from the Header N values
within the result column, and finding the difference with the total
number of subjects present in the result group. Note that for accurate
results, the subject variable needs to be defined using the
\`set_distinct_by()\` function. As with other methods, this function
instructs how distinct results should be identified.

## Usage

``` r
add_missing_subjects_row(e, fmt = NULL, sort_value = NULL)
```

## Arguments

- e:

  A \`count_layer\` object

- fmt:

  An f_str object used to format the total row. If none is provided,
  display is based on the layer formatting.

- sort_value:

  The value that will appear in the ordering column for total rows. This
  must be a numeric value.

## Examples

``` r
tplyr_table(mtcars, gear) %>%
  add_layer(
    group_count(cyl) %>%
      add_missing_subjects_row(f_str("xxxx", n))
   ) %>%
   build()
#> Warning:    Population data was not set separately from the target data.
#>  Missing subject counts may be misleading in this scenario.
#>  Did you mean to use `set_missing_count() instead?
#> # A tibble: 4 × 6
#>   row_label1 var1_3        var1_4        var1_5      ord_layer_index ord_layer_1
#>   <chr>      <chr>         <chr>         <chr>                 <int>       <dbl>
#> 1 4          " 1 (  6.7%)" " 8 ( 66.7%)" " 2 ( 40.0…               1           1
#> 2 6          " 2 ( 13.3%)" " 4 ( 33.3%)" " 1 ( 20.0…               1           2
#> 3 8          "12 ( 80.0%)" " 0 (  0.0%)" " 2 ( 40.0…               1           3
#> 4 Missing    "    "        "    "        "    "                    1           4
```
