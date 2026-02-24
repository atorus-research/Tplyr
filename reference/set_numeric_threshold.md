# Set a numeric cutoff

**\[experimental\]**

In certain tables, it may be necessary to only include rows that meet
numeric conditions. Rows that are less than a certain cutoff can be
suppressed from the output. This function allows you to pass a cutoff, a
cutoff stat(n, distinct_n, pct, or distinct_pct) to supress values that
are lesser than the cutoff.

## Usage

``` r
set_numeric_threshold(e, numeric_cutoff, stat, column = NULL)
```

## Arguments

- e:

  A `count_layer` object

- numeric_cutoff:

  A numeric value where only values greater than or equal to will be
  displayed.

- stat:

  The statistic to use when filtering out rows. Either 'n',
  'distinct_n', or 'pct' are allowable

- column:

  If only a particular column should be used to cutoff values, it can be
  supplied here as a character value.

## Value

The modified Tplyr layer object

## Examples

``` r
mtcars %>%
tplyr_table(gear) %>%
  add_layer(
    group_count(cyl) %>%
      set_numeric_threshold(10, "n") %>%
      add_total_row() %>%
      set_order_count_method("bycount")
  )
#> *** tplyr_table ***
#> Target (data.frame):
#>  Name:  .
#>  Rows:  32
#>  Columns:  11 
#> treat_var variable (quosure)
#>  gear
#> header_n:  header groups
#> treat_grps groupings (list)
#> Table Columns (cols):
#> where: TRUE
#> Number of layer(s): 1
#> layer_output: <Table Not Built Yet>
```
