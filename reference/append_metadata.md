# Append the Tplyr table metadata dataframe

`append_metadata()` allows a user to extend the Tplyr metadata data
frame with user provided data. In some tables, Tplyr may be able to
provided most of the data, but a user may have to extend the table with
other summaries, statistics, etc. This function allows the user to
extend the tplyr_table's metadata with their own metadata content using
custom data frames created using the `tplyr_meta` object.

## Usage

``` r
append_metadata(t, meta)
```

## Arguments

- t:

  A tplyr_table object

- meta:

  A dataframe fitting the specifications of the details section of this
  function

## Value

A tplyr_table object

## Details

As this is an advanced feature of Tplyr, ownership is on the user to
make sure the metadata data frame is assembled properly. The only
restrictions applied by `append_metadata()` are that `meta` must have a
column named `row_id`, and the values in `row_id` cannot be duplicates
of any `row_id` value already present in the Tplyr metadata dataframe.
[`tplyr_meta()`](https://atorus-research.github.io/Tplyr/reference/tplyr_meta.md)
objects align with constructed dataframes using the `row_id` and output
dataset column name. As such,
[`tplyr_meta()`](https://atorus-research.github.io/Tplyr/reference/tplyr_meta.md)
objects should be inserted into a data frame using a list column.

## Examples

``` r
t <- tplyr_table(mtcars, gear) %>%
  add_layer(
    group_desc(wt)
  )

t %>%
  build(metadata=TRUE)
#> # A tibble: 6 × 7
#>   row_id row_label1 var1_3             var1_4 var1_5 ord_layer_index ord_layer_1
#>   <chr>  <chr>      <chr>              <chr>  <chr>            <int>       <int>
#> 1 d1_1   n          " 15"              " 12"  "  5"                1           1
#> 2 d2_1   Mean (SD)  "3.8926 (0.83299)" "2.61… "2.63…               1           2
#> 3 d3_1   Median     "3.7300"           "2.70… "2.77…               1           3
#> 4 d4_1   Q1, Q3     "3.4500, 3.9575"   "2.13… "2.14…               1           4
#> 5 d5_1   Min, Max   "2.465, 5.424"     "1.61… "1.51…               1           5
#> 6 d6_1   Missing    "  0"              "  0"  "  0"                1           6

m <- tibble::tibble(
  row_id = c('x1_1'),
  var1_3 = list(tplyr_meta(rlang::quos(a, b, c), rlang::quos(a==1, b==2, c==3)))
)

append_metadata(t, m)
#> *** tplyr_table ***
#> Target (data.frame):
#>  Name:  mtcars
#>  Rows:  32
#>  Columns:  11 
#> treat_var variable (quosure)
#>  gear
#> header_n: 3 header groups
#> treat_grps groupings (list)
#> Table Columns (cols):
#> where: TRUE
#> Number of layer(s): 1
#> layer_output: <Table Not Built Yet>
```
