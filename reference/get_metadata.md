# Get the metadata dataframe from a tplyr_table

Pull out the metadata dataframe from a tplyr_table to work with it
directly

## Usage

``` r
get_metadata(t)
```

## Arguments

- t:

  A Tplyr table with metadata built

## Value

Tplyr metadata dataframe

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

get_metadata(t)
#> # A tibble: 6 × 5
#>   row_id row_label1 var1_3     var1_4     var1_5    
#>   <chr>  <chr>      <list>     <list>     <list>    
#> 1 d1_1   n          <tplyr_mt> <tplyr_mt> <tplyr_mt>
#> 2 d2_1   Mean (SD)  <tplyr_mt> <tplyr_mt> <tplyr_mt>
#> 3 d3_1   Median     <tplyr_mt> <tplyr_mt> <tplyr_mt>
#> 4 d4_1   Q1, Q3     <tplyr_mt> <tplyr_mt> <tplyr_mt>
#> 5 d5_1   Min, Max   <tplyr_mt> <tplyr_mt> <tplyr_mt>
#> 6 d6_1   Missing    <tplyr_mt> <tplyr_mt> <tplyr_mt>
```
