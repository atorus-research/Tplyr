# Set variables to limit reported data values only to those that exist rather than fully completing all possible levels

This function allows you to select a combination of by variables or
potentially target variables for which you only want to display values
present in the data. By default, Tplyr will create a cartesian
combination of potential values of the data. For example, if you have 2
by variables present, then each potential combination of those by
variables will have a row present in the final table.
`set_limit_data_by()` allows you to choose the by variables whose
combination you wish to limit to values physically present in the
available data.

## Usage

``` r
set_limit_data_by(e, ...)
```

## Arguments

- e:

  A tplyr_layer

- ...:

  Subset of variables within by or target variables

## Value

a tplyr_table

## Examples

``` r
tplyr_table(tplyr_adpe, TRT01A) %>%
  add_layer(
    group_desc(AVAL, by = vars(PECAT, PARAM, AVISIT))
  ) %>%
  build()
#> # A tibble: 72 × 11
#>    row_label1 row_label2 row_label3 row_label4 `var1_TRT A` `var1_TRT B`
#>    <chr>      <chr>      <chr>      <chr>      <chr>        <chr>       
#>  1 A          Head       Screening  n          "  2"        "  1"       
#>  2 A          Head       Screening  Mean (SD)  "5.0 (5.66)" "5.0 (    )"
#>  3 A          Head       Screening  Median     "5.0"        "5.0"       
#>  4 A          Head       Screening  Q1, Q3     "3.0, 7.0"   "5.0, 5.0"  
#>  5 A          Head       Screening  Min, Max   "1, 9"       "5, 5"      
#>  6 A          Head       Screening  Missing    "  0"        "  0"       
#>  7 A          Head       Day -1     n          ""           ""          
#>  8 A          Head       Day -1     Mean (SD)  ""           ""          
#>  9 A          Head       Day -1     Median     ""           ""          
#> 10 A          Head       Day -1     Q1, Q3     ""           ""          
#> # ℹ 62 more rows
#> # ℹ 5 more variables: ord_layer_index <int>, ord_layer_1 <dbl>,
#> #   ord_layer_2 <dbl>, ord_layer_3 <dbl>, ord_layer_4 <int>

tplyr_table(tplyr_adpe, TRT01A) %>%
  add_layer(
    group_desc(AVAL, by = vars(PECAT, PARAM, AVISIT)) %>%
      set_limit_data_by(PARAM, AVISIT)
  ) %>%
  build()
#> # A tibble: 48 × 11
#>    row_label1 row_label2 row_label3 row_label4 `var1_TRT A` `var1_TRT B`
#>    <chr>      <chr>      <chr>      <chr>      <chr>        <chr>       
#>  1 A          Head       Screening  n          "  2"        "  1"       
#>  2 A          Head       Screening  Mean (SD)  "5.0 (5.66)" "5.0 (    )"
#>  3 A          Head       Screening  Median     "5.0"        "5.0"       
#>  4 A          Head       Screening  Q1, Q3     "3.0, 7.0"   "5.0, 5.0"  
#>  5 A          Head       Screening  Min, Max   "1, 9"       "5, 5"      
#>  6 A          Head       Screening  Missing    "  0"        "  0"       
#>  7 B          Head       Screening  n          ""           ""          
#>  8 B          Head       Screening  Mean (SD)  ""           ""          
#>  9 B          Head       Screening  Median     ""           ""          
#> 10 B          Head       Screening  Q1, Q3     ""           ""          
#> # ℹ 38 more rows
#> # ℹ 5 more variables: ord_layer_index <int>, ord_layer_1 <dbl>,
#> #   ord_layer_2 <dbl>, ord_layer_3 <dbl>, ord_layer_4 <int>

tplyr_table(tplyr_adpe, TRT01A) %>%
  add_layer(
    group_count(AVALC, by = vars(PECAT, PARAM, AVISIT)) %>%
      set_limit_data_by(PARAM, AVISIT)
  ) %>%
  build()
#> # A tibble: 24 × 11
#>    row_label1 row_label2 row_label3 row_label4  `var1_TRT A` `var1_TRT B`
#>    <chr>      <chr>      <chr>      <chr>       <chr>        <chr>       
#>  1 A          Head       Screening  Normal      2 ( 14.3%)   0 (  0.0%)  
#>  2 A          Head       Screening  Semi-Normal 0 (  0.0%)   1 ( 14.3%)  
#>  3 A          Head       Screening  Abnormal    0 (  0.0%)   0 (  0.0%)  
#>  4 B          Head       Screening  Normal      0 (  0.0%)   0 (  0.0%)  
#>  5 B          Head       Screening  Semi-Normal 0 (  0.0%)   0 (  0.0%)  
#>  6 B          Head       Screening  Abnormal    0 (  0.0%)   0 (  0.0%)  
#>  7 A          Lungs      Screening  Normal      1 (  7.1%)   1 ( 14.3%)  
#>  8 A          Lungs      Screening  Semi-Normal 0 (  0.0%)   0 (  0.0%)  
#>  9 A          Lungs      Screening  Abnormal    1 (  7.1%)   0 (  0.0%)  
#> 10 B          Lungs      Screening  Normal      1 (  7.1%)   1 ( 14.3%)  
#> # ℹ 14 more rows
#> # ℹ 5 more variables: ord_layer_index <int>, ord_layer_1 <dbl>,
#> #   ord_layer_2 <dbl>, ord_layer_3 <dbl>, ord_layer_4 <dbl>

tplyr_table(tplyr_adpe, TRT01A) %>%
  add_layer(
    group_count(AVALC, by = vars(PECAT, PARAM, AVISIT)) %>%
      set_limit_data_by(PECAT, PARAM, AVISIT)
  ) %>%
  build()
#> # A tibble: 21 × 11
#>    row_label1 row_label2 row_label3 row_label4  `var1_TRT A` `var1_TRT B`
#>    <chr>      <chr>      <chr>      <chr>       <chr>        <chr>       
#>  1 A          Head       Screening  Normal      2 ( 14.3%)   0 (  0.0%)  
#>  2 A          Head       Screening  Semi-Normal 0 (  0.0%)   1 ( 14.3%)  
#>  3 A          Head       Screening  Abnormal    0 (  0.0%)   0 (  0.0%)  
#>  4 A          Lungs      Screening  Normal      1 (  7.1%)   1 ( 14.3%)  
#>  5 A          Lungs      Screening  Semi-Normal 0 (  0.0%)   0 (  0.0%)  
#>  6 A          Lungs      Screening  Abnormal    1 (  7.1%)   0 (  0.0%)  
#>  7 A          Lungs      Day -1     Normal      2 ( 14.3%)   1 ( 14.3%)  
#>  8 A          Lungs      Day -1     Semi-Normal 0 (  0.0%)   0 (  0.0%)  
#>  9 A          Lungs      Day -1     Abnormal    0 (  0.0%)   0 (  0.0%)  
#> 10 A          Lungs      Day 5      Normal      1 (  7.1%)   1 ( 14.3%)  
#> # ℹ 11 more rows
#> # ℹ 5 more variables: ord_layer_index <int>, ord_layer_1 <dbl>,
#> #   ord_layer_2 <dbl>, ord_layer_3 <dbl>, ord_layer_4 <dbl>
```
