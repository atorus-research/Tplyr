# Retrieve the numeric data from a tplyr objects

`get_numeric_data` provides access to the un-formatted numeric data for
each of the layers within a `tplyr_table`, with options to allow you to
extract distinct layers and filter as desired.

## Usage

``` r
get_numeric_data(x, layer = NULL, where = TRUE, ...)
```

## Arguments

- x:

  A tplyr_table or tplyr_layer object

- layer:

  Layer name or index to select out specifically

- where:

  Subset criteria passed to dplyr::filter

- ...:

  Additional arguments to pass forward

## Value

Numeric data from the Tplyr layer

## Details

When used on a `tplyr_table` object, this method will aggregate the
numeric data from all Tplyr layers. The data will be returned to the
user in a list of data frames. If the data has already been processed
(i.e. `build` has been run), the numeric data is already available and
will be returned without reprocessing. Otherwise, the numeric portion of
the layer will be processed.

Using the layer and where parameters, data for a specific layer can be
extracted and subset. This is most clear when layers are given text
names instead of using a layer index, but a numeric index works as well.

## Examples

``` r
# Load in pipe
library(magrittr)

t <- tplyr_table(mtcars, gear) %>%
 add_layer(name='drat',
           group_desc(drat)
 ) %>%
 add_layer(name='cyl',
           group_count(cyl)
 )

 # Return a list of the numeric data frames
 get_numeric_data(t)
#> $drat
#> # A tibble: 27 × 4
#>    summary_var gear  stat     value
#>    <chr>       <fct> <chr>    <dbl>
#>  1 drat        3     n       15    
#>  2 drat        3     mean     3.13 
#>  3 drat        3     sd       0.274
#>  4 drat        3     median   3.08 
#>  5 drat        3     q1       3.04 
#>  6 drat        3     q3       3.18 
#>  7 drat        3     min      2.76 
#>  8 drat        3     max      3.73 
#>  9 drat        3     missing  0    
#> 10 drat        4     n       12    
#> # ℹ 17 more rows
#> 
#> $cyl
#> # A tibble: 9 × 8
#>   gear  summary_var     n distinct_n total distinct_total    pct distinct_pct
#>   <chr> <chr>       <dbl>      <dbl> <int>          <int>  <dbl>        <dbl>
#> 1 3     4               1          1    15              1 0.0667            1
#> 2 3     6               2          1    15              1 0.133             1
#> 3 3     8              12          1    15              1 0.8               1
#> 4 4     4               8          1    12              1 0.667             1
#> 5 4     6               4          1    12              1 0.333             1
#> 6 4     8               0          0    12              1 0                 0
#> 7 5     4               2          1     5              1 0.4               1
#> 8 5     6               1          1     5              1 0.2               1
#> 9 5     8               2          1     5              1 0.4               1
#> 

 # Get the data from a specific layer
 get_numeric_data(t, layer='drat')
#> # A tibble: 27 × 4
#>    summary_var gear  stat     value
#>    <chr>       <fct> <chr>    <dbl>
#>  1 drat        3     n       15    
#>  2 drat        3     mean     3.13 
#>  3 drat        3     sd       0.274
#>  4 drat        3     median   3.08 
#>  5 drat        3     q1       3.04 
#>  6 drat        3     q3       3.18 
#>  7 drat        3     min      2.76 
#>  8 drat        3     max      3.73 
#>  9 drat        3     missing  0    
#> 10 drat        4     n       12    
#> # ℹ 17 more rows
 get_numeric_data(t, layer=1)
#> # A tibble: 27 × 4
#>    summary_var gear  stat     value
#>    <chr>       <fct> <chr>    <dbl>
#>  1 drat        3     n       15    
#>  2 drat        3     mean     3.13 
#>  3 drat        3     sd       0.274
#>  4 drat        3     median   3.08 
#>  5 drat        3     q1       3.04 
#>  6 drat        3     q3       3.18 
#>  7 drat        3     min      2.76 
#>  8 drat        3     max      3.73 
#>  9 drat        3     missing  0    
#> 10 drat        4     n       12    
#> # ℹ 17 more rows

 # Choose multiple layers by name or index
 get_numeric_data(t, layer=c('cyl', 'drat'))
#> $cyl
#> # A tibble: 9 × 8
#>   gear  summary_var     n distinct_n total distinct_total    pct distinct_pct
#>   <chr> <chr>       <dbl>      <dbl> <int>          <int>  <dbl>        <dbl>
#> 1 3     4               1          1    15              1 0.0667            1
#> 2 3     6               2          1    15              1 0.133             1
#> 3 3     8              12          1    15              1 0.8               1
#> 4 4     4               8          1    12              1 0.667             1
#> 5 4     6               4          1    12              1 0.333             1
#> 6 4     8               0          0    12              1 0                 0
#> 7 5     4               2          1     5              1 0.4               1
#> 8 5     6               1          1     5              1 0.2               1
#> 9 5     8               2          1     5              1 0.4               1
#> 
#> $drat
#> # A tibble: 27 × 4
#>    summary_var gear  stat     value
#>    <chr>       <fct> <chr>    <dbl>
#>  1 drat        3     n       15    
#>  2 drat        3     mean     3.13 
#>  3 drat        3     sd       0.274
#>  4 drat        3     median   3.08 
#>  5 drat        3     q1       3.04 
#>  6 drat        3     q3       3.18 
#>  7 drat        3     min      2.76 
#>  8 drat        3     max      3.73 
#>  9 drat        3     missing  0    
#> 10 drat        4     n       12    
#> # ℹ 17 more rows
#> 
 get_numeric_data(t, layer=c(2, 1))
#> $cyl
#> # A tibble: 9 × 8
#>   gear  summary_var     n distinct_n total distinct_total    pct distinct_pct
#>   <chr> <chr>       <dbl>      <dbl> <int>          <int>  <dbl>        <dbl>
#> 1 3     4               1          1    15              1 0.0667            1
#> 2 3     6               2          1    15              1 0.133             1
#> 3 3     8              12          1    15              1 0.8               1
#> 4 4     4               8          1    12              1 0.667             1
#> 5 4     6               4          1    12              1 0.333             1
#> 6 4     8               0          0    12              1 0                 0
#> 7 5     4               2          1     5              1 0.4               1
#> 8 5     6               1          1     5              1 0.2               1
#> 9 5     8               2          1     5              1 0.4               1
#> 
#> $drat
#> # A tibble: 27 × 4
#>    summary_var gear  stat     value
#>    <chr>       <fct> <chr>    <dbl>
#>  1 drat        3     n       15    
#>  2 drat        3     mean     3.13 
#>  3 drat        3     sd       0.274
#>  4 drat        3     median   3.08 
#>  5 drat        3     q1       3.04 
#>  6 drat        3     q3       3.18 
#>  7 drat        3     min      2.76 
#>  8 drat        3     max      3.73 
#>  9 drat        3     missing  0    
#> 10 drat        4     n       12    
#> # ℹ 17 more rows
#> 

 # Get the data and filter it
 get_numeric_data(t, layer='drat', where = gear==3)
#> # A tibble: 9 × 4
#>   summary_var gear  stat     value
#>   <chr>       <fct> <chr>    <dbl>
#> 1 drat        3     n       15    
#> 2 drat        3     mean     3.13 
#> 3 drat        3     sd       0.274
#> 4 drat        3     median   3.08 
#> 5 drat        3     q1       3.04 
#> 6 drat        3     q3       3.18 
#> 7 drat        3     min      2.76 
#> 8 drat        3     max      3.73 
#> 9 drat        3     missing  0    
```
