# Get statistics data

Like the layer numeric data, Tplyr also stores the numeric data produced
from statistics like risk difference. This helper function gives you
access to obtain that data from the environment

## Usage

``` r
get_stats_data(x, layer = NULL, statistic = NULL, where = TRUE, ...)
```

## Arguments

- x:

  A tplyr_table or tplyr_layer object

- layer:

  Layer name or index to select out specifically

- statistic:

  Statistic name or index to select

- where:

  Subset criteria passed to dplyr::filter

- ...:

  Additional arguments passed to dispatch

## Value

The statistics data of the supplied layer

## Details

When used on a `tplyr_table` object, this method will aggregate the
numeric data from all Tplyr layers and calculate all statistics. The
data will be returned to the user in a list of data frames. If the data
has already been processed (i.e. `build` has been run), the numeric data
is already available and the statistic data will simply be returned.
Otherwise, the numeric portion of the layer will be processed.

Using the layer, where, and statistic parameters, data for a specific
layer statistic can be extracted and subset, allowing you to directly
access data of interest. This is most clear when layers are given text
names instead of using a layer index, but a numeric index works as well.
If just a statistic is specified, that statistic will be collected and
returned in a list of data frames, allowing you to grab, for example,
just the risk difference statistics across all layers.

## Examples

``` r
library(magrittr)

t <- tplyr_table(mtcars, gear) %>%
  add_layer(name='drat',
            group_desc(drat)
  ) %>%
  add_layer(name="cyl",
            group_count(cyl)
  ) %>%
  add_layer(name="am",
            group_count(am) %>%
              add_risk_diff(c('4', '3'))
  ) %>%
  add_layer(name="carb",
            group_count(carb) %>%
              add_risk_diff(c('4', '3'))
  )

 # Returns a list of lists, containing stats data from each layer
 get_stats_data(t)
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> $drat
#> list()
#> 
#> $cyl
#> list()
#> 
#> $am
#> $am$riskdiff
#> # A tibble: 10 × 3
#>    summary_var measure  `4_3`
#>    <chr>       <chr>    <dbl>
#>  1 0           comp     0.333
#>  2 0           ref      1    
#>  3 0           dif     -0.667
#>  4 0           low     -1    
#>  5 0           high    -0.325
#>  6 1           comp     0.667
#>  7 1           ref      0    
#>  8 1           dif      0.667
#>  9 1           low      0.325
#> 10 1           high     1    
#> 
#> 
#> $carb
#> $carb$riskdiff
#> # A tibble: 30 × 3
#>    summary_var measure   `4_3`
#>    <chr>       <chr>     <dbl>
#>  1 1           comp     0.333 
#>  2 1           ref      0.2   
#>  3 1           dif      0.133 
#>  4 1           low     -0.277 
#>  5 1           high     0.543 
#>  6 2           comp     0.333 
#>  7 2           ref      0.267 
#>  8 2           dif      0.0667
#>  9 2           low     -0.348 
#> 10 2           high     0.481 
#> # ℹ 20 more rows
#> 
#> 

 # Returns just the riskdiff statistics from each layer - NULL
 # for layers without riskdiff
 get_stats_data(t, statistic="riskdiff")
#> $drat
#> NULL
#> 
#> $cyl
#> NULL
#> 
#> $am
#> # A tibble: 10 × 3
#>    summary_var measure  `4_3`
#>    <chr>       <chr>    <dbl>
#>  1 0           comp     0.333
#>  2 0           ref      1    
#>  3 0           dif     -0.667
#>  4 0           low     -1    
#>  5 0           high    -0.325
#>  6 1           comp     0.667
#>  7 1           ref      0    
#>  8 1           dif      0.667
#>  9 1           low      0.325
#> 10 1           high     1    
#> 
#> $carb
#> # A tibble: 30 × 3
#>    summary_var measure   `4_3`
#>    <chr>       <chr>     <dbl>
#>  1 1           comp     0.333 
#>  2 1           ref      0.2   
#>  3 1           dif      0.133 
#>  4 1           low     -0.277 
#>  5 1           high     0.543 
#>  6 2           comp     0.333 
#>  7 2           ref      0.267 
#>  8 2           dif      0.0667
#>  9 2           low     -0.348 
#> 10 2           high     0.481 
#> # ℹ 20 more rows
#> 

 # Return the statistic data for just the "am" layer - a list
 get_stats_data(t, layer="am")
#> $riskdiff
#> # A tibble: 10 × 3
#>    summary_var measure  `4_3`
#>    <chr>       <chr>    <dbl>
#>  1 0           comp     0.333
#>  2 0           ref      1    
#>  3 0           dif     -0.667
#>  4 0           low     -1    
#>  5 0           high    -0.325
#>  6 1           comp     0.667
#>  7 1           ref      0    
#>  8 1           dif      0.667
#>  9 1           low      0.325
#> 10 1           high     1    
#> 
 get_stats_data(t, layer=3)
#> $riskdiff
#> # A tibble: 10 × 3
#>    summary_var measure  `4_3`
#>    <chr>       <chr>    <dbl>
#>  1 0           comp     0.333
#>  2 0           ref      1    
#>  3 0           dif     -0.667
#>  4 0           low     -1    
#>  5 0           high    -0.325
#>  6 1           comp     0.667
#>  7 1           ref      0    
#>  8 1           dif      0.667
#>  9 1           low      0.325
#> 10 1           high     1    
#> 

 # Return the statistic data for just the "am" and "cyl", layer - a
 # list of lists
 get_stats_data(t, layer=c("am", "cyl"))
#> $am
#> $am$riskdiff
#> # A tibble: 10 × 3
#>    summary_var measure  `4_3`
#>    <chr>       <chr>    <dbl>
#>  1 0           comp     0.333
#>  2 0           ref      1    
#>  3 0           dif     -0.667
#>  4 0           low     -1    
#>  5 0           high    -0.325
#>  6 1           comp     0.667
#>  7 1           ref      0    
#>  8 1           dif      0.667
#>  9 1           low      0.325
#> 10 1           high     1    
#> 
#> 
#> $cyl
#> list()
#> 
 get_stats_data(t, layer=c(3, 2))
#> $am
#> $am$riskdiff
#> # A tibble: 10 × 3
#>    summary_var measure  `4_3`
#>    <chr>       <chr>    <dbl>
#>  1 0           comp     0.333
#>  2 0           ref      1    
#>  3 0           dif     -0.667
#>  4 0           low     -1    
#>  5 0           high    -0.325
#>  6 1           comp     0.667
#>  7 1           ref      0    
#>  8 1           dif      0.667
#>  9 1           low      0.325
#> 10 1           high     1    
#> 
#> 
#> $cyl
#> list()
#> 

 # Return just the statistic data for "am" and "cyl" - a list
 get_stats_data(t, layer=c("am", "cyl"), statistic="riskdiff")
#> $am
#> # A tibble: 10 × 3
#>    summary_var measure  `4_3`
#>    <chr>       <chr>    <dbl>
#>  1 0           comp     0.333
#>  2 0           ref      1    
#>  3 0           dif     -0.667
#>  4 0           low     -1    
#>  5 0           high    -0.325
#>  6 1           comp     0.667
#>  7 1           ref      0    
#>  8 1           dif      0.667
#>  9 1           low      0.325
#> 10 1           high     1    
#> 
#> $cyl
#> NULL
#> 
 get_stats_data(t, layer=c(3, 2), statistic="riskdiff")
#> $am
#> # A tibble: 10 × 3
#>    summary_var measure  `4_3`
#>    <chr>       <chr>    <dbl>
#>  1 0           comp     0.333
#>  2 0           ref      1    
#>  3 0           dif     -0.667
#>  4 0           low     -1    
#>  5 0           high    -0.325
#>  6 1           comp     0.667
#>  7 1           ref      0    
#>  8 1           dif      0.667
#>  9 1           low      0.325
#> 10 1           high     1    
#> 
#> $cyl
#> NULL
#> 


 # Return the riskdiff for the "am" layer - a data frame
 get_stats_data(t, layer="am", statistic="riskdiff")
#> # A tibble: 10 × 3
#>    summary_var measure  `4_3`
#>    <chr>       <chr>    <dbl>
#>  1 0           comp     0.333
#>  2 0           ref      1    
#>  3 0           dif     -0.667
#>  4 0           low     -1    
#>  5 0           high    -0.325
#>  6 1           comp     0.667
#>  7 1           ref      0    
#>  8 1           dif      0.667
#>  9 1           low      0.325
#> 10 1           high     1    

 # Return and filter the riskdiff for the am layer - a data frame
 get_stats_data(t, layer="am", statistic="riskdiff", where = summary_var==1)
#> # A tibble: 5 × 3
#>   summary_var measure `4_3`
#>   <chr>       <chr>   <dbl>
#> 1 1           comp    0.667
#> 2 1           ref     0    
#> 3 1           dif     0.667
#> 4 1           low     0.325
#> 5 1           high    1    
```
