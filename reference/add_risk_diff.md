# Add risk difference to a count layer

A very common requirement for summary tables is to calculate the risk
difference between treatment groups. `add_risk_diff` allows you to do
this. The underlying risk difference calculations are performed using
the Base R function
[`prop.test`](https://rdrr.io/r/stats/prop.test.html) - so prior to
using this function, be sure to familiarize yourself with its
functionality.

## Usage

``` r
add_risk_diff(layer, ..., args = list(), distinct = TRUE)
```

## Arguments

- layer:

  Layer upon which the risk difference will be attached

- ...:

  Comparison groups, provided as character vectors where the first group
  is the comparison, and the second is the reference

- args:

  Arguments passed directly into
  [`prop.test`](https://rdrr.io/r/stats/prop.test.html)

- distinct:

  Logical - Use distinct counts (if available).

## Details

`add_risk_diff` can only be attached to a count layer, so the count
layer must be constructed first. `add_risk_diff` allows you to compare
the difference between treatment group, so all comparisons should be
based upon the values within the specified `treat_var` in your
`tplyr_table` object.

Comparisons are specified by providing two-element character vectors.
You can provide as many of these groups as you want. You can also use
groups that have been constructed using
[`add_treat_grps`](https://atorus-research.github.io/Tplyr/reference/treat_grps.md)
or
[`add_total_group`](https://atorus-research.github.io/Tplyr/reference/treat_grps.md).
The first element provided will be considered the 'reference' group
(i.e. the left side of the comparison), and the second group will be
considered the 'comparison'. So if you'd like to see the risk difference
of 'T1 - Placebo', you would specify this as `c('T1', 'Placebo')`.

Tplyr forms your two-way table in the background, and then runs
[`prop.test`](https://rdrr.io/r/stats/prop.test.html) appropriately.
Similar to way that the display of layers are specified, the exact
values and format of how you'd like the risk difference display are set
using
[`set_format_strings`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md).
This controls both the values and the format of how the risk difference
is displayed. Risk difference formats are set within
[`set_format_strings`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md)
by using the name 'riskdiff'.

You have 5 variables to choose from in your data presentation:

- **comp**:

  Probability of the left hand side group (i.e. comparison)

- **ref**:

  Probability of the right hand side group (i.e. reference)

- **dif**:

  Difference of comparison - reference

- **low**:

  Lower end of the confidence interval (default is 95%, override with
  the `args` paramter)

- **high**:

  Upper end of the confidence interval (default is 95%, override with
  the `args` paramter)

Use these variable names when forming your
[`f_str`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
objects. The default presentation, if no string format is specified,
will be:

`f_str('xx.xxx (xx.xxx, xx.xxx)', dif, low, high)`

Note - within Tplyr, you can account for negatives by allowing an extra
space within your integer side settings. This will help with your
alignment.

If columns are specified on a Tplyr table, risk difference comparisons
still only take place between groups within the `treat_var` variable -
but they are instead calculated treating the `cols` variables as by
variables. Just like the tplyr layers themselves, the risk difference
will then be transposed and display each risk difference as separate
variables by each of the `cols` variables.

If `distinct` is TRUE (the default), all calculations will take place on
the distinct counts, if they are available. Otherwise, non-distinct
counts will be used.

One final note - [`prop.test`](https://rdrr.io/r/stats/prop.test.html)
may throw quite a few warnings. This is natural, because it alerts you
when there's not enough data for the approximations to be correct. This
may be unnerving coming from a SAS programming world, but this is R is
trying to alert you that the values provided don't have enough data to
truly be statistically accurate.

## Examples

``` r
library(magrittr)

## Two group comparisons with default options applied
t <- tplyr_table(mtcars, gear)

# Basic risk diff for two groups, using defaults
l1 <- group_count(t, carb) %>%
  # Compare 3 vs. 4, 3 vs. 5
  add_risk_diff(
    c('3', '4'),
    c('3', '5')
  )

# Build and show output
add_layers(t, l1) %>% build()
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> # A tibble: 6 × 8
#>   row_label1 var1_3     var1_4     var1_5    ord_layer_index rdiff_3_4 rdiff_3_5
#>   <chr>      <chr>      <chr>      <chr>               <int> <chr>     <chr>    
#> 1 1          3 ( 20.0%) 4 ( 33.3%) 0 (  0.0…               1 "-0.133 … " 0.200 …
#> 2 2          4 ( 26.7%) 4 ( 33.3%) 2 ( 40.0…               1 "-0.067 … "-0.133 …
#> 3 3          3 ( 20.0%) 0 (  0.0%) 0 (  0.0…               1 " 0.200 … " 0.200 …
#> 4 4          5 ( 33.3%) 4 ( 33.3%) 1 ( 20.0…               1 " 0.000 … " 0.133 …
#> 5 6          0 (  0.0%) 0 (  0.0%) 1 ( 20.0…               1 " 0.000 … "-0.200 …
#> 6 8          0 (  0.0%) 0 (  0.0%) 1 ( 20.0…               1 " 0.000 … "-0.200 …
#> # ℹ 1 more variable: ord_layer_1 <dbl>

## Specify custom formats and display variables
t <- tplyr_table(mtcars, gear)

# Create the layer with custom formatting
l2 <- group_count(t, carb) %>%
  # Compare 3 vs. 4, 3 vs. 5
  add_risk_diff(
    c('3', '4'),
    c('3', '5')
  ) %>%
  set_format_strings(
    'n_counts' = f_str('xx (xx.x)', n, pct),
    'riskdiff' = f_str('xx.xxx, xx.xxx, xx.xxx, xx.xxx, xx.xxx', comp, ref, dif, low, high)
  )

# Build and show output
add_layers(t, l2) %>% build()
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> # A tibble: 6 × 8
#>   row_label1 var1_3      var1_4      var1_5  ord_layer_index rdiff_3_4 rdiff_3_5
#>   <chr>      <chr>       <chr>       <chr>             <int> <chr>     <chr>    
#> 1 1          " 3 (20.0)" " 4 (33.3)" " 0 ( …               1 " 0.200,… " 0.200,…
#> 2 2          " 4 (26.7)" " 4 (33.3)" " 2 (4…               1 " 0.267,… " 0.267,…
#> 3 3          " 3 (20.0)" " 0 ( 0.0)" " 0 ( …               1 " 0.200,… " 0.200,…
#> 4 4          " 5 (33.3)" " 4 (33.3)" " 1 (2…               1 " 0.333,… " 0.333,…
#> 5 6          " 0 ( 0.0)" " 0 ( 0.0)" " 1 (2…               1 " 0.000,… " 0.000,…
#> 6 8          " 0 ( 0.0)" " 0 ( 0.0)" " 1 (2…               1 " 0.000,… " 0.000,…
#> # ℹ 1 more variable: ord_layer_1 <dbl>

## Passing arguments to prop.test
t <- tplyr_table(mtcars, gear)

# Create the layer with args option
l3 <- group_count(t, carb) %>%
  # Compare 3 vs. 4, 4 vs. 5
  add_risk_diff(
    c('3', '4'),
    c('3', '5'),
    args = list(conf.level = 0.9, correct=FALSE, alternative='less')
  )

# Build and show output
add_layers(t, l3) %>% build()
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> # A tibble: 6 × 8
#>   row_label1 var1_3     var1_4     var1_5    ord_layer_index rdiff_3_4 rdiff_3_5
#>   <chr>      <chr>      <chr>      <chr>               <int> <chr>     <chr>    
#> 1 1          3 ( 20.0%) 4 ( 33.3%) 0 (  0.0…               1 "-0.133 … " 0.200 …
#> 2 2          4 ( 26.7%) 4 ( 33.3%) 2 ( 40.0…               1 "-0.067 … "-0.133 …
#> 3 3          3 ( 20.0%) 0 (  0.0%) 0 (  0.0…               1 " 0.200 … " 0.200 …
#> 4 4          5 ( 33.3%) 4 ( 33.3%) 1 ( 20.0…               1 " 0.000 … " 0.133 …
#> 5 6          0 (  0.0%) 0 (  0.0%) 1 ( 20.0…               1 " 0.000 … "-0.200 …
#> 6 8          0 (  0.0%) 0 (  0.0%) 1 ( 20.0…               1 " 0.000 … "-0.200 …
#> # ℹ 1 more variable: ord_layer_1 <dbl>
```
