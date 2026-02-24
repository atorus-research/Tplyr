# Set the format strings and associated summaries to be performed in a layer

'Tplyr' gives you extensive control over how strings are presented.
`set_format_strings` allows you to apply these string formats to your
layer. This behaves slightly differently between layers.

## Usage

``` r
set_format_strings(e, ...)

# S3 method for class 'desc_layer'
set_format_strings(e, ..., cap = getOption("tplyr.precision_cap"))

# S3 method for class 'count_layer'
set_format_strings(e, ...)
```

## Arguments

- e:

  Layer on which to bind format strings

- ...:

  Named parameters containing calls to `f_str` to set the format strings

- cap:

  A named character vector containing an 'int' element for the cap on
  integer precision, and a 'dec' element for the cap on decimal
  precision.

## Value

The layer environment with the format string binding added

tplyr_layer object with formats attached

Returns the modified layer object.

## Details

Format strings are one of the most powerful components of 'Tplyr'.
Traditionally, converting numeric values into strings for presentation
can consume a good deal of time. Values and decimals need to align
between rows, rounding before trimming is sometimes forgotten - it can
become a tedious mess that, in the grand scheme of things, is not an
important part of the analysis being performed. 'Tplyr' makes this
process as simple as we can, while still allowing flexibility to the
user.

In a count layer, you can simply provide a single
[`f_str`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
object to specify how you want your n's, percentages, and denominators
formatted. If you are additionally supplying a statistic, like risk
difference using
[`add_risk_diff`](https://atorus-research.github.io/Tplyr/reference/add_risk_diff.md),
you specify the count formats using the name 'n_counts'. The risk
difference formats would then be specified using the name 'riskdiff'. In
a descriptive statistic layer, `set_format_strings` allows you to do a
couple more things:

- By naming parameters with character strings, those character strings
  become a row label in the resulting data frame

- The actual summaries that are performed come from the variable names
  used within the
  [`f_str`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
  calls

- Using multiple summaries (declared by your
  [`f_str`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
  calls), multiple summary values can appear within the same line. For
  example, to present "Mean (SD)" like displays.

- Format strings in the desc layer also allow you to configure how empty
  values should be presented. In the `f_str` call, use the `empty`
  parameter to specify how missing values should present. A single
  element character vector should be provided. If the vector is unnamed,
  that value will be used in the format string and fill the space
  similar to how the numbers will display. Meaning - if your empty
  string is 'NA' and your format string is 'xx (xxx)', the empty values
  will populate as 'NA ( NA)'. If you name the character vector in the
  'empty' parameter '.overall', like `empty = c(.overall='')`, then that
  exact string will fill the value instead. For example, providing 'NA'
  will instead create the formatted string as 'NA' exactly.

See the
[`f_str`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
documentation for more details about how this implementation works.

## Examples

``` r
# Load in pipe
library(magrittr)

# In a count layer
tplyr_table(mtcars, gear) %>%
  add_layer(
    group_count(cyl) %>%
      set_format_strings(f_str('xx (xx%)', n, pct))
  ) %>%
  build()
#> # A tibble: 3 × 6
#>   row_label1 var1_3     var1_4     var1_5     ord_layer_index ord_layer_1
#>   <chr>      <chr>      <chr>      <chr>                <int>       <dbl>
#> 1 4          " 1 ( 7%)" " 8 (67%)" " 2 (40%)"               1           1
#> 2 6          " 2 (13%)" " 4 (33%)" " 1 (20%)"               1           2
#> 3 8          "12 (80%)" " 0 ( 0%)" " 2 (40%)"               1           3

# In a descriptive statistics layer
tplyr_table(mtcars, gear) %>%
  add_layer(
    group_desc(mpg) %>%
      set_format_strings(
        "n"         = f_str("xx", n),
        "Mean (SD)" = f_str("xx.x", mean, empty='NA'),
        "SD"        = f_str("xx.xx", sd),
        "Median"    = f_str("xx.x", median),
        "Q1, Q3"    = f_str("xx, xx", q1, q3, empty=c(.overall='NA')),
        "Min, Max"  = f_str("xx, xx", min, max),
        "Missing"   = f_str("xx", missing)
      )
  ) %>%
  build()
#> # A tibble: 7 × 6
#>   row_label1 var1_3   var1_4   var1_5   ord_layer_index ord_layer_1
#>   <chr>      <chr>    <chr>    <chr>              <int>       <int>
#> 1 n          "15"     "12"     " 5"                   1           1
#> 2 Mean (SD)  "16.1"   "24.5"   "21.4"                 1           2
#> 3 SD         " 3.37"  " 5.28"  " 6.66"                1           3
#> 4 Median     "15.5"   "22.8"   "19.7"                 1           4
#> 5 Q1, Q3     "14, 18" "21, 28" "16, 26"               1           5
#> 6 Min, Max   "10, 22" "18, 34" "15, 30"               1           6
#> 7 Missing    " 0"     " 0"     " 0"                   1           7

# In a shift layer
tplyr_table(mtcars, am) %>%
  add_layer(
    group_shift(vars(row=gear, column=carb), by=cyl) %>%
    set_format_strings(f_str("xxx (xx.xx%)", n, pct))
  ) %>%
  build()
#> # A tibble: 9 × 17
#>   row_label1 row_label2 var1_0_1    var1_0_2 var1_0_3 var1_0_4 var1_0_6 var1_0_8
#>   <chr>      <chr>      <chr>       <chr>    <chr>    <chr>    <chr>    <chr>   
#> 1 4          3          "  1 (33.3… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 2 4          4          "  0 ( 0.0… "  2 (6… "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 3 4          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 4 6          3          "  2 (50.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 5 6          4          "  0 ( 0.0… "  0 ( … "  0 ( … "  2 (5… "  0 ( … "  0 ( …
#> 6 6          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 7 8          3          "  0 ( 0.0… "  4 (3… "  3 (2… "  5 (4… "  0 ( … "  0 ( …
#> 8 8          4          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 9 8          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> # ℹ 9 more variables: var1_1_1 <chr>, var1_1_2 <chr>, var1_1_3 <chr>,
#> #   var1_1_4 <chr>, var1_1_6 <chr>, var1_1_8 <chr>, ord_layer_index <int>,
#> #   ord_layer_1 <dbl>, ord_layer_2 <dbl>
```
