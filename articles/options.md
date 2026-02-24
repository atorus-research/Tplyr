# Tplyr Options

One thing that we wanted to build into **Tplyr** to make it a user
friendly package is the ability to eliminate redundant code where
possible. This is why there are several options available in **Tplyr**
to allow you to control things at your session level instead of each
individual table, or even each individual layer.

The following are the options available in **Tplyr** and their
descriptions:

|              Option               | Description                                                                                                                                                                                                                                                                                      |
|:---------------------------------:|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| tplyr.count_layer_default_formats | The default format strings for a count layer. Defaults to an auto-calculated n width, and (xxx.x%) for distinct counts (if available - non-distinct counts are used otherwise). Risk difference formats default to dif (low CI, high CI), all with 2 integer spaces and 3 decimal places.        |
| tplyr.shift_layer_default_formats | The default shift layer format. Defaults to an auto-calculated n width.                                                                                                                                                                                                                          |
| tplyr.desc_layer_default_formats  | The default descriptive statistics layer format. Defaults to “n”, “Mean (SD)”, “Median”, “Q1, Q3”, “Min, Max”, and “Missing”. Everything except “n” and “Missing” use auto-precision. Mean, Q1, Q3, and median defaults to +1 decimal places and Standard Deviation defaults to +2.              |
|        tplyr.precision_cap        | The default precision cap for auto-precision. Both integer and decimal places default to 99, essentially ensuring that precision is not capped by default.                                                                                                                                       |
|      tplyr.custom_summaries       | Default custom summaries available to Tplyr. Defaults to NULL, as Tplyr’s defaults are seen as built-ins and not custom summaries.                                                                                                                                                               |
|           tplyr.scipen            | The default ‘scipen’ setting used while Tplyr is executing. Defaults to 1000. See the R documentation on the ‘scipen’ option to understand more, but this allows you to control how small a number must be before scientific notation is used when a number is string formatted in presentation. |
|        tplyr.quantile_type        | The default quantile algorithm used by Tpylr when using the built-in summaries for Q1, Q3, and IQR. Defaults to Type 7, which is the R default                                                                                                                                                   |

Each of these options allows you to set these settings in one place, and
every **Tplyr** table you create will inherit your option settings as
defaults. This allows your table code to be more concise, and
centralizes where you need to make an update when your code has to be
adjusted. This vignette is dedicated to helping you understand how to
leverage each of these options properly.

## Default Layer Formats

Declaring string formats and summaries that need to be performed is one
of the more verbose parts of **Tplyr**. Furthermore, this is something
that will often be fairly consistent within a study, as you’ll likely
want to look across a consistent set of descriptive statistics, or your
count/shift tables will likely require the same sort of “n (%)”
formatting.

Using the format options is very similar to setting a string format. The
only difference is that you need to enter the string formats as a named
list instead of as separate parameters to a function call.

``` r
options(
    # Count layer defaults
  tplyr.count_layer_default_formats =
    list(n_counts = f_str("xxx (xx%)", n, pct),
         riskdiff = f_str('xx.xxx', dif)
         ),

  # Desc layer defaults
  tplyr.desc_layer_default_formats =
    list("n"        = f_str("xxx", n),
         "Mean (SD)"= f_str("a.a+1 (a.a+2)", mean, sd),
         "Median"   = f_str("a.a+4", median)
         ),

  # Shift layer defaults
  tplyr.shift_layer_default_formats = list(f_str("xxx", n))
)
 
```

Here you can see that **Tplyr** picks up these option changes. In the
table below, we didn’t use
[`set_format_strings()`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md)
anywhere - instead we let **Tplyr** pick up the default formats from the
options.

``` r
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_desc(AGE, by = "Age (years)")
  ) %>% 
  add_layer(
    group_count(AGEGR1, by = "Categorical Age Groups")
  ) %>% 
  build() %>% 
  kable()
```

| row_label1             | row_label2 | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:-----------------------|:-----------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|------------:|
| Age (years)            | n          | 86           | 84                        | 84                       |               1 |           1 |           1 |
| Age (years)            | Mean (SD)  | 75.2 ( 8.59) | 74.4 ( 7.89)              | 75.7 ( 8.29)             |               1 |           1 |           2 |
| Age (years)            | Median     | 76.0         | 76.0                      | 77.5                     |               1 |           1 |           3 |
| Age (years)            | Q1, Q3     | 69.2, 81.8   | 70.8, 80.0                | 71.0, 82.0               |               1 |           1 |           4 |
| Age (years)            | Min, Max   | 52, 89       | 56, 88                    | 51, 88                   |               1 |           1 |           5 |
| Age (years)            | Missing    | 0            | 0                         | 0                        |               1 |           1 |           6 |
| Categorical Age Groups | \<65       | 14 ( 16.3%)  | 11 ( 13.1%)               | 8 ( 9.5%)                |               2 |           1 |           1 |
| Categorical Age Groups | \>80       | 30 ( 34.9%)  | 18 ( 21.4%)               | 29 ( 34.5%)              |               2 |           1 |           2 |
| Categorical Age Groups | 65-80      | 42 ( 48.8%)  | 55 ( 65.5%)               | 47 ( 56.0%)              |               2 |           1 |           3 |

One important thing to understand about how these options work in
particular is the scoping.

- **Tplyr** options have the broadest scope, as they work across the
  entire session for any table.
- Setting formats at the
  [`tplyr_table()`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)
  level will override the **Tplyr** options and extends to any layer of
  the specified type in the current table.
- Setting formats at the
  [`tplyr_layer()`](https://atorus-research.github.io/Tplyr/reference/tplyr_layer.md)
  level will always be prioritized over **Tplyr** options and any
  [`tplyr_table()`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)
  formats set. This has the narrowest scope and will always be used when
  specified.

To demonstrate, consider the following. The **Tplyr** options remain set
from the block above.

``` r
tplyr_table(tplyr_adsl, TRT01P) %>% 
  set_count_layer_formats(n_counts = f_str("xx (xxx%)", n, pct)) %>% 
  set_desc_layer_formats("Mean (SD)" = f_str("a.a+1 (a.a+2)", mean, sd)) %>% 
  add_layer(
    group_desc(AGE, by = "Age (Years)")
  ) %>% 
  add_layer(
    group_count(AGEGR1, by = "Categorical Age Groups")
  ) %>% 
  add_layer(
    group_count(ETHNIC, by = "Ethnicity") %>% 
      set_format_strings(f_str("xxxxx (xx.xxx%)", n, pct))
  ) %>% 
  build() %>% 
  kable()
```

| row_label1             | row_label2             | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:-----------------------|:-----------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|------------:|
| Age (Years)            | Mean (SD)              | 75.2 ( 8.59) | 74.4 ( 7.89)              | 75.7 ( 8.29)             |               1 |           1 |           1 |
| Categorical Age Groups | \<65                   | 14 ( 16%)    | 11 ( 13%)                 | 8 ( 10%)                 |               2 |           1 |           1 |
| Categorical Age Groups | \>80                   | 30 ( 35%)    | 18 ( 21%)                 | 29 ( 35%)                |               2 |           1 |           2 |
| Categorical Age Groups | 65-80                  | 42 ( 49%)    | 55 ( 65%)                 | 47 ( 56%)                |               2 |           1 |           3 |
| Ethnicity              | HISPANIC OR LATINO     | 3 ( 3.488%)  | 3 ( 3.571%)               | 6 ( 7.143%)              |               3 |           1 |           1 |
| Ethnicity              | NOT HISPANIC OR LATINO | 83 (96.512%) | 81 (96.429%)              | 78 (92.857%)             |               3 |           1 |           2 |

In the above output:

- The descriptive statistics layer for “Age (Years)” uses the specified
  table default using
  [`set_desc_layer_formats()`](https://atorus-research.github.io/Tplyr/reference/table_format_defaults.md)
- The first count layer for “Categorical Age Groups” uses the specified
  table default using
  [`set_count_layer_formats()`](https://atorus-research.github.io/Tplyr/reference/table_format_defaults.md)
- The second count layer for “Ethnicity” uses the layer level format
  specified using
  [`set_format_strings()`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md)

Each of the outputs ignores the **Tplyr** option defaults.

## Precision Cap

**Tplyr** defaults to avoiding capping precision. We do this because
capping precision should be a conscious decision, where you as the user
specifically set the limit of how many decimal places are relevant to a
specific result. One way to cap precision is by using the `cap`
parameter within
[`set_format_strings()`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md).
But perhaps you have a specific limit to what you’d like to see on any
output. Here, we offer the `tplyr.precision_cap` option to set whatever
cap you wish.

``` r
options(tplyr.precision_cap = c('int'=2, 'dec'=2))
```

Similar to the layer defaults, setting a precision cap at the layer
level will override the `tplyr.precision_cap` option.

``` r
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_desc(HEIGHTBL, by = "Height at Baseline") %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a (a.a+1)', mean, sd)
      )
  ) %>% 
  add_layer(
    group_desc(HEIGHTBL, by = "Height at Baseline (Limited)") %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a (a.a+1)', mean, sd),
        cap = c('int'= 1, 'dec'=0)
      )
  ) %>% 
  build() %>% 
  kable()
```

| row_label1                   | row_label2 | var1_Placebo  | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:-----------------------------|:-----------|:--------------|:--------------------------|:-------------------------|----------------:|------------:|------------:|
| Height at Baseline           | Mean (SD)  | 162.6 (11.52) | 165.8 (10.13)             | 163.4 (10.42)            |               1 |           1 |           1 |
| Height at Baseline (Limited) | Mean (SD)  | 163 (11.5)    | 166 (10.1)                | 163 (10.4)               |               2 |           1 |           1 |

Both layers in the above example are summarizing the same data. The top
layer is using the `tplyr.precision_cap` option set above, which limits
to 2 integer place and 2 decimal places. To bottom layer applies its own
cap to override the option. Recall a few things about auto precision:

- Integers will not be truncated - rather, the auto precision will
  default how much padding should be applied. If the number exceeds that
  number of spaces, then the length will be extended (i.e. if 2 integer
  places are allotted, 100 will still consume 3 places).
- The cap applies to the spaces allotted by the ‘a’. If the cap is 2,
  ‘a+1’ will not exceed 3 spaces.

The bottom layer overrides the **Tplyr** option. Instead, integers are
capped at 1 space, and decimals are capped at 0.

## Custom Summaries

Custom summaries allow you to extend the capabilities of descriptive
statistics layers in **Tplyr**. Maybe our defaults don’t work how you’d
like them to, or maybe you have some custom functions within your
organization that you commonly would like to use. Specifying the custom
summaries you wish to use in every table would prove quite tedious -
therefore, the `tplyr.custom_summaries` option is a better choice.

``` r
options(tplyr.custom_summaries = rlang::quos(
  geometric_mean = exp(sum(log(.var[.var > 0]),na.rm=TRUE) / length(.var))
))
```

Note that the table code used to produce the output is the same. Now
**Tplyr** used the custom summary function for `geometric_mean` as
specified in the `tplyr.custom_summaries` option. Also note the use of
[`rlang::quos()`](https://rlang.r-lib.org/reference/defusing-advanced.html).
We’ve done our best to mask this from the user everywhere possible and
make the interfaces clean and intuitive, but a great deal of **Tplyr**
is built using ‘rlang’ and non-standard evaluation. Within this option
is one of the very few instances where a user needs to concern
themselves with the use of quosures. If you’d like to learn more about
non-standard evaluation and quosures, we recommend [Section
IV](https://adv-r.hadley.nz/metaprogramming.html) in Advanced R.

Now that geometric mean is set within the **Tplyr** options, you can use
it within your descriptive statistics layers, just like it was one of
the built-in summaries.

``` r
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_desc(AGE) %>% 
      set_format_strings('Geometric Mean' = f_str('xx.xx', geometric_mean))
  ) %>% 
  build() %>% 
  kable()
```

| row_label1     | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:---------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|
| Geometric Mean | 74.70        | 73.94                     | 75.18                    |               1 |           1 |

## Scientific Notation

By default, R will switch to scientific notation for a number less than
.001. This is controlled by the `scipen` option. The default value of
`scipen` is 0. If you’d like to increase the decimal places required for
scientific notation to be triggered, increase the value of `scipen`. The
value of `scipen` is the number of orders of ten smaller (i.e. decimal
places preceded by 0’s) required to switch to scientific notation.
Decreasing the value of `scipen` will cause R to switch to scientific
location for larger numbers.

This is easier to understand with an example.

``` r
options(scipen = 0) # This is the default
.0001
#> [1] 1e-04

options(scipen = 1) # Require 5 decimal places instead

.0001
#> [1] 0.0001
.00001
#> [1] 1e-05

options(scipen = -1) # Only require 3 decimal places
.001
#> [1] 1e-03
```

In **Tplyr**, we have the option `tplyr.scipen`. This is the `scipen`
setting that will be used *only* while the **Tplyr** table is being
built. This allows you to use a different `scipen` setting within
**Tplyr** than your R session. The default value we use in **Tplyr** is
9999, which is intended to totally prevent numbers from switching to
scientific notation. We want this to be a conscious decision that you
make in order to prevent any unexpected outputs.

``` r
options(tplyr.scipen = -3)
t <- tplyr_table(tplyr_adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      add_risk_diff(c('Xanomeline Low Dose', 'Placebo'))
  )

suppressWarnings(build(t)) %>% # Chi-squared warnings occur with small samples
  head() %>% 
  kable()
```

| row_label1         | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | rdiff_Xanomeline Low Dose_Placebo | ord_layer_1 |
|:-------------------|:-------------|:--------------------------|:-------------------------|----------------:|:----------------------------------|------------:|
| ACTINIC KERATOSIS  | 0 ( 0.0%)    | 1 ( 0.9%)                 | 0 ( 0.0%)                |               1 | 0.000 ( 0.000, 0.000)             |           1 |
| ALOPECIA           | 1 ( 2.1%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 | -2.1e-02 (-7.7e-02, 3.5e-02)      |           2 |
| BLISTER            | 0 ( 0.0%)    | 2 ( 1.8%)                 | 8 ( 6.8%)                |               1 | 6.8e-02 ( 8e-03, 0.128)           |           3 |
| COLD SWEAT         | 3 ( 6.4%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 | -6.4e-02 (-0.149, 2.1e-02)        |           4 |
| DERMATITIS ATOPIC  | 1 ( 2.1%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 | -2.1e-02 (-7.7e-02, 3.5e-02)      |           5 |
| DERMATITIS CONTACT | 0 ( 0.0%)    | 0 ( 0.0%)                 | 2 ( 1.7%)                |               1 | 1.7e-02 (-2.1e-02, 5.5e-02)       |           6 |

Note that the risk-difference variables above have mostly shifted to
scientific notation. This is because the limit has been shifted to .1
within the **Tplyr** build.

## Quantile Algorithms

There are many algorithms available to compute quantile R has 9,
controlled by the `type` parameter in
[`quantile()`](https://rdrr.io/r/stats/quantile.html). The descriptive
statistics offer built-in summaries for Q1, Q3, and Interquartile range,
all three of which use the
[`quantile()`](https://rdrr.io/r/stats/quantile.html) in the underlying
implementation. Given that we offer this default, we felt it was
important to offer you the flexibility to change the algorithm. You can
do this with `tplyr.quantile_type`.

The default we chose to use is the R default of Type 7:

$$m = 1 - p.p\lbrack k\rbrack = (k - 1)/(n - 1).{\text{In this case,}\mspace{6mu}}p\lbrack k\rbrack = mode\left\lbrack F\left( x\lbrack k\rbrack \right) \right\rbrack.\text{This is used by S.}$$
The example below demonstrates using the default quantile algorithm in R

``` r
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_desc(CUMDOSE) %>% 
      set_format_strings("Q1, Q3" = f_str('xxxxx, xxxxx', q1, q3))
  ) %>% 
  build() %>%
  kable()
```

| row_label1 | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:-----------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|
| Q1, Q3     | 0, 0         | 2646, 13959               | 1984, 9801               |               1 |           1 |

Within the clinical world, you may wish to match the way that SAS
calculates quantiles. To match SAS’s definition, use Type 3:

$$\text{Nearest even order statistic. γ = 0 if g = 0 and j is even, and 1 otherwise.}$$

``` r
options(tplyr.quantile_type = 3)

tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_desc(CUMDOSE) %>% 
      set_format_strings("Q1, Q3" = f_str('xxxxx, xxxxx', q1, q3))
  ) %>% 
  build() %>% 
    select(-starts_with("ord")) %>% 
  kable()
```

| row_label1 | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose |
|:-----------|:-------------|:--------------------------|:-------------------------|
| Q1, Q3     | 0, 0         | 2565, 13959               | 1944, 9774               |

## IBM Rounding

In certain cases users may want to match tables produced by other
languages that IBM rounding.**Tplyr** offers the option
‘tplyr.IBMRounding’ to change the default rounding behavior of **Tplyr**
tables. Review var1_4 in the tables below.

Using the default R behavior

``` r
tplyr_table(mtcars, gear) %>%
  add_layer(
    group_desc(qsec) %>%
      set_format_strings(mean = f_str("xx.xx", mean))
  ) %>%
  build()
#> # A tibble: 1 × 6
#>   row_label1 var1_3 var1_4 var1_5 ord_layer_index ord_layer_1
#>   <chr>      <chr>  <chr>  <chr>            <int>       <int>
#> 1 mean       17.69  18.96  15.64                1           1
```

Using IBM rounding

``` r
withr::with_options(
  list(tplyr.IBMRounding = TRUE),
  {
    tplyr_table(mtcars, gear) %>%
      add_layer(
        group_desc(qsec) %>%
          set_format_strings(mean = f_str("xx.xx", mean))
      ) %>%
      build()
  }
)
#> Warning: You have enabled IBM Rounding. This is an experimental feature.
#> •  If you have feedback please get in touch with the maintainers!
#> This warning is displayed once every 8 hours.
#> # A tibble: 1 × 6
#>   row_label1 var1_3 var1_4 var1_5 ord_layer_index ord_layer_1
#>   <chr>      <chr>  <chr>  <chr>            <int>       <int>
#> 1 mean       17.69  18.97  15.64                1           1
```
