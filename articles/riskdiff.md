# Risk Difference

**Tplyr** does not support, nor do we intend to support, a wide array of
statistical methods. Our goal is rather to take your focus as an analyst
off the mundane summaries so you can focus on the interesting analysis.
That said, there are some things that are common enough that we feel
that it’s reasonable for us to include. So let’s take a look at risk
difference.

## **Tplyr** Implementation

Our current implementation of risk difference is solely built on top of
the base R function
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html). For any
and all questions about this method, please review the
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html)
documentation within R.

Risk difference is built on top of count layers, as it’s a comparison of
proportions. To add a risk difference calculation into a count layer,
you simply use the function
[`add_risk_diff()`](https://atorus-research.github.io/Tplyr/reference/add_risk_diff.md).
We made a large effort to make this flow very naturally with the count
layer construction, so let’s walk through it step by step.

``` r
t <- tplyr_table(tplyr_adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID) %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo')
      )
  )

suppressWarnings(build(t)) %>% 
  head() %>% 
  select(starts_with("rdiff"), everything()) %>% 
  kable()
```

| rdiff_Xanomeline High Dose_Placebo | rdiff_Xanomeline Low Dose_Placebo | row_label1         | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:-----------------------------------|:----------------------------------|:-------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|
| 0.024 (-0.046, 0.094)              | 0.000 ( 0.000, 0.000)             | ACTINIC KERATOSIS  | 0 ( 0.0%)    | 1 ( 2.4%)                 | 0 ( 0.0%)                |               1 |           1 |
| -0.048 (-0.174, 0.079)             | -0.048 (-0.174, 0.079)            | ALOPECIA           | 1 ( 4.8%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           2 |
| 0.024 (-0.046, 0.094)              | 0.119 (-0.015, 0.253)             | BLISTER            | 0 ( 0.0%)    | 1 ( 2.4%)                 | 5 ( 11.9%)               |               1 |           3 |
| -0.048 (-0.174, 0.079)             | -0.048 (-0.174, 0.079)            | COLD SWEAT         | 1 ( 4.8%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           4 |
| -0.048 (-0.174, 0.079)             | -0.048 (-0.174, 0.079)            | DERMATITIS ATOPIC  | 1 ( 4.8%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           5 |
| 0.000 ( 0.000, 0.000)              | 0.024 (-0.046, 0.094)             | DERMATITIS CONTACT | 0 ( 0.0%)    | 0 ( 0.0%)                 | 1 ( 2.4%)                |               1 |           6 |

Comparisons are specified with two-element character vectors. These are
simply your comparison group - the first element, and your reference
group - the second. This coincides with how you might see risk
difference specified in the header of your mock, where you’ll see
something like T1-Placebo. You can provide as many comparisons as you
want - the values specified in the comparison just need to be valid
treatment groups within your data. This works with any treatment group
built using
[`add_treat_grps()`](https://atorus-research.github.io/Tplyr/reference/treat_grps.md)
or
[`add_total_group()`](https://atorus-research.github.io/Tplyr/reference/treat_grps.md)
as well.

The risk difference calculations are displayed in the `rdiff` columns.
There will be an `rdiff` column for every comparison that is made,
following the convention `rdiff_<comparison>_<reference>`.

Note the use of
[`base::suppressWarnings()`](https://rdrr.io/r/base/warning.html) - if
the counts used in
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html) are too
low, you’ll get a warning that says “Chi-squared approximation may be
incorrect” for every time
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html) is run
with counts that are too low… This could happen a lot, but the warning
is perfectly valid.

## Controlling Presentation

The default values presented within formatted strings in the built table
will be:

- The difference
- 95% confidence interval low
- 95% confidence interval high

You have a good bit of control over these values though, and this can be
controlled in the same way you format the count summaries - using
[`set_format_strings()`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md).

``` r
t <- tplyr_table(tplyr_adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID) %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo')
      ) %>% 
      set_format_strings(
        'n_counts' = f_str('xx (xx.x) [x]', distinct_n, distinct_pct, n),
        'riskdiff' = f_str('xx.xxx, xx.xxx, xx.xxx, xx.xxx, xx.xxx', comp, ref, dif, low, high)
      )
  )

suppressWarnings(build(t)) %>% 
  head() %>% 
  select(starts_with("rdiff"), everything()) %>% 
  kable()
```

| rdiff_Xanomeline High Dose_Placebo  | rdiff_Xanomeline Low Dose_Placebo   | row_label1         | var1_Placebo   | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:------------------------------------|:------------------------------------|:-------------------|:---------------|:--------------------------|:-------------------------|----------------:|------------:|
| 0.024, 0.000, 0.024, -0.046, 0.094  | 0.000, 0.000, 0.000, 0.000, 0.000   | ACTINIC KERATOSIS  | 0 ( 0.0) \[0\] | 1 ( 2.4) \[1\]            | 0 ( 0.0) \[0\]           |               1 |           1 |
| 0.000, 0.048, -0.048, -0.174, 0.079 | 0.000, 0.048, -0.048, -0.174, 0.079 | ALOPECIA           | 1 ( 4.8) \[1\] | 0 ( 0.0) \[0\]            | 0 ( 0.0) \[0\]           |               1 |           2 |
| 0.024, 0.000, 0.024, -0.046, 0.094  | 0.119, 0.000, 0.119, -0.015, 0.253  | BLISTER            | 0 ( 0.0) \[0\] | 1 ( 2.4) \[2\]            | 5 (11.9) \[8\]           |               1 |           3 |
| 0.000, 0.048, -0.048, -0.174, 0.079 | 0.000, 0.048, -0.048, -0.174, 0.079 | COLD SWEAT         | 1 ( 4.8) \[3\] | 0 ( 0.0) \[0\]            | 0 ( 0.0) \[0\]           |               1 |           4 |
| 0.000, 0.048, -0.048, -0.174, 0.079 | 0.000, 0.048, -0.048, -0.174, 0.079 | DERMATITIS ATOPIC  | 1 ( 4.8) \[1\] | 0 ( 0.0) \[0\]            | 0 ( 0.0) \[0\]           |               1 |           5 |
| 0.000, 0.000, 0.000, 0.000, 0.000   | 0.024, 0.000, 0.024, -0.046, 0.094  | DERMATITIS CONTACT | 0 ( 0.0) \[0\] | 0 ( 0.0) \[0\]            | 1 ( 2.4) \[2\]           |               1 |           6 |

Take a look at the `rdiff` columns now - you’ll see they have 5 values.
These are:

- The comparison proportion (i.e. the estimate\[1\] output from a
  [`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html) object)
- The reference proportion (i.e. the estimate\[2\] output from a
  [`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html) object)
- The difference (i.e. estimate\[1\] - estimate\[2\])
- The lower end of the confidence interval
- The upper end of the confidence interval

You have the same control over the formatting of the display of these
values here as you do with the count summaries. Taking things a step
further, you can also pass forward arguments to
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html) using a
named list and the `args` argument in
[`add_risk_diff()`](https://atorus-research.github.io/Tplyr/reference/add_risk_diff.md).
This wasn’t done using the ellipsis (i.e. `...`) like typical R
functions because it’s already used to capture a varying number of
comparisons, but it’s not much more difficult to use:

``` r
t <- tplyr_table(tplyr_adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID) %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo'),
        args = list(conf.level=0.90, alternative='less', correct=FALSE)
      ) %>% 
      set_format_strings(
        'n_counts' = f_str('xx (xx.x) [x]', distinct_n, distinct_pct, n),
        'riskdiff' = f_str('xx.xxx, xx.xxx, xx.xxx, xx.xxx, xx.xxx', comp, ref, dif, low, high)
      )
  )

suppressWarnings(build(t)) %>% 
  head() %>%
  select(starts_with("rdiff"), everything()) %>%  
  kable()
```

| rdiff_Xanomeline High Dose_Placebo  | rdiff_Xanomeline Low Dose_Placebo   | row_label1         | var1_Placebo   | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:------------------------------------|:------------------------------------|:-------------------|:---------------|:--------------------------|:-------------------------|----------------:|------------:|
| 0.024, 0.000, 0.024, -1.000, 0.054  | 0.000, 0.000, 0.000, -1.000, 0.000  | ACTINIC KERATOSIS  | 0 ( 0.0) \[0\] | 1 ( 2.4) \[1\]            | 0 ( 0.0) \[0\]           |               1 |           1 |
| 0.000, 0.048, -0.048, -1.000, 0.012 | 0.000, 0.048, -0.048, -1.000, 0.012 | ALOPECIA           | 1 ( 4.8) \[1\] | 0 ( 0.0) \[0\]            | 0 ( 0.0) \[0\]           |               1 |           2 |
| 0.024, 0.000, 0.024, -1.000, 0.054  | 0.119, 0.000, 0.119, -1.000, 0.183  | BLISTER            | 0 ( 0.0) \[0\] | 1 ( 2.4) \[2\]            | 5 (11.9) \[8\]           |               1 |           3 |
| 0.000, 0.048, -0.048, -1.000, 0.012 | 0.000, 0.048, -0.048, -1.000, 0.012 | COLD SWEAT         | 1 ( 4.8) \[3\] | 0 ( 0.0) \[0\]            | 0 ( 0.0) \[0\]           |               1 |           4 |
| 0.000, 0.048, -0.048, -1.000, 0.012 | 0.000, 0.048, -0.048, -1.000, 0.012 | DERMATITIS ATOPIC  | 1 ( 4.8) \[1\] | 0 ( 0.0) \[0\]            | 0 ( 0.0) \[0\]           |               1 |           5 |
| 0.000, 0.000, 0.000, -1.000, 0.000  | 0.024, 0.000, 0.024, -1.000, 0.054  | DERMATITIS CONTACT | 0 ( 0.0) \[0\] | 0 ( 0.0) \[0\]            | 1 ( 2.4) \[2\]           |               1 |           6 |

As seen above, using the `args` argument, we:

- Changed the confidence interval level to 90% instead of the default
  95%
- Switched the alternative hypothesis of
  [`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html) to
  “less” instead of the default “two.sided”
- Turned off the Yates’ continuity correction

For more information on these parameters, see the documentation for
[`stats::prop.test()`](https://rdrr.io/r/stats/prop.test.html).

## Other Notes

The default of
[`add_risk_diff()`](https://atorus-research.github.io/Tplyr/reference/add_risk_diff.md)
works on the distinct counts available within the count summary.

``` r
t <- tplyr_table(tplyr_adae, TRTA, where= AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS") %>% 
  set_pop_data(tplyr_adsl) %>% 
  set_pop_treat_var(TRT01A) %>% 
  set_pop_where(TRUE) %>% 
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>% 
      set_distinct_by(USUBJID) %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo')
      ) 
  )

suppressWarnings(build(t)) %>% 
  head() %>% 
  select(starts_with("rdiff"), everything()) %>%  
  kable()
```

| rdiff_Xanomeline High Dose_Placebo | rdiff_Xanomeline Low Dose_Placebo | row_label1                             | row_label2                             | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:-----------------------------------|:----------------------------------|:---------------------------------------|:---------------------------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|------------:|
| 0.256 ( 0.104, 0.408)              | 0.256 ( 0.104, 0.408)             | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | 21 ( 24.4%)  | 42 ( 50.0%)               | 42 ( 50.0%)              |               1 |           1 |         Inf |
| 0.012 (-0.023, 0.047)              | 0.000 ( 0.000, 0.000)             | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | ACTINIC KERATOSIS                      | 0 ( 0.0%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               1 |           1 |           1 |
| -0.012 (-0.046, 0.023)             | -0.012 (-0.046, 0.023)            | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | ALOPECIA                               | 1 ( 1.2%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           1 |           2 |
| 0.012 (-0.023, 0.047)              | 0.060 (-0.003, 0.122)             | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | BLISTER                                | 0 ( 0.0%)    | 1 ( 1.2%)                 | 5 ( 6.0%)                |               1 |           1 |           3 |
| -0.012 (-0.046, 0.023)             | -0.012 (-0.046, 0.023)            | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | COLD SWEAT                             | 1 ( 1.2%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           1 |           4 |
| -0.012 (-0.046, 0.023)             | -0.012 (-0.046, 0.023)            | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | DERMATITIS ATOPIC                      | 1 ( 1.2%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           1 |           5 |

If for whatever reason you’d like to run risk difference on the
non-distinct counts, switch the `distinct` argument to FALSE.
[`add_risk_diff()`](https://atorus-research.github.io/Tplyr/reference/add_risk_diff.md)
also will function on multi-level summaries no different than single
level, so no concerns there either.

``` r
t <- tplyr_table(tplyr_adae, TRTA, where= AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS") %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID) %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo'),
        distinct=FALSE
      ) 
  )

suppressWarnings(build(t)) %>% 
  head() %>% 
  select(starts_with("rdiff"), everything()) %>%  
  kable()
```

| rdiff_Xanomeline High Dose_Placebo | rdiff_Xanomeline Low Dose_Placebo | row_label1         | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:-----------------------------------|:----------------------------------|:-------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|
| 0.009 (-0.018, 0.036)              | 0.000 ( 0.000, 0.000)             | ACTINIC KERATOSIS  | 0 ( 0.0%)    | 1 ( 2.4%)                 | 0 ( 0.0%)                |               1 |           1 |
| -0.021 (-0.078, 0.035)             | -0.021 (-0.077, 0.035)            | ALOPECIA           | 1 ( 4.8%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           2 |
| 0.018 (-0.022, 0.058)              | 0.068 ( 0.008, 0.128)             | BLISTER            | 0 ( 0.0%)    | 1 ( 2.4%)                 | 5 ( 11.9%)               |               1 |           3 |
| -0.064 (-0.149, 0.021)             | -0.064 (-0.149, 0.021)            | COLD SWEAT         | 1 ( 4.8%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           4 |
| -0.021 (-0.078, 0.035)             | -0.021 (-0.077, 0.035)            | DERMATITIS ATOPIC  | 1 ( 4.8%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           5 |
| 0.000 ( 0.000, 0.000)              | 0.017 (-0.021, 0.055)             | DERMATITIS CONTACT | 0 ( 0.0%)    | 0 ( 0.0%)                 | 1 ( 2.4%)                |               1 |           6 |

Risk difference also works with the `cols` argument, but it’s important
to understand how the comparisons work in these situation. Here, it’s
still the treatment groups that are compared - but the column argument
is used as a “by” variable. For example:

``` r
t <- tplyr_table(tplyr_adae, TRTA, where= AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS", cols=SEX) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID) %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo')
      ) 
  )

suppressWarnings(build(t)) %>% 
  head() %>% 
  select(starts_with("rdiff"), starts_with("row")) %>% 
  kable()
```

| rdiff_Xanomeline High Dose_Placebo_F | rdiff_Xanomeline High Dose_Placebo_M | rdiff_Xanomeline Low Dose_Placebo_F | rdiff_Xanomeline Low Dose_Placebo_M | row_label1         |
|:-------------------------------------|:-------------------------------------|:------------------------------------|:------------------------------------|:-------------------|
| 0.000 ( 0.000, 0.000)                | 0.036 (-0.069, 0.140)                | 0.000 ( 0.000, 0.000)               | 0.000 ( 0.000, 0.000)               | ACTINIC KERATOSIS  |
| -0.077 (-0.296, 0.142)               | 0.000 ( 0.000, 0.000)                | -0.077 (-0.281, 0.127)              | 0.000 ( 0.000, 0.000)               | ALOPECIA           |
| 0.000 ( 0.000, 0.000)                | 0.036 (-0.069, 0.140)                | 0.083 (-0.087, 0.253)               | 0.167 (-0.096, 0.429)               | BLISTER            |
| 0.000 ( 0.000, 0.000)                | -0.125 (-0.435, 0.185)               | 0.000 ( 0.000, 0.000)               | -0.125 (-0.444, 0.194)              | COLD SWEAT         |
| 0.000 ( 0.000, 0.000)                | -0.125 (-0.435, 0.185)               | 0.000 ( 0.000, 0.000)               | -0.125 (-0.444, 0.194)              | DERMATITIS ATOPIC  |
| 0.000 ( 0.000, 0.000)                | 0.000 ( 0.000, 0.000)                | 0.000 ( 0.000, 0.000)               | 0.056 (-0.106, 0.217)               | DERMATITIS CONTACT |

## Getting Raw Numbers

Just like you can get the numeric data from a **Tplyr** layer with
[`get_numeric_data()`](https://atorus-research.github.io/Tplyr/reference/get_numeric_data.md),
we’ve also opened up the door to extract the raw numeric data from risk
difference calculations as well. This is done using the function
[`get_stats_data()`](https://atorus-research.github.io/Tplyr/reference/get_stats_data.md).
The function interface is almost identical to
[`get_numeric_data()`](https://atorus-research.github.io/Tplyr/reference/get_numeric_data.md),
except for the extra parameter of `statistic`. Although risk difference
is the only statistic implemented in **Tplyr** at the moment (outside of
descriptive statistics), we understand that there are multiple methods
to calculate risk difference, so we’ve built risk difference in a way
that it could be expanded to easily add new methods in the future. And
therefore,
[`get_stats_data()`](https://atorus-research.github.io/Tplyr/reference/get_stats_data.md)
the `statistic` parameter to allow you to differentiate in the situation
where there are multiple statistical methods applied to the layer.

The output of
[`get_stats_data()`](https://atorus-research.github.io/Tplyr/reference/get_stats_data.md)
depends on what parameters have been used:

- If no specific layer has been entered in the `layer` parameter, then
  an element will be returned for each layer
- If no statistic has been entered in the `statistic` parameter, an
  element will be returned for each statistic for each layer
- If neither statistic nor layer are entered, a list of lists is
  returned, where the outer list is each layer and the inside list is
  the numeric statistic data for that layer.

This works best when layers are named, as it makes the output much
clearer.

``` r
t <- tplyr_table(tplyr_adae, TRTA) %>% 
  add_layer(name="PreferredTerm",
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID) %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo')
      ) 
  ) %>% 
  add_layer(name="BodySystem",
    group_count(AEBODSYS) %>% 
      set_distinct_by(USUBJID) %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo')
      ) 
  )

suppressWarnings(
  get_stats_data(t)
  )
#> $PreferredTerm
#> $PreferredTerm$riskdiff
#> # A tibble: 105 × 4
#>    summary_var       measure Xanomeline High Dose_Place…¹ Xanomeline Low Dose_…²
#>    <chr>             <chr>                          <dbl>                  <dbl>
#>  1 ACTINIC KERATOSIS comp                          0.0238                 0     
#>  2 ACTINIC KERATOSIS ref                           0                      0     
#>  3 ACTINIC KERATOSIS dif                           0.0238                 0     
#>  4 ACTINIC KERATOSIS low                          -0.0461                 0     
#>  5 ACTINIC KERATOSIS high                          0.0937                 0     
#>  6 ALOPECIA          comp                          0                      0     
#>  7 ALOPECIA          ref                           0.0476                 0.0476
#>  8 ALOPECIA          dif                          -0.0476                -0.0476
#>  9 ALOPECIA          low                          -0.174                 -0.174 
#> 10 ALOPECIA          high                          0.0792                 0.0792
#> # ℹ 95 more rows
#> # ℹ abbreviated names: ¹​`Xanomeline High Dose_Placebo`,
#> #   ²​`Xanomeline Low Dose_Placebo`
#> 
#> 
#> $BodySystem
#> $BodySystem$riskdiff
#> # A tibble: 5 × 4
#>   summary_var              measure Xanomeline High Dose…¹ Xanomeline Low Dose_…²
#>   <chr>                    <chr>                    <dbl>                  <dbl>
#> 1 SKIN AND SUBCUTANEOUS T… comp                         1                      1
#> 2 SKIN AND SUBCUTANEOUS T… ref                          1                      1
#> 3 SKIN AND SUBCUTANEOUS T… dif                          0                      0
#> 4 SKIN AND SUBCUTANEOUS T… low                          0                      0
#> 5 SKIN AND SUBCUTANEOUS T… high                         0                      0
#> # ℹ abbreviated names: ¹​`Xanomeline High Dose_Placebo`,
#> #   ²​`Xanomeline Low Dose_Placebo`
```

Instead of playing around with lists,
[`get_stats_data()`](https://atorus-research.github.io/Tplyr/reference/get_stats_data.md)
is most advantageous if you’d like to extract out some data
specifically. Let’s say that you’d like to see just the difference
values from the Preferred Term layer in the table above.

``` r

suppressWarnings(
  get_stats_data(t, layer='PreferredTerm', statistic='riskdiff', where= measure == "dif")
  ) %>% 
  head() %>% 
  kable()
```

| summary_var        | measure | Xanomeline High Dose_Placebo | Xanomeline Low Dose_Placebo |
|:-------------------|:--------|-----------------------------:|----------------------------:|
| ACTINIC KERATOSIS  | dif     |                    0.0238095 |                   0.0000000 |
| ALOPECIA           | dif     |                   -0.0476190 |                  -0.0476190 |
| BLISTER            | dif     |                    0.0238095 |                   0.1190476 |
| COLD SWEAT         | dif     |                   -0.0476190 |                  -0.0476190 |
| DERMATITIS ATOPIC  | dif     |                   -0.0476190 |                  -0.0476190 |
| DERMATITIS CONTACT | dif     |                    0.0000000 |                   0.0238095 |

Using this data frame, you have access to the un-formatted numeric
values before any rounding or formatting. This gives you flexibility to
use these calculations in other contexts, make more precise comparisons
in a double programming scenario, or take a deeper look into the
calculations that were made if any values in the result warrant further
investigation.
