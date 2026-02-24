# Advanced Descriptive Statistic Layer Formatting

A lot of the nuance to formatting descriptive statistics layers was
covered in the descriptive statistic layer vignette, but there are a
couple more tricks to getting the most out of **Tplyr**. In this
vignette, we’ll cover some of the options in more detail.

## Empty Value Formatting

By default, if there is no available value for a summary in a particular
observation, the result being presented will be blanked out.

*Note: **Tplyr** generally respects factor levels - so in instances of a
missing row or column group, if the factor level is present, then the
variable or row will still generate)*

``` r
tplyr_adsl$TRT01P <- as.factor(tplyr_adsl$TRT01P)
tplyr_adlb$TRTA <- as.factor(tplyr_adlb$TRTA)

tplyr_adlb_2 <- tplyr_adlb %>% 
  filter(TRTA != "Placebo")

tplyr_table(tplyr_adlb_2, TRTA) %>% 
  set_pop_data(tplyr_adsl) %>% 
  set_pop_treat_var(TRT01P) %>% 
  add_layer(
    group_desc(AVAL, by=PARAMCD) %>% 
      set_format_strings('Mean (SD)' = f_str('xxx (xxx)', mean, sd))
  ) %>% 
  build() %>% 
  head() %>% 
  select(-starts_with("ord")) %>% 
  kable()
```

| row_label1 | row_label2 | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose |
|:-----------|:-----------|:-------------|:--------------------------|:-------------------------|
| BUN        | Mean (SD)  |              | 5 ( 1)                    | 7 ( 3)                   |
| CA         | Mean (SD)  |              | 2 ( 0)                    | 2 ( 0)                   |
| CK         | Mean (SD)  |              | 108 ( 94)                 | 83 ( 78)                 |
| GGT        | Mean (SD)  |              | 36 ( 49)                  | 35 ( 27)                 |
| URATE      | Mean (SD)  |              | 289 ( 88)                 | 253 ( 87)                |

Note how the entire example above has all records in `var1_Placebo`
missing. **Tplyr** gives you control over how you fill this space. Let’s
say that we wanted instead to make that space say “Missing”. You can
control this with the
[`f_str()`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
object using the `empty` parameter.

``` r
tplyr_table(tplyr_adlb_2, TRTA) %>% 
  set_pop_data(tplyr_adsl) %>% 
  set_pop_treat_var(TRT01P) %>% 
  add_layer(
    group_desc(AVAL, by=PARAMCD) %>% 
      set_format_strings('Mean (SD)' = f_str('xxx.xx (xxx.xxx)', mean, sd, empty=c(.overall="MISSING")))
  ) %>% 
  build() %>% 
  head() %>% 
  select(-starts_with("ord")) %>% 
  kable()
```

| row_label1 | row_label2 | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose |
|:-----------|:-----------|:-------------|:--------------------------|:-------------------------|
| BUN        | Mean (SD)  | MISSING      | 4.61 ( 1.301)             | 6.73 ( 2.940)            |
| CA         | Mean (SD)  | MISSING      | 2.20 ( 0.137)             | 2.16 ( 0.083)            |
| CK         | Mean (SD)  | MISSING      | 108.25 ( 93.986)          | 83.13 ( 77.915)          |
| GGT        | Mean (SD)  | MISSING      | 36.05 ( 48.692)           | 34.92 ( 26.989)          |
| URATE      | Mean (SD)  | MISSING      | 289.26 ( 88.161)          | 253.06 ( 87.006)         |

Look at the `empty` parameter above. Here, we use a named character
vector, where the name is `.overall`. When this name is used, if all
elements within the cell are missing, they will be filled with the
specified text. Otherwise, the provided string will fill just the
missing parameter. In some cases, this may not be what you’d like to
see. Perhaps we want a string that fills each missing space.

``` r
tplyr_table(tplyr_adlb_2, TRTA) %>% 
  set_pop_data(tplyr_adsl) %>% 
  set_pop_treat_var(TRT01P) %>% 
  add_layer(
    group_desc(AVAL, by=PARAMCD) %>% 
      set_format_strings('Mean (SD)' = f_str('xxx.xx (xxx.xxx)', mean, sd, empty=c("NA")))
  ) %>% 
  build() %>% 
  head() %>%
  select(-starts_with("ord")) %>%
  kable()
```

| row_label1 | row_label2 | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose |
|:-----------|:-----------|:-------------|:--------------------------|:-------------------------|
| BUN        | Mean (SD)  |              | 4.61 ( 1.301)             | 6.73 ( 2.940)            |
| CA         | Mean (SD)  |              | 2.20 ( 0.137)             | 2.16 ( 0.083)            |
| CK         | Mean (SD)  |              | 108.25 ( 93.986)          | 83.13 ( 77.915)          |
| GGT        | Mean (SD)  |              | 36.05 ( 48.692)           | 34.92 ( 26.989)          |
| URATE      | Mean (SD)  |              | 289.26 ( 88.161)          | 253.06 ( 87.006)         |

In the example above, instead of filling the whole space, the `empty`
text of “NA” replaces the empty value for each element. So for ‘Mean
(SD)’, we now have ‘NA ( NA)’. Note that the proper padding was still
used for ‘NA’ to make sure the parentheses still align with populated
records.

## Auto Precision

You may have noticed that the approach to formatting covered so far
leaves a lot to be desired. Consider analyzing lab results, where you
may want precision to vary based on the collected precision of the
tests. Furthermore, depending on the summary being presented, you may
wish to increase the precision further. For example, you may want the
mean to be at collected precision +1 decimal place, and for standard
deviation +2.

**Tplyr** has this covered using auto-precision. Auto-precision allows
you to format your numeric summaries based on the precision of the data
collected. This has all been built into the format strings, because a
natural place to specify your desired format is where you specify how
you want your data presented. If you wish to use auto-precision, use `a`
instead of `x` when creating your summaries. Note that only one `a` is
needed on each side of a decimal. To use increased precision, use `a+n`
where `n` is the number of additional spaces you wish to add.

``` r
tplyr_table(tplyr_adlb, TRTA) %>% 
  add_layer(
    group_desc(AVAL, by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd)
      )
  ) %>% 
  build() %>% 
  head(20) %>% 
  select(-starts_with("ord")) %>% 
  kable()
```

| row_label1 | row_label2 | var1_Placebo         | var1_Xanomeline High Dose | var1_Xanomeline Low Dose |
|:-----------|:-----------|:---------------------|:--------------------------|:-------------------------|
| BUN        | Mean (SD)  | 5.3058 ( 2.05463)    | 4.6070 ( 1.30148)         | 6.7320 ( 2.94018)        |
| CA         | Mean (SD)  | 2.180413 (0.0692494) | 2.204332 (0.1372011)      | 2.161054 (0.0830867)     |
| CK         | Mean (SD)  | 175.8 ( 288.41)      | 108.2 ( 93.99)            | 83.1 ( 77.91)            |
| GGT        | Mean (SD)  | 34.5 ( 34.77)        | 36.0 ( 48.69)             | 34.9 ( 26.99)            |
| URATE      | Mean (SD)  | 260.6499 ( 83.69662) | 289.2606 ( 88.16093)      | 253.0604 ( 87.00646)     |

As you can see, the decimal precision is now varying depending on the
test being performed. Notice that both the integer and the decimal side
of each number fluctuate as well. **Tplyr** collects both the integer
and decimal precision, and you can specify both separately. For example,
you could use `x`’s to specify a default number of spaces for your
integers that are used consistently across by variables, but vary the
decimal precision based on collected data. You can also increment the
number of spaces for both integer and decimal separately.

But - this is kind of ugly, isn’t it? Do we really need all 6 decimal
places collected for CA? For this reason, you’re able to set a cap on
the precision that’s displayed:

``` r
tplyr_table(tplyr_adlb, TRTA) %>% 
  add_layer(
    group_desc(AVAL, by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd),
        cap = c(int=3, dec=2)
      )
  ) %>% 
  build() %>% 
  head(20) %>% 
  select(-starts_with("ord")) %>%
  kable()
```

| row_label1 | row_label2 | var1_Placebo       | var1_Xanomeline High Dose | var1_Xanomeline Low Dose |
|:-----------|:-----------|:-------------------|:--------------------------|:-------------------------|
| BUN        | Mean (SD)  | 5.306 ( 2.0546)    | 4.607 ( 1.3015)           | 6.732 ( 2.9402)          |
| CA         | Mean (SD)  | 2.180 (0.0692)     | 2.204 (0.1372)            | 2.161 (0.0831)           |
| CK         | Mean (SD)  | 175.8 (288.41)     | 108.2 ( 93.99)            | 83.1 ( 77.91)            |
| GGT        | Mean (SD)  | 34.5 ( 34.77)      | 36.0 ( 48.69)             | 34.9 ( 26.99)            |
| URATE      | Mean (SD)  | 260.650 ( 83.6966) | 289.261 ( 88.1609)        | 253.060 ( 87.0065)       |

Now that looks better. The `cap` argument is part of
[`set_format_strings()`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md).
You need to specify the integer and decimal caps separately. Note that
integer precision works slightly differently than decimal precision.
Integer precision relates to the length allotted for the left side of a
decimal, but integers will not truncate. When using ‘x’ formatting, if
an integer exceeds the set length, it will push the number over. If the
integer side of auto-precision is not capped, the necessary length for
an integer in the associated by group will be as long as necessary.
Decimals, on the other hand, round to the specified length. These caps
apply to the length allotted for the “a” on either the integer or the
decimal. So for example, if the decimal length is capped at 2 and the
selected precision is “a+1”, then 3 decimal places will be allotted.

This was a basic situation, but if you’re paying close attention, you
may have some questions. What if you have more by variables, like by
visit AND test. Do we then calculate precision by visit and test? What
if collected precision is different per visit and we don’t want that?
What about multiple summary variables? How do we determine precision
then? We have modifier functions for this:

``` r
tplyr_table(tplyr_adlb, TRTA) %>% 
  add_layer(
    group_desc(vars(AVAL, CHG, BASE), by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd, empty="NA"),
        cap = c(int=3, dec=2)
      ) %>% 
      set_precision_on(AVAL) %>% 
      set_precision_by(PARAMCD)
  ) %>%
  build() %>% 
  head() %>% 
  select(-starts_with("ord")) %>%
  kable()
```

| row_label1 | row_label2 | var1_Placebo       | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | var2_Placebo       | var2_Xanomeline High Dose | var2_Xanomeline Low Dose | var3_Placebo       | var3_Xanomeline High Dose | var3_Xanomeline Low Dose |
|:-----------|:-----------|:-------------------|:--------------------------|:-------------------------|:-------------------|:--------------------------|:-------------------------|:-------------------|:--------------------------|:-------------------------|
| BUN        | Mean (SD)  | 5.306 ( 2.0546)    | 4.607 ( 1.3015)           | 6.732 ( 2.9402)          | -0.542 ( 1.6734)   | -0.544 ( 1.5910)          | -0.395 ( 1.3299)         | 5.847 ( 1.5661)    | 5.151 ( 1.6382)           | 7.196 ( 2.7940)          |
| CA         | Mean (SD)  | 2.180 (0.0692)     | 2.204 (0.1372)            | 2.161 (0.0831)           | -0.111 (0.0972)    | -0.086 (0.1242)           | -0.155 (0.0621)          | 2.291 (0.0923)     | 2.290 (0.0828)            | 2.317 (0.0719)           |
| CK         | Mean (SD)  | 175.8 (288.41)     | 108.2 ( 93.99)            | 83.1 ( 77.91)            | 86.5 (275.75)      | 3.6 ( 77.56)              | 1.0 ( 51.54)             | 89.3 ( 38.13)      | 104.6 ( 64.27)            | 82.1 ( 35.71)            |
| GGT        | Mean (SD)  | 34.5 ( 34.77)      | 36.0 ( 48.69)             | 34.9 ( 26.99)            | 5.9 ( 21.67)       | 7.8 ( 41.48)              | 10.3 ( 20.55)            | 28.6 ( 19.36)      | 28.2 ( 25.84)             | 24.6 ( 11.05)            |
| URATE      | Mean (SD)  | 260.650 ( 83.6966) | 289.261 ( 88.1609)        | 253.060 ( 87.0065)       | -21.030 ( 37.1799) | -26.296 ( 55.6673)        | -41.906 ( 24.7959)       | 281.680 ( 65.7021) | 315.557 ( 61.8285)        | 294.967 ( 86.9470)       |

Three variables are being summarized here - AVAL, CHG, and BASE. So
which should be used for precision?
[`set_precision_on()`](https://atorus-research.github.io/Tplyr/reference/precision_on.md)
allows you to specify this, where the `precision_on()` variable must be
one of the variables within `target_var`. Similarly,
[`set_precision_by()`](https://atorus-research.github.io/Tplyr/reference/precision_by.md)
changes the `by` variables used to determine collected precision. If no
`precision_on()` variable is specified, the first variable in
`target_var` is used. If no `precision_by` variables are specified, then
the default `by` variables are used.

## External Precision

Lastly, while dynamic precision might be what you’re looking for, you
may not want precision driven by the data. Perhaps there’s a company
standard that dictates what decimal precision should be used for each
separate lab test. Maybe even deeper down to the lab test and category.
New in **Tplyr** 1.0.0 we’ve added the ability to take decimal precision
from an external source.

The principal of external precision is exactly the same as
auto-precision. The only difference is that you - the user - provide the
precision table that **Tplyr** was automatically calculating in the
background. This is done using the new function
[`set_precision_data()`](https://atorus-research.github.io/Tplyr/reference/set_precision_data.md).
In the output below, Notice how the precision by PARAMCD varies
depending on what was specified in the data frame `prec_data`.

``` r
prec_data <- tibble::tribble(
  ~PARAMCD, ~max_int, ~max_dec,
  "BUN",   1, 0,
  "CA",    2, 4,
  "CK",    3, 1,
  "GGT",   3, 0,
  "URATE", 3, 1,
)
  
tplyr_table(tplyr_adlb, TRTA) %>% 
  add_layer(
    group_desc(AVAL, by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd, empty="NA")
      ) %>% 
      set_precision_on(AVAL) %>% 
      set_precision_by(PARAMCD) %>%
      set_precision_data(prec_data)
  ) %>%
  build() %>% 
  head() %>% 
  select(-starts_with("ord")) %>%
  kable()
```

| row_label1 | row_label2 | var1_Placebo        | var1_Xanomeline High Dose | var1_Xanomeline Low Dose |
|:-----------|:-----------|:--------------------|:--------------------------|:-------------------------|
| BUN        | Mean (SD)  | 5.3 (2.05)          | 4.6 (1.30)                | 6.7 (2.94)               |
| CA         | Mean (SD)  | 2.18041 ( 0.069249) | 2.20433 ( 0.137201)       | 2.16105 ( 0.083087)      |
| CK         | Mean (SD)  | 175.84 (288.405)    | 108.25 ( 93.986)          | 83.13 ( 77.915)          |
| GGT        | Mean (SD)  | 34.5 ( 34.77)       | 36.0 ( 48.69)             | 34.9 ( 26.99)            |
| URATE      | Mean (SD)  | 260.65 ( 83.697)    | 289.26 ( 88.161)          | 253.06 ( 87.006)         |

If one of your by variable groups are missing in the precision data,
**Tplyr** can default back to using auto-precision by using the option
`default=auto`.

``` r
prec_data <- tibble::tribble(
  ~PARAMCD, ~max_int, ~max_dec,
  "BUN", 1, 0,
  "CA",  2, 4,
  "CK",  3, 1,
  "GGT", 3, 0,
)
  
tplyr_table(tplyr_adlb, TRTA) %>% 
  add_layer(
    group_desc(AVAL, by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd, empty="NA")
      ) %>% 
      set_precision_on(AVAL) %>% 
      set_precision_by(PARAMCD) %>%
      set_precision_data(prec_data, default="auto")
  ) %>%
  build() %>% 
  head() %>% 
  select(-starts_with("ord")) %>%
  kable()
#> Unhandled precision cases were found - calculating precision based on source data
```

| row_label1 | row_label2 | var1_Placebo         | var1_Xanomeline High Dose | var1_Xanomeline Low Dose |
|:-----------|:-----------|:---------------------|:--------------------------|:-------------------------|
| BUN        | Mean (SD)  | 5.3 (2.05)           | 4.6 (1.30)                | 6.7 (2.94)               |
| CA         | Mean (SD)  | 2.18041 ( 0.069249)  | 2.20433 ( 0.137201)       | 2.16105 ( 0.083087)      |
| CK         | Mean (SD)  | 175.84 (288.405)     | 108.25 ( 93.986)          | 83.13 ( 77.915)          |
| GGT        | Mean (SD)  | 34.5 ( 34.77)        | 36.0 ( 48.69)             | 34.9 ( 26.99)            |
| URATE      | Mean (SD)  | 260.6499 ( 83.69662) | 289.2606 ( 88.16093)      | 253.0604 ( 87.00646)     |

## Parenthesis Hugging

By default, when using ‘x’ or ‘a’, any other character within a format
string will stay stationary. Consider the standard example from the
descriptive statistic layer vignette.

``` r
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_desc(AGE, by = "Age (years)", where= SAFFL=="Y") %>% 
      set_format_strings(
        "n"        = f_str("xx", n),
        "Mean (SD)"= f_str("xx.x (xx.xx)", mean, sd),
        "Median"   = f_str("xx.x", median),
        "Q1, Q3"   = f_str("xx, xx", q1, q3),
        "Min, Max" = f_str("xx, xx", min, max),
        "Missing"  = f_str("xx", missing)
      )
  ) %>% 
  build() %>% 
  select(-starts_with('ord'))
#> # A tibble: 6 × 5
#>   row_label1  row_label2 var1_Placebo   `var1_Xanomeline High Dose`
#>   <chr>       <chr>      <chr>          <chr>                      
#> 1 Age (years) n          "86"           "84"                       
#> 2 Age (years) Mean (SD)  "75.2 ( 8.59)" "74.4 ( 7.89)"             
#> 3 Age (years) Median     "76.0"         "76.0"                     
#> 4 Age (years) Q1, Q3     "69, 82"       "71, 80"                   
#> 5 Age (years) Min, Max   "52, 89"       "56, 88"                   
#> 6 Age (years) Missing    " 0"           " 0"                       
#> # ℹ 1 more variable: `var1_Xanomeline Low Dose` <chr>
```

Note that if a certain number of integers are alotted, space will be
left for the numbers that fill that space, but the position of the
parenthesis stays fixed. In some displays, you may want the parenthesis
to ‘hug’ your number - the “format group” width should stay fixed, the
parenthesis should move to the right along with the numbers consuming
less integer space. Within your
[`f_str()`](https://atorus-research.github.io/Tplyr/reference/f_str.md),
you can achieve this by using a capital ‘X’. For example, focusing on
the mean and standard deviation line:

``` r
tplyr_table(tplyr_adlb, TRTA, PARAMCD == "CK") %>% 
  add_layer(
    group_desc(AVAL, by=vars(PARAMCD, AVISIT)) %>% 
      set_format_strings(
        TEST = f_str("xxx.x (XXX.x)", mean, sd, empty="NA")
      ) %>% 
      set_precision_by(PARAMCD)
  ) %>% 
  build() %>% 
  head() %>% 
  select(-starts_with('ord'))
#> # A tibble: 3 × 6
#>   row_label1 row_label2 row_label3 var1_Placebo  `var1_Xanomeline High Dose`
#>   <chr>      <chr>      <chr>      <chr>         <chr>                      
#> 1 CK         Week 12    TEST       140.0 (148.6) "140.1 (115.5)"            
#> 2 CK         Week 24    TEST       246.6 (438.5) " 55.5   (3.5)"            
#> 3 CK         Week 8     TEST       116.0  (78.9) " 93.3  (80.6)"            
#> # ℹ 1 more variable: `var1_Xanomeline Low Dose` <chr>
```

Similarly, the same functionality works with auto precision by using a
capital A.

``` r
tplyr_table(tplyr_adlb, TRTA, PARAMCD == "CK") %>% 
  add_layer(
    group_desc(AVAL, by=vars(PARAMCD, AVISIT)) %>% 
      set_format_strings(
        TEST = f_str("a.a (A.a)", mean, sd, empty="NA")
      ) %>% 
      set_precision_by(PARAMCD)
  ) %>% 
  build() %>% 
  head() %>% 
  select(-starts_with('ord'))
#> # A tibble: 3 × 6
#>   row_label1 row_label2 row_label3 var1_Placebo `var1_Xanomeline High Dose`
#>   <chr>      <chr>      <chr>      <chr>        <chr>                      
#> 1 CK         Week 12    TEST       " 140 (149)" " 140 (115)"               
#> 2 CK         Week 24    TEST       " 247 (438)" "  56   (4)"               
#> 3 CK         Week 8     TEST       " 116  (79)" "  93  (81)"               
#> # ℹ 1 more variable: `var1_Xanomeline Low Dose` <chr>
```

There are a two rules when using ‘parenthesis hugging’:

- Capital letters should only be used on the integer side of a number
- A character must precede the capital letter, otherwise there’s no
  character to ‘hug’

Aside from these rules, parenthesis hugging can be combined with all
other valid format string capabilities.
