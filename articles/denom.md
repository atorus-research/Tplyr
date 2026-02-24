# Totals, Missings, and Denominators

Counting is pretty easy, right? There’s not all that much to it. With a
few considerations we can cover most of the scenarios that users will
encounter while using Tplyr. Denominators, on the other hand, get **a
lot** more complicated. Why? Because there are a lot of ways to do it.
What values do we exclude from the denominator? What variables establish
denominator grouping? Does the denominator use a different filter than
the values being counted? If you’ve programmed enough of these tables,
you know that it’s all very situational.

Given the complexity, we’ve spent a lot of time trying to make this easy
for you, the user. In doing this it became very clear that denominators
needed their own vignette. Additionally, a few other things go hand in
hand with denominators.

Make sure you have a good understand of count and shift layers before
you review this content. If you’ve done your due diligence, then - here
we go.

## Population Data in the Denominator

What do you do when your target dataset doesn’t *have* the information
necessary to create your denominator? For example, when you create an
adverse event table, the adverse event dataset likely only contains
records for subjects who experienced an adverse event. But subjects who
did *not* have an adverse event are still part of the study population
and must be considered in the denominator.

For this reason,**Tplyr** allows lets you set a separate population
dataset - but there are a couple things you need to do to trigger
**Tplyr** to use the population data as your denominator.

Consider these two examples.

``` r
tplyr_table(tplyr_adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID) %>% 
      set_format_strings(f_str('xx (xx.x%)', distinct_n, distinct_pct))
  ) %>% 
  build() %>% 
  head() %>% 
  kable()
```

| row_label1         | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:-------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|
| ACTINIC KERATOSIS  | 0 ( 0.0%)    | 1 ( 2.4%)                 | 0 ( 0.0%)                |               1 |           1 |
| ALOPECIA           | 1 ( 4.8%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           2 |
| BLISTER            | 0 ( 0.0%)    | 1 ( 2.4%)                 | 5 (11.9%)                |               1 |           3 |
| COLD SWEAT         | 1 ( 4.8%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           4 |
| DERMATITIS ATOPIC  | 1 ( 4.8%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           5 |
| DERMATITIS CONTACT | 0 ( 0.0%)    | 0 ( 0.0%)                 | 1 ( 2.4%)                |               1 |           6 |

``` r
tplyr_table(tplyr_adae, TRTA) %>% 
  set_pop_data(tplyr_adsl) %>%
  set_pop_treat_var(TRT01A) %>%
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID) %>% 
      set_format_strings(f_str('xx (xx.x%)', distinct_n, distinct_pct))
  ) %>% 
  build() %>% 
  head() %>% 
  kable()
```

| row_label1         | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:-------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|
| ACTINIC KERATOSIS  | 0 ( 0.0%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               1 |           1 |
| ALOPECIA           | 1 ( 1.2%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           2 |
| BLISTER            | 0 ( 0.0%)    | 1 ( 1.2%)                 | 5 ( 6.0%)                |               1 |           3 |
| COLD SWEAT         | 1 ( 1.2%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           4 |
| DERMATITIS ATOPIC  | 1 ( 1.2%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           5 |
| DERMATITIS CONTACT | 0 ( 0.0%)    | 0 ( 0.0%)                 | 1 ( 1.2%)                |               1 |           6 |

There are three things done above that use the population data:

- Population data must be set in the first place using
  [`set_pop_where()`](https://atorus-research.github.io/Tplyr/reference/where.md)
  and the population treatment variable must be specified using
  [`set_pop_treat_var()`](https://atorus-research.github.io/Tplyr/reference/pop_treat_var.md)
- [`set_distinct_by()`](https://atorus-research.github.io/Tplyr/reference/set_distinct_by.md)
  must be used. Why? Because when you need to use a separate population
  dataset, the target dataset is likely more than one record per
  subject. So
  [`set_distinct_by()`](https://atorus-research.github.io/Tplyr/reference/set_distinct_by.md)
  in this scenario would be used to get distinct counts per subject. In
  this example, we’re looking at the number of unique subjects who
  experienced a specific adverse event.
- The population denominator is then used when calculating
  `distinct_pct`. It is also worth noting that the default count layer
  formats use distinct values.

Note that if you need more control over the values used in the
denominator from the population data, you can set a separate filter on
the population data used
[`set_pop_where()`](https://atorus-research.github.io/Tplyr/reference/where.md).

Fortunately, denominators are much simpler when they’re kept within a
single dataset. Just kidding! Let’s get weird.

## Denominator Grouping

When you’re looking within a single dataset, there are a couple factors
that you need to consider for a denominator. Firstly, which grouping
variables create those denominators? Let’s look at this from two
perspectives: count layers and shift layers.

### Count layers

Most of the complexity of denominators comes from nuanced situations.
Tplyr is designed with practical defaults that suit most clinical
summaries. For example, in a frequency table, you will typically want
data within a column to sum to 100%, like so:

``` r
tplyr_adsl <- tplyr_adsl %>% 
  mutate(DCSREAS = ifelse(DCSREAS == '', 'Completed', DCSREAS))
         
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(DCSREAS)
  ) %>% 
  build() %>% 
  kable()
```

| row_label1         | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:-------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|
| Adverse Event      | 8 ( 9.3%)    | 40 ( 47.6%)               | 44 ( 52.4%)              |               1 |           1 |
| Completed          | 58 ( 67.4%)  | 27 ( 32.1%)               | 25 ( 29.8%)              |               1 |           2 |
| Death              | 2 ( 2.3%)    | 0 ( 0.0%)                 | 1 ( 1.2%)                |               1 |           3 |
| I/E Not Met        | 1 ( 1.2%)    | 2 ( 2.4%)                 | 0 ( 0.0%)                |               1 |           4 |
| Lack of Efficacy   | 3 ( 3.5%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               1 |           5 |
| Lost to Follow-up  | 1 ( 1.2%)    | 0 ( 0.0%)                 | 1 ( 1.2%)                |               1 |           6 |
| Physician Decision | 1 ( 1.2%)    | 2 ( 2.4%)                 | 0 ( 0.0%)                |               1 |           7 |
| Protocol Violation | 1 ( 1.2%)    | 1 ( 1.2%)                 | 1 ( 1.2%)                |               1 |           8 |
| Sponsor Decision   | 2 ( 2.3%)    | 3 ( 3.6%)                 | 2 ( 2.4%)                |               1 |           9 |
| Withdrew Consent   | 9 ( 10.5%)   | 8 ( 9.5%)                 | 10 ( 11.9%)              |               1 |          10 |

By default, when not using the population data strategy shown above, a
count layer assumes that you want columns to sum to 100%. But that’s not
always the case. Perhaps you’d like to break this summary down by sex
presented row-wise.

``` r
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(DCSREAS, by=SEX)
  ) %>% 
  build() %>% 
  kable()
```

| row_label1 | row_label2         | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:-----------|:-------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|------------:|
| F          | Adverse Event      | 6 ( 7.0%)    | 20 ( 23.8%)               | 26 ( 31.0%)              |               1 |           1 |           1 |
| F          | Completed          | 34 ( 39.5%)  | 13 ( 15.5%)               | 17 ( 20.2%)              |               1 |           1 |           2 |
| F          | Death              | 1 ( 1.2%)    | 0 ( 0.0%)                 | 1 ( 1.2%)                |               1 |           1 |           3 |
| F          | I/E Not Met        | 0 ( 0.0%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           1 |           4 |
| F          | Lack of Efficacy   | 2 ( 2.3%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               1 |           1 |           5 |
| F          | Lost to Follow-up  | 1 ( 1.2%)    | 0 ( 0.0%)                 | 1 ( 1.2%)                |               1 |           1 |           6 |
| F          | Physician Decision | 1 ( 1.2%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               1 |           1 |           7 |
| F          | Protocol Violation | 1 ( 1.2%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               1 |           1 |           8 |
| F          | Sponsor Decision   | 1 ( 1.2%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           1 |           9 |
| F          | Withdrew Consent   | 6 ( 7.0%)    | 4 ( 4.8%)                 | 5 ( 6.0%)                |               1 |           1 |          10 |
| M          | Adverse Event      | 2 ( 2.3%)    | 20 ( 23.8%)               | 18 ( 21.4%)              |               1 |           2 |           1 |
| M          | Completed          | 24 ( 27.9%)  | 14 ( 16.7%)               | 8 ( 9.5%)                |               1 |           2 |           2 |
| M          | Death              | 1 ( 1.2%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           2 |           3 |
| M          | I/E Not Met        | 1 ( 1.2%)    | 2 ( 2.4%)                 | 0 ( 0.0%)                |               1 |           2 |           4 |
| M          | Lack of Efficacy   | 1 ( 1.2%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           2 |           5 |
| M          | Lost to Follow-up  | 0 ( 0.0%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           2 |           6 |
| M          | Physician Decision | 0 ( 0.0%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               1 |           2 |           7 |
| M          | Protocol Violation | 0 ( 0.0%)    | 0 ( 0.0%)                 | 1 ( 1.2%)                |               1 |           2 |           8 |
| M          | Sponsor Decision   | 1 ( 1.2%)    | 3 ( 3.6%)                 | 2 ( 2.4%)                |               1 |           2 |           9 |
| M          | Withdrew Consent   | 3 ( 3.5%)    | 4 ( 4.8%)                 | 5 ( 6.0%)                |               1 |           2 |          10 |

Ok - so, now this is a little bit off. By breaking sex down as a row
group, the denominators are still the total treatment group. Does that
make sense? 34 female Placebo group subjects completed, but that
calculated 39.5% also includes male subjects in the denominator. Let’s
fix this using
[`set_denoms_by()`](https://atorus-research.github.io/Tplyr/reference/set_denoms_by.md).

``` r
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(DCSREAS, by=SEX) %>% 
      set_denoms_by(SEX, TRT01P)
  ) %>% 
  build() %>% 
  kable()
```

| row_label1 | row_label2         | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:-----------|:-------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|------------:|
| F          | Adverse Event      | 6 ( 11.3%)   | 20 ( 50.0%)               | 26 ( 52.0%)              |               1 |           1 |           1 |
| F          | Completed          | 34 ( 64.2%)  | 13 ( 32.5%)               | 17 ( 34.0%)              |               1 |           1 |           2 |
| F          | Death              | 1 ( 1.9%)    | 0 ( 0.0%)                 | 1 ( 2.0%)                |               1 |           1 |           3 |
| F          | I/E Not Met        | 0 ( 0.0%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           1 |           4 |
| F          | Lack of Efficacy   | 2 ( 3.8%)    | 1 ( 2.5%)                 | 0 ( 0.0%)                |               1 |           1 |           5 |
| F          | Lost to Follow-up  | 1 ( 1.9%)    | 0 ( 0.0%)                 | 1 ( 2.0%)                |               1 |           1 |           6 |
| F          | Physician Decision | 1 ( 1.9%)    | 1 ( 2.5%)                 | 0 ( 0.0%)                |               1 |           1 |           7 |
| F          | Protocol Violation | 1 ( 1.9%)    | 1 ( 2.5%)                 | 0 ( 0.0%)                |               1 |           1 |           8 |
| F          | Sponsor Decision   | 1 ( 1.9%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           1 |           9 |
| F          | Withdrew Consent   | 6 ( 11.3%)   | 4 ( 10.0%)                | 5 ( 10.0%)               |               1 |           1 |          10 |
| M          | Adverse Event      | 2 ( 6.1%)    | 20 ( 45.5%)               | 18 ( 52.9%)              |               1 |           2 |           1 |
| M          | Completed          | 24 ( 72.7%)  | 14 ( 31.8%)               | 8 ( 23.5%)               |               1 |           2 |           2 |
| M          | Death              | 1 ( 3.0%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           2 |           3 |
| M          | I/E Not Met        | 1 ( 3.0%)    | 2 ( 4.5%)                 | 0 ( 0.0%)                |               1 |           2 |           4 |
| M          | Lack of Efficacy   | 1 ( 3.0%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           2 |           5 |
| M          | Lost to Follow-up  | 0 ( 0.0%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           2 |           6 |
| M          | Physician Decision | 0 ( 0.0%)    | 1 ( 2.3%)                 | 0 ( 0.0%)                |               1 |           2 |           7 |
| M          | Protocol Violation | 0 ( 0.0%)    | 0 ( 0.0%)                 | 1 ( 2.9%)                |               1 |           2 |           8 |
| M          | Sponsor Decision   | 1 ( 3.0%)    | 3 ( 6.8%)                 | 2 ( 5.9%)                |               1 |           2 |           9 |
| M          | Withdrew Consent   | 3 ( 9.1%)    | 4 ( 9.1%)                 | 5 ( 14.7%)               |               1 |           2 |          10 |

Ok - much better.
[`set_denoms_by()`](https://atorus-research.github.io/Tplyr/reference/set_denoms_by.md)
now changed the denominator grouping for us, so the denominator used for
those 34 female Placebo subjects are now the total number of female
Placebo subjects. Makes sense, right?
[`set_denoms_by()`](https://atorus-research.github.io/Tplyr/reference/set_denoms_by.md)
allows you to specify:

- The treatment variable
- any `cols` variables specified at the table level
- any `by` variables specified at the layer level

Depending on your presentation, what you require may change - but the
flexibility is there to choose what you need.

### Shift layers

A major part of the shift API is the control of the denominators used in
the calculation of the percentages. In shift tables, most percentages
are relative to the “box” that is formed from the “from” and “to” groups
of the shift for each treatment group. Just like the count layers, the
[`set_denoms_by()`](https://atorus-research.github.io/Tplyr/reference/set_denoms_by.md)
functions any variable name from the treatment variable, `cols`
argument, `by` variables. The difference with shift layers is that now
you can also include your target variables used for the row or column.

``` r
tplyr_table(tplyr_adlb, TRTA, where=PARAMCD == "CK") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, AVISIT)) %>%
      set_format_strings(f_str("xx (xxx.x%)", n, pct)) %>%
      # This is the default, the 3x3 box formed by the target variables
      set_denoms_by(TRTA, PARAM, AVISIT) 
  ) %>%
  build() %>%
  kable()
```

| row_label1            | row_label2 | row_label3 | var1_Placebo_H | var1_Placebo_N | var1_Xanomeline High Dose_H | var1_Xanomeline High Dose_N | var1_Xanomeline Low Dose_H | var1_Xanomeline Low Dose_N | ord_layer_index | ord_layer_1 | ord_layer_2 | ord_layer_3 |
|:----------------------|:-----------|:-----------|:---------------|:---------------|:----------------------------|:----------------------------|:---------------------------|:---------------------------|----------------:|------------:|------------:|------------:|
| Creatine Kinase (U/L) | Week 12    | H          | 0 ( 0.0%)      | 0 ( 0.0%)      | 1 ( 12.5%)                  | 0 ( 0.0%)                   | 0 ( 0.0%)                  | 0 ( 0.0%)                  |               1 |          35 |          12 |           1 |
| Creatine Kinase (U/L) | Week 12    | N          | 1 ( 8.3%)      | 11 ( 91.7%)    | 1 ( 12.5%)                  | 6 ( 75.0%)                  | 1 ( 14.3%)                 | 6 ( 85.7%)                 |               1 |          35 |          12 |           3 |
| Creatine Kinase (U/L) | Week 24    | H          | 0 ( 0.0%)      | 0 ( 0.0%)      | 0 ( 0.0%)                   | 0 ( 0.0%)                   | 0 ( 0.0%)                  | 0 ( 0.0%)                  |               1 |          35 |          24 |           1 |
| Creatine Kinase (U/L) | Week 24    | N          | 2 ( 16.7%)     | 10 ( 83.3%)    | 0 ( 0.0%)                   | 2 (100.0%)                  | 0 ( 0.0%)                  | 2 (100.0%)                 |               1 |          35 |          24 |           3 |
| Creatine Kinase (U/L) | Week 8     | H          | 0 ( 0.0%)      | 0 ( 0.0%)      | 0 ( 0.0%)                   | 0 ( 0.0%)                   | 0 ( 0.0%)                  | 0 ( 0.0%)                  |               1 |          35 |           8 |           1 |
| Creatine Kinase (U/L) | Week 8     | N          | 1 ( 14.3%)     | 6 ( 85.7%)     | 1 ( 10.0%)                  | 9 ( 90.0%)                  | 0 ( 0.0%)                  | 6 (100.0%)                 |               1 |          35 |           8 |           3 |

In the example above, the denominators were based on the by and
treatment variables, `TRTA`, `PARAM` and `VISIT`. This creates a 3 X 3
box, where the denominator is the total of all record within the
**FROM** and **TO** shift variables, within each parameter, visit, and
treatment. This is the default, and this is how **Tplyr** will create
the denominators if
[`set_denoms_by()`](https://atorus-research.github.io/Tplyr/reference/set_denoms_by.md)
isn’t specified.

In the next example, the percentage denominators are calculated
row-wise, each row percentage sums to 100%.

``` r
tplyr_table(tplyr_adlb, TRTA, where=PARAMCD == "CK") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, AVISIT)) %>%
      set_format_strings(f_str("xx (xxx.x%)", n, pct)) %>%
      set_denoms_by(TRTA, PARAM, AVISIT, BNRIND) # Each row made by TRTA, BNRIND
  ) %>%
  build() %>%
  arrange(ord_layer_1, ord_layer_2, ord_layer_3) %>% 
  head() %>% 
  kable()
```

| row_label1            | row_label2 | row_label3 | var1_Placebo_H | var1_Placebo_N | var1_Xanomeline High Dose_H | var1_Xanomeline High Dose_N | var1_Xanomeline Low Dose_H | var1_Xanomeline Low Dose_N | ord_layer_index | ord_layer_1 | ord_layer_2 | ord_layer_3 |
|:----------------------|:-----------|:-----------|:---------------|:---------------|:----------------------------|:----------------------------|:---------------------------|:---------------------------|----------------:|------------:|------------:|------------:|
| Creatine Kinase (U/L) | Week 8     | H          | 0 ( 0.0%)      | 0 ( 0.0%)      | 0 ( 0.0%)                   | 0 ( 0.0%)                   | 0 ( 0.0%)                  | 0 ( 0.0%)                  |               1 |          35 |           8 |           1 |
| Creatine Kinase (U/L) | Week 8     | N          | 1 ( 14.3%)     | 6 ( 85.7%)     | 1 ( 10.0%)                  | 9 ( 90.0%)                  | 0 ( 0.0%)                  | 6 (100.0%)                 |               1 |          35 |           8 |           3 |
| Creatine Kinase (U/L) | Week 12    | H          | 0 ( 0.0%)      | 0 ( 0.0%)      | 1 (100.0%)                  | 0 ( 0.0%)                   | 0 ( 0.0%)                  | 0 ( 0.0%)                  |               1 |          35 |          12 |           1 |
| Creatine Kinase (U/L) | Week 12    | N          | 1 ( 8.3%)      | 11 ( 91.7%)    | 1 ( 14.3%)                  | 6 ( 85.7%)                  | 1 ( 14.3%)                 | 6 ( 85.7%)                 |               1 |          35 |          12 |           3 |
| Creatine Kinase (U/L) | Week 24    | H          | 0 ( 0.0%)      | 0 ( 0.0%)      | 0 ( 0.0%)                   | 0 ( 0.0%)                   | 0 ( 0.0%)                  | 0 ( 0.0%)                  |               1 |          35 |          24 |           1 |
| Creatine Kinase (U/L) | Week 24    | N          | 2 ( 16.7%)     | 10 ( 83.3%)    | 0 ( 0.0%)                   | 2 (100.0%)                  | 0 ( 0.0%)                  | 2 (100.0%)                 |               1 |          35 |          24 |           3 |

While not practical, in this last example the denominators are changed
to be based on the entire column instead of the 3 x 3 box. By passing
the column variables, `TRTA` and `ANRIND` the layer will use those
denominators when determining the percentages.

``` r
tplyr_table(tplyr_adlb, TRTA, where = PARAMCD == "CK") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, AVISIT)) %>%
      set_format_strings(f_str("xx (xx.xx%)", n, pct)) %>%
      set_denoms_by(TRTA, ANRIND) # Use the column total as the denominator
  ) %>%
  build() %>%
  arrange(ord_layer_1, ord_layer_2, ord_layer_3) %>% 
  head() %>%
  kable()
```

| row_label1            | row_label2 | row_label3 | var1_Placebo_H | var1_Placebo_N | var1_Xanomeline High Dose_H | var1_Xanomeline High Dose_N | var1_Xanomeline Low Dose_H | var1_Xanomeline Low Dose_N | ord_layer_index | ord_layer_1 | ord_layer_2 | ord_layer_3 |
|:----------------------|:-----------|:-----------|:---------------|:---------------|:----------------------------|:----------------------------|:---------------------------|:---------------------------|----------------:|------------:|------------:|------------:|
| Creatine Kinase (U/L) | Week 8     | H          | 0 ( 0.00%)     | 0 ( 0.00%)     | 0 ( 0.00%)                  | 0 ( 0.00%)                  | 0 ( 0.00%)                 | 0 ( 0.00%)                 |               1 |          35 |           8 |           1 |
| Creatine Kinase (U/L) | Week 8     | N          | 1 (25.00%)     | 6 (22.22%)     | 1 (33.33%)                  | 9 (52.94%)                  | 0 ( 0.00%)                 | 6 (42.86%)                 |               1 |          35 |           8 |           3 |
| Creatine Kinase (U/L) | Week 12    | H          | 0 ( 0.00%)     | 0 ( 0.00%)     | 1 (33.33%)                  | 0 ( 0.00%)                  | 0 ( 0.00%)                 | 0 ( 0.00%)                 |               1 |          35 |          12 |           1 |
| Creatine Kinase (U/L) | Week 12    | N          | 1 (25.00%)     | 11 (40.74%)    | 1 (33.33%)                  | 6 (35.29%)                  | 1 (100.00%)                | 6 (42.86%)                 |               1 |          35 |          12 |           3 |
| Creatine Kinase (U/L) | Week 24    | H          | 0 ( 0.00%)     | 0 ( 0.00%)     | 0 ( 0.00%)                  | 0 ( 0.00%)                  | 0 ( 0.00%)                 | 0 ( 0.00%)                 |               1 |          35 |          24 |           1 |
| Creatine Kinase (U/L) | Week 24    | N          | 2 (50.00%)     | 10 (37.04%)    | 0 ( 0.00%)                  | 2 (11.76%)                  | 0 ( 0.00%)                 | 2 (14.29%)                 |               1 |          35 |          24 |           3 |

Our hope is that this gives you the flexibility you need to structure
your denominator however required.

## Controlling the Denominator Filter

There are some circumstances that you’ll encounter where the filter used
for a denominator needs to be different than the filter used to count.
Disposition tables are an example of this, and we’ll use that example to
paint this picture.

**Tplyr**offers you the ability to specifically control the filter used
within the denominator. This is provided through the function
[`set_denom_where()`](https://atorus-research.github.io/Tplyr/reference/set_denom_where.md).
The default for
[`set_denom_where()`](https://atorus-research.github.io/Tplyr/reference/set_denom_where.md)
is the layer level `where` parameter, if one was supplied.
[`set_denom_where()`](https://atorus-research.github.io/Tplyr/reference/set_denom_where.md)
allows you to replace this layer level filter with a custom filter of
your choosing. This is done on top of any filtering specified in the
[`tplyr_table()`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)
where parameter - which means that the
[`set_denom_where()`](https://atorus-research.github.io/Tplyr/reference/set_denom_where.md)
filter is applied *in addition to* any table level filtering.

Yeah we know - there are a lot of different places that filtering can
happen…

So let’s take the example shown below. The first layer has no
layer-level filtering applied, so the table-level `where` is the only
filter applied. The second layer has a layer-level filter applied, so
the denominators will be based on that layer-level filter. Notice how in
this case, the percentages in the second layer add up to 100%. This is
because the denominator only includes values used in that layer.

The third layer has a layer-level filter applied, but additionally uses
[`set_denom_where()`](https://atorus-research.github.io/Tplyr/reference/set_denom_where.md).
The
[`set_denom_where()`](https://atorus-research.github.io/Tplyr/reference/set_denom_where.md)
in this example is actually *removing* the layer-level filter for the
denominators. This is because in R, when you filter using `TRUE`, the
filter returns all records. So by using `TRUE` in
[`set_denom_where()`](https://atorus-research.github.io/Tplyr/reference/set_denom_where.md),
the layer-level filter is effectively removed. This causes the
denominator to include all values available from the table and not just
those selected for that layer - so for this layer, the percentages will
*not add up to 100%*. This is important - this allows the percentages
from Layer 3 to sum to the total percentage of “DISCONTINUED” from Layer
1.

``` r
tplyr_adsl2 <- tplyr_adsl %>% 
  mutate(DISCONTEXT = if_else(DISCONFL == 'Y', 'DISCONTINUED', 'COMPLETED'))

t <- tplyr_table(tplyr_adsl2, TRT01P, where = SAFFL == 'Y') %>%
  add_layer(
    group_count(DISCONTEXT)
  ) %>%
  add_layer(
    group_count(DCSREAS, where = DISCONFL == 'Y')
  ) %>%
  add_layer(
    group_count(DCSREAS, where = DISCONFL == 'Y') %>% 
    set_denom_where(TRUE)
  ) %>%
  build() %>%
  arrange(ord_layer_index, ord_layer_1) 

t %>% 
  kable()
```

| row_label1         | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:-------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|
| COMPLETED          | 58 ( 67.4%)  | 27 ( 32.1%)               | 25 ( 29.8%)              |               1 |           1 |
| DISCONTINUED       | 28 ( 32.6%)  | 57 ( 67.9%)               | 59 ( 70.2%)              |               1 |           2 |
| Adverse Event      | 8 ( 28.6%)   | 40 ( 70.2%)               | 44 ( 74.6%)              |               2 |           1 |
| Death              | 2 ( 7.1%)    | 0 ( 0.0%)                 | 1 ( 1.7%)                |               2 |           3 |
| I/E Not Met        | 1 ( 3.6%)    | 2 ( 3.5%)                 | 0 ( 0.0%)                |               2 |           4 |
| Lack of Efficacy   | 3 ( 10.7%)   | 1 ( 1.8%)                 | 0 ( 0.0%)                |               2 |           5 |
| Lost to Follow-up  | 1 ( 3.6%)    | 0 ( 0.0%)                 | 1 ( 1.7%)                |               2 |           6 |
| Physician Decision | 1 ( 3.6%)    | 2 ( 3.5%)                 | 0 ( 0.0%)                |               2 |           7 |
| Protocol Violation | 1 ( 3.6%)    | 1 ( 1.8%)                 | 1 ( 1.7%)                |               2 |           8 |
| Sponsor Decision   | 2 ( 7.1%)    | 3 ( 5.3%)                 | 2 ( 3.4%)                |               2 |           9 |
| Withdrew Consent   | 9 ( 32.1%)   | 8 ( 14.0%)                | 10 ( 16.9%)              |               2 |          10 |
| Adverse Event      | 8 ( 9.3%)    | 40 ( 47.6%)               | 44 ( 52.4%)              |               3 |           1 |
| Death              | 2 ( 2.3%)    | 0 ( 0.0%)                 | 1 ( 1.2%)                |               3 |           3 |
| I/E Not Met        | 1 ( 1.2%)    | 2 ( 2.4%)                 | 0 ( 0.0%)                |               3 |           4 |
| Lack of Efficacy   | 3 ( 3.5%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               3 |           5 |
| Lost to Follow-up  | 1 ( 1.2%)    | 0 ( 0.0%)                 | 1 ( 1.2%)                |               3 |           6 |
| Physician Decision | 1 ( 1.2%)    | 2 ( 2.4%)                 | 0 ( 0.0%)                |               3 |           7 |
| Protocol Violation | 1 ( 1.2%)    | 1 ( 1.2%)                 | 1 ( 1.2%)                |               3 |           8 |
| Sponsor Decision   | 2 ( 2.3%)    | 3 ( 3.6%)                 | 2 ( 2.4%)                |               3 |           9 |
| Withdrew Consent   | 9 ( 10.5%)   | 8 ( 9.5%)                 | 10 ( 11.9%)              |               3 |          10 |

## Missing Counts

Missing counts are a tricky area for frequency tables, and they play
directly in with denominators as well. These values raise a number of
questions. For example, do you want to format the missing counts the
same way as the event counts? Do you want to present missing counts with
percentages? Do missing counts belong in the denominator?

The
[`set_missing_count()`](https://atorus-research.github.io/Tplyr/reference/set_missing_count.md)
function can take a new
[`f_str()`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
object to set the display of missing values. If not specified, the
associated count layer’s format will be used. Using the `...` parameter,
you are able to specify the row label desired for missing values and
values that you determine to be considered ‘missing’. For example, you
may have NA values in the target variable, and then values like “Not
Collected” that you also wish to consider “missing”.
[`set_missing_count()`](https://atorus-research.github.io/Tplyr/reference/set_missing_count.md)
allows you to group those together. Actually you’re able to establish as
many different “missing” groups as you want - even though that scenario
is fairly unlikely.

In the example below, 50 random values are removed and NA is specified
as the missing string. This leads us to another parameter:
`denom_ignore`. By default, Tplyr will include missing values within the
denominator, but you may wish to exclude them from the totals being
summarized. By setting `denom_ignore` to TRUE, your denominators will
ignore any groups of missing values that you’ve specified.

``` r
set.seed(1234)
tplyr_adae2 <- tplyr_adae
tplyr_adae2[sample(nrow(tplyr_adae2), 50), "AESEV"] <- NA

t <- tplyr_table(tplyr_adae2, TRTA) %>%
  add_layer(
    group_count(AESEV) %>%
      set_format_strings(f_str("xxx (xx.xx%)", n, pct)) %>%
      set_missing_count(f_str("xxx", n), sort_value=Inf, denom_ignore=TRUE, Missing = NA)
  ) %>%
  build() %>% 
  arrange(ord_layer_1)

t %>% 
  kable()
```

| row_label1 | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:-----------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|
| MILD       | 21 (55.26%)  | 69 (76.67%)               | 47 (47.96%)              |               1 |           1 |
| MODERATE   | 17 (44.74%)  | 20 (22.22%)               | 47 (47.96%)              |               1 |           2 |
| SEVERE     | 0 ( 0.00%)   | 1 ( 1.11%)                | 4 ( 4.08%)               |               1 |           3 |
| Missing    | 9            | 21                        | 20                       |               1 |         Inf |

We did one more other thing worth explaining in the example above - we
gave the missing count its own sort value. If you leave this field null,
it will simply be the maximum value in the order layer plus 1, to put
the Missing counts at the bottom during an ascending sort. But tables
can be sorted a lot of different ways, as you’ll see in the sort
vignette. So instead of trying to come up with novel ways for you to
control where the missing row goes, we decided to just let you specify
your own value.

## Missing Subjects

Missing counts and counting missing subjects work two different ways
within Tplyr. Missing counts, as described above, will examine the
records present in the data and collect any missing values. But for
these results to be counted, they need to first be provided within the
input data itself. On the other hand, missing subjects are calculated by
looking at the difference between the *potential* number of subjects
within the column (i.e. the combination of the treatment variables and
column variables) and the number of subjects *actually* present.
Consider this example:

``` r
  missing_subs <- tplyr_table(tplyr_adae, TRTA) %>%
    set_pop_data(tplyr_adsl) %>%
    set_pop_treat_var(TRT01A) %>%
    add_layer(
      group_count(vars(AEBODSYS, AEDECOD)) %>%
        set_nest_count(TRUE) %>% 
        set_distinct_by(USUBJID) %>%
        add_missing_subjects_row(f_str("xx (XX.x%)", distinct_n, distinct_pct), sort_value = Inf) %>% 
        set_missing_subjects_row_label("Missing Subjects")
    ) %>%
    build()

  tail(missing_subs) %>% 
    select(-starts_with('ord')) %>% 
    kable()
```

| row_label1          | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose |
|:--------------------|:-------------|:--------------------------|:-------------------------|
| SKIN EXFOLIATION    | 0 ( 0.0%)    | 0 ( 0.0%)                 | 1 ( 1.2%)                |
| SKIN IRRITATION     | 3 ( 3.5%)    | 5 ( 6.0%)                 | 6 ( 7.1%)                |
| SKIN ODOUR ABNORMAL | 0 ( 0.0%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |
| SKIN ULCER          | 1 ( 1.2%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |
| URTICARIA           | 0 ( 0.0%)    | 1 ( 1.2%)                 | 1 ( 1.2%)                |
| Missing Subjects    | 85 (98.8%)   | 83 (98.8%)                | 83 (98.8%)               |

In the example above, we produce a nested count layer. The function
[`add_missing_subjects_row()`](https://atorus-research.github.io/Tplyr/reference/add_missing_subjects_row.md)
triggers the addition of the new result row for which the missing
subjects are calculated. The row label applied for this can be
configured using
[`set_missing_subjects_row_label()`](https://atorus-research.github.io/Tplyr/reference/set_missing_subjects_row_label.md),
and the row label itself will default to ‘Missing’. Depending on your
sorting needs, a `sort_value` can be applied to whatever numeric value
you provide. You can also provide an
[`f_str()`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
to format the missing subjects row separately from the rest of the
layer.

Note that in nested count layers, missing subject rows will generate for
each independent group within the outer layer. Outer layers cannot have
missing subject rows calculated individually. This would best be done in
an independent layer itself, as the result would apply to the whole
input target dataset.

## Adding a ‘Total’ Row

In addition to missing counts, some summaries require the addition of a
‘Total’ row. **Tplyr** has the helper function
[`add_total_row()`](https://atorus-research.github.io/Tplyr/reference/add_total_row.md)
to ease this process for you. Like most other things within **Tplyr** -
particularly in this vignette - this too has a significant bit of nuance
to it.

Much of this functionality is similar to
[`set_missing_count()`](https://atorus-research.github.io/Tplyr/reference/set_missing_count.md).
You’re able to specify a different format for the total, but if not
specified, the associated count layer’s format will be used. You’re able
to set your own sort value to specify where you want the total row to
sit.

More nuance comes in two places:

- By default,
  [`add_total_row()`](https://atorus-research.github.io/Tplyr/reference/add_total_row.md)
  *will count missing values*, but you can exclude those values using
  the `count_missings` parameter. **Tplyr** will warn you when
  `set_count_missing()` has `denom_ignore` set to `TRUE`,
  [`add_total_row()`](https://atorus-research.github.io/Tplyr/reference/add_total_row.md)
  has `count_missings` set to `TRUE` and the format contains a
  percentage. Why? Because if the denominator is ignoring missing values
  but you’re still counting them in your total, the percentage shown can
  exceed 100%.
- [`add_total_row()`](https://atorus-research.github.io/Tplyr/reference/add_total_row.md)
  will throw a warning when a `by` variable is used, because it becomes
  ambiguous what total should be calculated. You can rectify this by
  using
  [`set_denoms_by()`](https://atorus-research.github.io/Tplyr/reference/set_denoms_by.md),
  which allows the user to control exactly which groups are used to form
  the denominator. This way the totals presented by
  [`add_total_row()`](https://atorus-research.github.io/Tplyr/reference/add_total_row.md)
  will align with denominators specified in `set_denom_by()` and
  generate total rows that match the grouping of your denominator
  values.

In the example below, we summarize age groups by sex. The denominators
are determined by treatment group and sex, and since we are not
excluding any values from the denominator, the total row ends up
matching the denominator that was used. The ‘Missing’ row tells us the
number of missing values, but because `count_missings` is set to `TRUE`,
the missing counts are included in the total row. This probably isn’t
how you would choose to display things, but here we’re trying to show
the flexibility built into **Tplyr**.

``` r
set.seed(1234)
tplyr_adsl2 <- tplyr_adsl
tplyr_adsl2[sample(nrow(tplyr_adsl2), 50), "AGEGR1"] <- NA

tplyr_table(tplyr_adsl2, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by=SEX) %>% 
      set_denoms_by(TRT01P, SEX) %>%  # This gives me a Total row each group
      add_total_row(f_str("xxx", n), count_missings=TRUE, sort_value=-Inf) %>% 
      set_total_row_label("All Age Groups") %>% 
      set_missing_count(f_str("xx (xx.x%)", n, pct), Missing = NA, sort_value=Inf)
  ) %>% 
  build() %>% 
  arrange(ord_layer_1, ord_layer_2) %>% 
  kable()
```

| row_label1 | row_label2     | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:-----------|:---------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|------------:|
| F          | All Age Groups | 53           | 40                        | 50                       |               1 |           1 |        -Inf |
| F          | \<65           | 9 ( 17.0%)   | 3 ( 7.5%)                 | 2 ( 4.0%)                |               1 |           1 |           1 |
| F          | \>80           | 21 ( 39.6%)  | 5 ( 12.5%)                | 14 ( 28.0%)              |               1 |           1 |           2 |
| F          | 65-80          | 15 ( 28.3%)  | 25 ( 62.5%)               | 21 ( 42.0%)              |               1 |           1 |           3 |
| F          | Missing        | 8 (15.1%)    | 7 (17.5%)                 | 13 (26.0%)               |               1 |           1 |         Inf |
| M          | All Age Groups | 33           | 44                        | 34                       |               1 |           2 |        -Inf |
| M          | \<65           | 4 ( 12.1%)   | 4 ( 9.1%)                 | 2 ( 5.9%)                |               1 |           2 |           1 |
| M          | \>80           | 8 ( 24.2%)   | 9 ( 20.5%)                | 7 ( 20.6%)               |               1 |           2 |           2 |
| M          | 65-80          | 17 ( 51.5%)  | 23 ( 52.3%)               | 15 ( 44.1%)              |               1 |           2 |           3 |
| M          | Missing        | 4 (12.1%)    | 8 (18.2%)                 | 10 (29.4%)               |               1 |           2 |         Inf |

The default text for the Total row is “Total”, but we provide
[`set_total_row_label()`](https://atorus-research.github.io/Tplyr/reference/set_total_row_label.md)
to allow you to customize the text used in your display.

Let’s look at a more practical version of the table above. If you
display missings, you probably want to exclude them from the total. Here
we do that using
[`set_missing_count()`](https://atorus-research.github.io/Tplyr/reference/set_missing_count.md).
So more commonly, you’ll see this:

``` r
tplyr_table(tplyr_adsl2, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by=SEX) %>% 
      set_denoms_by(TRT01P, SEX) %>%  # This gives me a Total row each group
      add_total_row(f_str("xxx", n), count_missings=FALSE, sort_value=-Inf) %>% 
      set_total_row_label("All Age Groups") %>% 
      set_missing_count(f_str("xxx", n), Missing = NA, sort_value=Inf, denom_ignore=TRUE)
  ) %>% 
  build() %>% 
  arrange(ord_layer_1, ord_layer_2) %>% 
  kable()
```

| row_label1 | row_label2     | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:-----------|:---------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|------------:|
| F          | All Age Groups | 45           | 33                        | 37                       |               1 |           1 |        -Inf |
| F          | \<65           | 9 ( 20.0%)   | 3 ( 9.1%)                 | 2 ( 5.4%)                |               1 |           1 |           1 |
| F          | \>80           | 21 ( 46.7%)  | 5 ( 15.2%)                | 14 ( 37.8%)              |               1 |           1 |           2 |
| F          | 65-80          | 15 ( 33.3%)  | 25 ( 75.8%)               | 21 ( 56.8%)              |               1 |           1 |           3 |
| F          | Missing        | 8            | 7                         | 13                       |               1 |           1 |         Inf |
| M          | All Age Groups | 29           | 36                        | 24                       |               1 |           2 |        -Inf |
| M          | \<65           | 4 ( 13.8%)   | 4 ( 11.1%)                | 2 ( 8.3%)                |               1 |           2 |           1 |
| M          | \>80           | 8 ( 27.6%)   | 9 ( 25.0%)                | 7 ( 29.2%)               |               1 |           2 |           2 |
| M          | 65-80          | 17 ( 58.6%)  | 23 ( 63.9%)               | 15 ( 62.5%)              |               1 |           2 |           3 |
| M          | Missing        | 4            | 8                         | 10                       |               1 |           2 |         Inf |

Now the table is more intuitive. We used
[`set_missing_count()`](https://atorus-research.github.io/Tplyr/reference/set_missing_count.md)
to update our denominators, so missings have been excluded. Now, the
total row intuitively matches the denominators used within each group,
and we can see how many missing records were excluded.

*You may have stumbled upon this portion of the vignette while searching
for how to create a total column. **Tplyr** allows you to do this as
well with the function
[`add_total_group()`](https://atorus-research.github.io/Tplyr/reference/treat_grps.md)
and read more in
[`vignette("table")`](https://atorus-research.github.io/Tplyr/articles/table.md).*

And that’s it for denominators! Happy counting!
