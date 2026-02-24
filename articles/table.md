# Tplyr Table Properties

Most of the work in creating a **Tplyr** table is at the layer level,
but there are a few overarching properties that are worth spending some
time discussing. One of the things that we wanted to make sure we did in
**Tplyr** is allow you to eliminate redundant code wherever possible.
Adding some processing to the
[`tplyr_table()`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)
level allows us to do that. Furthermore, some settings simply need to be
applied table wide.

## Table Parameters

The
[`tplyr_table()`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)
function has 4 parameters:

- **target**: The dataset upon which summaries will be performed
- **`treat_var`**: The variable containing treatment group assignments
- **`where`**: The overarching table subset criteria. Each layer will
  use this subset by default. The `where` parameter at the table level
  will be called **in addition to** the layer subset criteria.
- **`cols`**: Grouping variables used in addition to the `by` variables
  set at the layer level, but will be transposed into columns in
  addition to `treat_var`.

Let’s look at an example:

``` r
tplyr_table(tplyr_adsl, TRT01P, where= SAFFL =="Y", cols = SEX) %>% 
  add_layer(
    group_count(RACE, by = "Race")
  ) %>% 
  add_layer(
    group_desc(AGE, by = "Age (Years)")
  ) %>% 
  build() %>% 
  kable()
```

| row_label1  | row_label2                       | var1_Placebo_F | var1_Placebo_M | var1_Xanomeline High Dose_F | var1_Xanomeline High Dose_M | var1_Xanomeline Low Dose_F | var1_Xanomeline Low Dose_M | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:------------|:---------------------------------|:---------------|:---------------|:----------------------------|:----------------------------|:---------------------------|:---------------------------|----------------:|------------:|------------:|
| Race        | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)      | 0 ( 0.0%)      | 0 ( 0.0%)                   | 1 ( 2.3%)                   | 0 ( 0.0%)                  | 0 ( 0.0%)                  |               1 |           1 |           1 |
| Race        | BLACK OR AFRICAN AMERICAN        | 5 ( 9.4%)      | 3 ( 9.1%)      | 6 ( 15.0%)                  | 3 ( 6.8%)                   | 6 ( 12.0%)                 | 0 ( 0.0%)                  |               1 |           1 |           2 |
| Race        | WHITE                            | 48 ( 90.6%)    | 30 ( 90.9%)    | 34 ( 85.0%)                 | 40 ( 90.9%)                 | 44 ( 88.0%)                | 34 (100.0%)                |               1 |           1 |           3 |
| Age (Years) | n                                | 53             | 33             | 40                          | 44                          | 50                         | 34                         |               2 |           1 |           1 |
| Age (Years) | Mean (SD)                        | 76.4 ( 8.73)   | 73.4 ( 8.15)   | 74.7 ( 7.67)                | 74.1 ( 8.16)                | 75.7 ( 8.09)               | 75.6 ( 8.69)               |               2 |           1 |           2 |
| Age (Years) | Median                           | 78.0           | 74.0           | 76.0                        | 77.0                        | 77.5                       | 77.5                       |               2 |           1 |           3 |
| Age (Years) | Q1, Q3                           | 70.0, 84.0     | 69.0, 80.0     | 72.0, 79.0                  | 69.0, 80.2                  | 72.0, 81.0                 | 68.2, 82.0                 |               2 |           1 |           4 |
| Age (Years) | Min, Max                         | 59, 89         | 52, 85         | 56, 88                      | 56, 86                      | 54, 87                     | 51, 88                     |               2 |           1 |           5 |
| Age (Years) | Missing                          | 0              | 0              | 0                           | 0                           | 0                          | 0                          |               2 |           1 |           6 |

In the example above, the `where` parameter is passed forward into both
the `RACE` and `AGE` layers. Furthermore, note how the `cols` parameter
works. By default, the target variables from the layers are transposed
by the `treat_var` variables. The `cols` argument adds an additional
variable to transpose by, and the values of these variable are added as
a suffix to the variable name. You are able to use multiple `cols`
variables just like `by`, by using
[`dplyr::vars()`](https://dplyr.tidyverse.org/reference/vars.html). But
use with caution - as depending on the distinct variable values in the
dataset, this could get quite wide.

*Note: Treatment groups and additional column variables presented in the
final output are always taken from the **pre-filtered** population data.
This means that if a filter completed excludes a treatment group or
group within a column variable, columns will still be created for those
groups and will be empty/zero filled.*

``` r
tplyr_table(tplyr_adsl, TRT01P, where= SAFFL =="Y", cols = vars(SEX, RACE)) %>% 
  add_layer(
    group_desc(AGE, by = "Age (Years)")
  ) %>% 
  build() %>% 
  kable()
```

| row_label1  | row_label2 | var1_Placebo_F_AMERICAN INDIAN OR ALASKA NATIVE | var1_Placebo_F_BLACK OR AFRICAN AMERICAN | var1_Placebo_F_WHITE | var1_Placebo_M_AMERICAN INDIAN OR ALASKA NATIVE | var1_Placebo_M_BLACK OR AFRICAN AMERICAN | var1_Placebo_M_WHITE | var1_Xanomeline High Dose_F_AMERICAN INDIAN OR ALASKA NATIVE | var1_Xanomeline High Dose_F_BLACK OR AFRICAN AMERICAN | var1_Xanomeline High Dose_F_WHITE | var1_Xanomeline High Dose_M_AMERICAN INDIAN OR ALASKA NATIVE | var1_Xanomeline High Dose_M_BLACK OR AFRICAN AMERICAN | var1_Xanomeline High Dose_M_WHITE | var1_Xanomeline Low Dose_F_AMERICAN INDIAN OR ALASKA NATIVE | var1_Xanomeline Low Dose_F_BLACK OR AFRICAN AMERICAN | var1_Xanomeline Low Dose_F_WHITE | var1_Xanomeline Low Dose_M_AMERICAN INDIAN OR ALASKA NATIVE | var1_Xanomeline Low Dose_M_BLACK OR AFRICAN AMERICAN | var1_Xanomeline Low Dose_M_WHITE | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:------------|:-----------|:------------------------------------------------|:-----------------------------------------|:---------------------|:------------------------------------------------|:-----------------------------------------|:---------------------|:-------------------------------------------------------------|:------------------------------------------------------|:----------------------------------|:-------------------------------------------------------------|:------------------------------------------------------|:----------------------------------|:------------------------------------------------------------|:-----------------------------------------------------|:---------------------------------|:------------------------------------------------------------|:-----------------------------------------------------|:---------------------------------|----------------:|------------:|------------:|
| Age (Years) | n          |                                                 | 5                                        | 48                   |                                                 | 3                                        | 30                   |                                                              | 6                                                     | 34                                | 1                                                            | 3                                                     | 40                                |                                                             | 6                                                    | 44                               |                                                             |                                                      | 34                               |               1 |           1 |           1 |
| Age (Years) | Mean (SD)  |                                                 | 75.2 ( 7.79)                             | 76.5 ( 8.89)         |                                                 | 64.7 ( 6.81)                             | 74.2 ( 7.84)         |                                                              | 72.2 ( 6.08)                                          | 75.1 ( 7.91)                      | 61.0 ( )                                                     | 79.3 ( 2.52)                                          | 74.0 ( 8.16)                      |                                                             | 72.5 (11.78)                                         | 76.1 ( 7.54)                     |                                                             |                                                      | 75.6 ( 8.69)                     |               1 |           1 |           2 |
| Age (Years) | Median     |                                                 | 80.0                                     | 78.0                 |                                                 | 67.0                                     | 74.5                 |                                                              | 73.5                                                  | 76.0                              | 61.0                                                         | 79.0                                                  | 76.0                              |                                                             | 75.0                                                 | 78.0                             |                                                             |                                                      | 77.5                             |               1 |           1 |           3 |
| Age (Years) | Q1, Q3     |                                                 | 70.0, 81.0                               | 70.5, 84.0           |                                                 | 62.0, 68.5                               | 70.0, 80.8           |                                                              | 68.5, 76.2                                            | 72.0, 79.8                        | 61.0, 61.0                                                   | 78.0, 80.5                                            | 69.0, 80.2                        |                                                             | 63.5, 79.8                                           | 72.0, 81.0                       |                                                             |                                                      | 68.2, 82.0                       |               1 |           1 |           4 |
| Age (Years) | Min, Max   |                                                 | 64, 81                                   | 59, 89               |                                                 | 57, 70                                   | 52, 85               |                                                              | 63, 79                                                | 56, 88                            | 61, 61                                                       | 77, 82                                                | 56, 86                            |                                                             | 57, 87                                               | 54, 86                           |                                                             |                                                      | 51, 88                           |               1 |           1 |           5 |
| Age (Years) | Missing    |                                                 | 0                                        | 0                    |                                                 | 0                                        | 0                    |                                                              | 0                                                     | 0                                 | 0                                                            | 0                                                     | 0                                 |                                                             | 0                                                    | 0                                |                                                             |                                                      | 0                                |               1 |           1 |           6 |

## Additional Treatment Groups

Another important feature that works at the table level is the addition
of treatment groups. By adding additional treatment groups, you’re able
to do a number of things:

- Add a ‘treated’ group to your data so you can analyze ‘treated’
  vs. ‘placebo’ when you have multiple treated cohorts
- Add a ‘total’ group so summarize the overall study population

We’ve added the function
[`add_treat_grps()`](https://atorus-research.github.io/Tplyr/reference/treat_grps.md)
to do this work for you. With this function, you can create new
treatment groups by combining existing treatment groups from values
within `treat_var`. Additionally, to simplify the process we added an
abstraction of
[`add_treat_grps()`](https://atorus-research.github.io/Tplyr/reference/treat_grps.md)
named
[`add_total_group()`](https://atorus-research.github.io/Tplyr/reference/treat_grps.md)
to simplify the process of creating a “Total” group.

``` r
tplyr_table(tplyr_adsl, TRT01P) %>%
  add_treat_grps('Treated' = c("Xanomeline High Dose", "Xanomeline Low Dose")) %>% 
  add_total_group() %>% 
  add_layer(
    group_desc(AGE, by = "Age (Years)")
  ) %>% 
  build() %>% 
  kable()
```

| row_label1  | row_label2 | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | var1_Treated | var1_Total   | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:------------|:-----------|:-------------|:--------------------------|:-------------------------|:-------------|:-------------|----------------:|------------:|------------:|
| Age (Years) | n          | 86           | 84                        | 84                       | 168          | 254          |               1 |           1 |           1 |
| Age (Years) | Mean (SD)  | 75.2 ( 8.59) | 74.4 ( 7.89)              | 75.7 ( 8.29)             | 75.0 ( 8.09) | 75.1 ( 8.25) |               1 |           1 |           2 |
| Age (Years) | Median     | 76.0         | 76.0                      | 77.5                     | 77.0         | 77.0         |               1 |           1 |           3 |
| Age (Years) | Q1, Q3     | 69.2, 81.8   | 70.8, 80.0                | 71.0, 82.0               | 71.0, 81.0   | 70.0, 81.0   |               1 |           1 |           4 |
| Age (Years) | Min, Max   | 52, 89       | 56, 88                    | 51, 88                   | 51, 88       | 51, 89       |               1 |           1 |           5 |
| Age (Years) | Missing    | 0            | 0                         | 0                        | 0            | 0            |               1 |           1 |           6 |

Note how in the above example, there are two new columns added to the
data - `var1_Total` and `var1_Treated`. The summaries for the individual
cohorts are left unchanged.

## Population Data

A last and very important aspect of table level properties in **Tplyr**
is the addition of a population dataset. In CDISC standards, datasets
like `adae` only contain adverse events when they occur. This means that
if a subject did not experience an adverse event, or did not experience
an adverse event within the criteria that you’re subsetting for, they
don’t appear in the dataset. When you’re looking at the proportion of
subject who experienced an adverse event compared to the total number of
subjects in that cohort, `adae` itself leaves you no way to calculate
that total - as the subjects won’t exist in the data.

**Tplyr** allows you to provide a separate population dataset to
overcome this. Furthermore, you are also able to provide a separate
population dataset `where` parameter and a population treatment variable
named `pop_treat_var`, as variable names may differ between the
datasets.

``` r
t <- tplyr_table(tplyr_adae, TRTA, where = AEREL != "NONE") %>% 
  set_pop_data(tplyr_adsl) %>% 
  set_pop_treat_var(TRT01A) %>% 
  set_pop_where(TRUE) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID)
  )
  
t %>% 
  build() %>% 
  kable()
```

| row_label1           | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:---------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|
| ALOPECIA             | 1 ( 1.2%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           2 |
| BLISTER              | 0 ( 0.0%)    | 1 ( 1.2%)                 | 5 ( 6.0%)                |               1 |           3 |
| COLD SWEAT           | 1 ( 1.2%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           4 |
| DERMATITIS CONTACT   | 0 ( 0.0%)    | 0 ( 0.0%)                 | 1 ( 1.2%)                |               1 |           6 |
| ERYTHEMA             | 9 ( 10.5%)   | 14 ( 16.7%)               | 13 ( 15.5%)              |               1 |           8 |
| HYPERHIDROSIS        | 2 ( 2.3%)    | 8 ( 9.5%)                 | 4 ( 4.8%)                |               1 |           9 |
| PRURITUS             | 8 ( 9.3%)    | 26 ( 31.0%)               | 21 ( 25.0%)              |               1 |          10 |
| PRURITUS GENERALISED | 0 ( 0.0%)    | 1 ( 1.2%)                 | 1 ( 1.2%)                |               1 |          11 |
| RASH                 | 4 ( 4.7%)    | 8 ( 9.5%)                 | 13 ( 15.5%)              |               1 |          12 |
| RASH ERYTHEMATOUS    | 0 ( 0.0%)    | 0 ( 0.0%)                 | 2 ( 2.4%)                |               1 |          13 |
| RASH MACULO-PAPULAR  | 0 ( 0.0%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               1 |          14 |
| RASH PAPULAR         | 0 ( 0.0%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               1 |          15 |
| RASH PRURITIC        | 0 ( 0.0%)    | 2 ( 2.4%)                 | 1 ( 1.2%)                |               1 |          16 |
| SKIN EXFOLIATION     | 0 ( 0.0%)    | 0 ( 0.0%)                 | 1 ( 1.2%)                |               1 |          17 |
| SKIN IRRITATION      | 2 ( 2.3%)    | 5 ( 6.0%)                 | 6 ( 7.1%)                |               1 |          18 |
| SKIN ODOUR ABNORMAL  | 0 ( 0.0%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               1 |          19 |
| SKIN ULCER           | 1 ( 1.2%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |          20 |
| URTICARIA            | 0 ( 0.0%)    | 1 ( 1.2%)                 | 1 ( 1.2%)                |               1 |          21 |

In the above example, `AEREL` doesn’t exist in `adsl`, therefore we used
[`set_pop_where()`](https://atorus-research.github.io/Tplyr/reference/where.md)
to remove the filter criteria on the population data. Setting the
population dataset where parameter to `TRUE` removes any filter applied
by the population data. If
[`set_pop_where()`](https://atorus-research.github.io/Tplyr/reference/where.md)
is not set for the population data, it will default to the `where`
parameter used in
[`tplyr_table()`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md).
The same logic applies to the population treatment variable. `TRTA` does
not exist in `adsl` either, so we used
[`set_pop_treat_var()`](https://atorus-research.github.io/Tplyr/reference/pop_treat_var.md)
to change it to the appropriate variable in `adsl`.

Note the percentage values in the summary above. By setting the
population data, **Tplyr** now knew to use those values when calculating
the percentages for the distinct counts of subjects who experienced the
summarized adverse events. Furthermore, with the population data
provided, **Tplyr** is able to calculate your header N’s properly:

``` r
header_n(t) %>% 
  kable()
```

| TRT01A               |   n |
|:---------------------|----:|
| Placebo              |  86 |
| Xanomeline High Dose |  84 |
| Xanomeline Low Dose  |  84 |

Note: it’s expected the
[`set_distinct_by()`](https://atorus-research.github.io/Tplyr/reference/set_distinct_by.md)
function is used with population data. This is because it does not make
sense to use population data denominators unless you have distinct
counts. The entire point of population data is to use subject counts, so
non-distinct counts would potentially count multiple records per subject
and then the percentage doesn’t make any sense.

## Data Completion

When creating summary tables, often we have to mock up the potential
values of data, even if those values aren’t present in the data we’re
summarizing. **Tplyr** does its best effort to do this for you. Let’s
consider the following dataset:

| USUBJID | AVISIT    | PECAT | PARAM | TRT01A | AVALC       | AVAL | BASEC       |
|:--------|:----------|:------|:------|:-------|:------------|-----:|:------------|
| 101-001 | Screening | A     | Head  | TRT A  | Normal      |    1 | Abnormal    |
| 101-001 | Screening | A     | Lungs | TRT A  | Normal      |    2 | Semi-Normal |
| 101-001 | Day -1    | A     | Lungs | TRT A  | Normal      |    3 | Normal      |
| 101-001 | Day 5     | A     | Lungs | TRT A  | Normal      |    4 | Normal      |
| 101-002 | Screening | A     | Head  | TRT B  | Semi-Normal |    5 | Normal      |
| 101-002 | Screening | A     | Lungs | TRT B  | Normal      |    6 | Normal      |

Let’s say we want to create a count summary for this dataset, and report
it by PARAM and AVISIT. Note that in the data, `PARAM=="HEAD"` is only
collected at screening, while `LUNGS` is collected at Screening, Day -1,
and Day 5.

``` r
tplyr_table(tplyr_adpe, TRT01A) %>%
  add_layer(
    group_count(AVALC, by = vars(PARAM, AVISIT))
  ) %>% 
  build() %>% 
  select(-starts_with('ord')) %>% 
  head(18) %>% 
  kable()
```

| row_label1 | row_label2 | row_label3  | var1_TRT A | var1_TRT B |
|:-----------|:-----------|:------------|:-----------|:-----------|
| Head       | Screening  | Normal      | 2 ( 14.3%) | 0 ( 0.0%)  |
| Head       | Screening  | Semi-Normal | 0 ( 0.0%)  | 1 ( 14.3%) |
| Head       | Screening  | Abnormal    | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Head       | Day -1     | Normal      | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Head       | Day -1     | Semi-Normal | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Head       | Day -1     | Abnormal    | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Head       | Day 5      | Normal      | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Head       | Day 5      | Semi-Normal | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Head       | Day 5      | Abnormal    | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Lungs      | Screening  | Normal      | 2 ( 14.3%) | 2 ( 28.6%) |
| Lungs      | Screening  | Semi-Normal | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Lungs      | Screening  | Abnormal    | 2 ( 14.3%) | 0 ( 0.0%)  |
| Lungs      | Day -1     | Normal      | 4 ( 28.6%) | 2 ( 28.6%) |
| Lungs      | Day -1     | Semi-Normal | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Lungs      | Day -1     | Abnormal    | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Lungs      | Day 5      | Normal      | 2 ( 14.3%) | 2 ( 28.6%) |
| Lungs      | Day 5      | Semi-Normal | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Lungs      | Day 5      | Abnormal    | 2 ( 14.3%) | 0 ( 0.0%)  |

By default, given the `by` variables of PARAM and AVISIT, all of the
potential visits have dummy rows created that are 0 filled - meaning
results of 0 records for all treatment groups are presented. However,
that might not be what you wish to present. Perhaps `HEAD` was only
intended to be collected at the Screening visit so it’s unnecessary to
present other visits. To address this, you can use the
[`set_limit_data_by()`](https://atorus-research.github.io/Tplyr/reference/set_limit_data_by.md)
function.

``` r
tplyr_table(tplyr_adpe, TRT01A) %>%
  add_layer(
    group_count(AVALC, by = vars(PARAM, AVISIT)) %>% 
      set_limit_data_by(PARAM, AVISIT)
  ) %>% 
  build() %>% 
  select(-starts_with('ord')) %>% 
  head(12) %>% 
  kable()
```

| row_label1 | row_label2 | row_label3  | var1_TRT A | var1_TRT B |
|:-----------|:-----------|:------------|:-----------|:-----------|
| Head       | Screening  | Normal      | 2 ( 14.3%) | 0 ( 0.0%)  |
| Head       | Screening  | Semi-Normal | 0 ( 0.0%)  | 1 ( 14.3%) |
| Head       | Screening  | Abnormal    | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Lungs      | Screening  | Normal      | 2 ( 14.3%) | 2 ( 28.6%) |
| Lungs      | Screening  | Semi-Normal | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Lungs      | Screening  | Abnormal    | 2 ( 14.3%) | 0 ( 0.0%)  |
| Lungs      | Day -1     | Normal      | 4 ( 28.6%) | 2 ( 28.6%) |
| Lungs      | Day -1     | Semi-Normal | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Lungs      | Day -1     | Abnormal    | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Lungs      | Day 5      | Normal      | 2 ( 14.3%) | 2 ( 28.6%) |
| Lungs      | Day 5      | Semi-Normal | 0 ( 0.0%)  | 0 ( 0.0%)  |
| Lungs      | Day 5      | Abnormal    | 2 ( 14.3%) | 0 ( 0.0%)  |

Here you can see that now records for `HEAD` only present the screening
visit. For count and shift layers, you can additionally dig further in
to use target variables:

``` r
tplyr_table(tplyr_adpe, TRT01A) %>%
  add_layer(
    group_count(AVALC, by = vars(PARAM, AVISIT)) %>% 
      set_limit_data_by(PARAM, AVISIT, AVALC)
  ) %>% 
  build() %>% 
  select(-starts_with('ord')) %>% 
  kable()
```

| row_label1 | row_label2 | row_label3  | var1_TRT A | var1_TRT B |
|:-----------|:-----------|:------------|:-----------|:-----------|
| Head       | Screening  | Normal      | 2 ( 14.3%) | 0 ( 0.0%)  |
| Lungs      | Screening  | Normal      | 2 ( 14.3%) | 2 ( 28.6%) |
| Lungs      | Screening  | Abnormal    | 2 ( 14.3%) | 0 ( 0.0%)  |
| Lungs      | Day -1     | Normal      | 4 ( 28.6%) | 2 ( 28.6%) |
| Lungs      | Day 5      | Normal      | 2 ( 14.3%) | 2 ( 28.6%) |
| Lungs      | Day 5      | Abnormal    | 2 ( 14.3%) | 0 ( 0.0%)  |
| Head       | Screening  | Semi-Normal | 0 ( 0.0%)  | 1 ( 14.3%) |

This effectively limits to the values present in the data itself.

## Where to Go From Here

With the table level settings under control, now you’re ready to learn
more about what **Tplyr** has to offer in each layer.

- Learn more about descriptive statistics layers in
  [`vignette("desc")`](https://atorus-research.github.io/Tplyr/articles/desc.md)
- Learn more about count and shift layers in
  [`vignette("count")`](https://atorus-research.github.io/Tplyr/articles/count.md)
- Learn more about shift layers in
  [`vignette("shift")`](https://atorus-research.github.io/Tplyr/articles/shift.md)
- Learn more about calculating risk differences in
  [`vignette("riskdiff")`](https://atorus-research.github.io/Tplyr/articles/riskdiff.md)
- Learn more about sorting **Tplyr** tables in
  [`vignette("sort")`](https://atorus-research.github.io/Tplyr/articles/sort.md)
- Learn more about using **Tplyr** options in
  [`vignette("options")`](https://atorus-research.github.io/Tplyr/articles/options.md)
- And finally, learn more about producing and outputting styled tables
  using **Tplyr** in
  [`vignette("styled-table")`](https://atorus-research.github.io/Tplyr/articles/styled-table.md)
