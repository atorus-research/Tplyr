# Shift Layers

Shift tables are a special kind of frequency table - but what they count
are changes in state. This is most common when looking at laboratory
ranges, where you may be interested in seeing how a subject’s results
related to normal ranges. The ‘change in state’ would refer to how that
subject’s results were at baseline versus different points of measure.
Shift tables allow you to see the distribution of how subjects move
between normal ranges, and if the population is improving or worsening
as the study progresses.

While shift tables are very similar to a normal frequency table, there’s
more nuance here, and thus we decided to create
[`group_shift()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md).
This function is largely an abstraction of a count layer, and in fact
re-uses a good deal of the same underlying code. But we handle some of
the complexity for you to make the interface easy to use and the
behavior similar to that of the
[`group_count()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md)
and
[`group_desc()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md)
APIs. Given that shift tables are built on count layers, many functions
that work with count layers behave in the same way when used on shift
layers. However, the following cannot be used in shift layers:

- Functions related to nested counts, including
  [`set_nest_count()`](https://atorus-research.github.io/Tplyr/reference/set_nest_count.md),
  [`set_outer_sort_position()`](https://atorus-research.github.io/Tplyr/reference/set_outer_sort_position.md)
- Functions related to total rows and missing rows, including
  [`set_missing_count()`](https://atorus-research.github.io/Tplyr/reference/set_missing_count.md),
  [`add_total_row()`](https://atorus-research.github.io/Tplyr/reference/add_total_row.md),
  [`set_total_row_label()`](https://atorus-research.github.io/Tplyr/reference/set_total_row_label.md)
- Risk difference, including
  [`add_risk_diff()`](https://atorus-research.github.io/Tplyr/reference/add_risk_diff.md)
- and finally, result-based sorting methods, including
  [`set_order_count_method()`](https://atorus-research.github.io/Tplyr/reference/ordering.md),
  [`set_ordering_cols()`](https://atorus-research.github.io/Tplyr/reference/ordering.md),
  [`set_result_order_var()`](https://atorus-research.github.io/Tplyr/reference/ordering.md)

One thing to note - the
[`group_shift()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md)
API is intended to be used on shift tables where one group is presented
in rows and the other group in columns. Occasionally, shift tables will
have a row-based approach that shows “Low to High”, “Normal to High”,
etc. For those situations,
[`group_count()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md)
will do just fine.

## A Basic Example

Let’s look at an example.

``` r
tplyr_table(tplyr_adlb, TRTA, where=PARAMCD == "CK") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, VISIT))
  ) %>%
  build() %>%
  head(20) %>%
  kable()
```

| row_label1            | row_label2 | row_label3 | var1_Placebo_H | var1_Placebo_N | var1_Xanomeline High Dose_H | var1_Xanomeline High Dose_N | var1_Xanomeline Low Dose_H | var1_Xanomeline Low Dose_N | ord_layer_index | ord_layer_1 | ord_layer_2 | ord_layer_3 |
|:----------------------|:-----------|:-----------|:---------------|:---------------|:----------------------------|:----------------------------|:---------------------------|:---------------------------|----------------:|------------:|------------:|------------:|
| Creatine Kinase (U/L) | WEEK 12    | H          | 0              | 0              | 1                           | 0                           | 0                          | 0                          |               1 |          35 |           1 |           1 |
| Creatine Kinase (U/L) | WEEK 12    | N          | 1              | 11             | 1                           | 6                           | 1                          | 6                          |               1 |          35 |           1 |           3 |
| Creatine Kinase (U/L) | WEEK 24    | H          | 0              | 0              | 0                           | 0                           | 0                          | 0                          |               1 |          35 |           2 |           1 |
| Creatine Kinase (U/L) | WEEK 24    | N          | 2              | 10             | 0                           | 2                           | 0                          | 2                          |               1 |          35 |           2 |           3 |
| Creatine Kinase (U/L) | WEEK 8     | H          | 0              | 0              | 0                           | 0                           | 0                          | 0                          |               1 |          35 |           3 |           1 |
| Creatine Kinase (U/L) | WEEK 8     | N          | 1              | 6              | 1                           | 9                           | 0                          | 6                          |               1 |          35 |           3 |           3 |

First, let’s look at the differences in the shift API. Shift layers
*must* take a row and a column variable, as the layer is designed to
create a box for you that explains the changes in state. The row
variable will typically be your “from” variable, and the column variable
will typically be your “to” variable. Behind the scenes, **Tplyr**
breaks this down for you to properly count and present the data.

For the most part, the last example gets us where we want to go - but
there’s still some that’s left to be desired. It doesn’t look like there
are any ‘L’ values for BNRIND in the dataset so we are not getting any
rows containing ‘L’. Let’s see if we can fix that by dummying in the
possible values.

## Filling Missing Groups Using Factors

``` r
tplyr_adlb$ANRIND <- factor(tplyr_adlb$ANRIND, levels=c("L", "N", "H"))
tplyr_adlb$BNRIND <- factor(tplyr_adlb$BNRIND, levels=c("L", "N", "H"))
tplyr_table(tplyr_adlb, TRTA, where=PARAMCD == "CK") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, VISIT))
  ) %>%
  build() %>%
  head(20) %>%
  kable()
```

| row_label1            | row_label2 | row_label3 | var1_Placebo_L | var1_Placebo_N | var1_Placebo_H | var1_Xanomeline High Dose_L | var1_Xanomeline High Dose_N | var1_Xanomeline High Dose_H | var1_Xanomeline Low Dose_L | var1_Xanomeline Low Dose_N | var1_Xanomeline Low Dose_H | ord_layer_index | ord_layer_1 | ord_layer_2 | ord_layer_3 |
|:----------------------|:-----------|:-----------|:---------------|:---------------|:---------------|:----------------------------|:----------------------------|:----------------------------|:---------------------------|:---------------------------|:---------------------------|----------------:|------------:|------------:|------------:|
| Creatine Kinase (U/L) | WEEK 12    | L          | 0              | 0              | 0              | 0                           | 0                           | 0                           | 0                          | 0                          | 0                          |               1 |          35 |           1 |           1 |
| Creatine Kinase (U/L) | WEEK 12    | N          | 0              | 11             | 1              | 0                           | 6                           | 1                           | 0                          | 6                          | 1                          |               1 |          35 |           1 |           2 |
| Creatine Kinase (U/L) | WEEK 12    | H          | 0              | 0              | 0              | 0                           | 0                           | 1                           | 0                          | 0                          | 0                          |               1 |          35 |           1 |           3 |
| Creatine Kinase (U/L) | WEEK 24    | L          | 0              | 0              | 0              | 0                           | 0                           | 0                           | 0                          | 0                          | 0                          |               1 |          35 |           2 |           1 |
| Creatine Kinase (U/L) | WEEK 24    | N          | 0              | 10             | 2              | 0                           | 2                           | 0                           | 0                          | 2                          | 0                          |               1 |          35 |           2 |           2 |
| Creatine Kinase (U/L) | WEEK 24    | H          | 0              | 0              | 0              | 0                           | 0                           | 0                           | 0                          | 0                          | 0                          |               1 |          35 |           2 |           3 |
| Creatine Kinase (U/L) | WEEK 8     | L          | 0              | 0              | 0              | 0                           | 0                           | 0                           | 0                          | 0                          | 0                          |               1 |          35 |           3 |           1 |
| Creatine Kinase (U/L) | WEEK 8     | N          | 0              | 6              | 1              | 0                           | 9                           | 1                           | 0                          | 6                          | 0                          |               1 |          35 |           3 |           2 |
| Creatine Kinase (U/L) | WEEK 8     | H          | 0              | 0              | 0              | 0                           | 0                           | 0                           | 0                          | 0                          | 0                          |               1 |          35 |           3 |           3 |

There we go. This is another situation where using factors in R enables
us to dummy values within the dataset. Furthermore, since factors are
ordered, Tplyr automatically corrected the sort order of the row labels
too. Now, instead of alphabetically (H then L then N), our rows are
sorted by factor levels (L then N then H).

## Where to go from here

There’s much more to learn! Check out the sorting vignette for more
information on sorting. Additionally, check out our vignette on
denominators to understand controlling more of the nuance in shift
tables.
