# Tplyr Metadata

**Tplyr** has a bit of a unique design, which might feel a bit weird as
you get used to the package. The process flow of building a
[`tplyr_table()`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)
object first, and then using
[`build()`](https://atorus-research.github.io/Tplyr/reference/build.md)
to construct the data frame is different than programming in the
tidyverse, or creating a ggplot. Why create the
[`tplyr_table()`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)
object first? Why is the
[`tplyr_table()`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)
object different than the resulting data frame?

The purpose of the
[`tplyr_table()`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)
object is to let **Tplyr** do more than just summarize data. As you
build the table, all of the metadata around the table being built is
maintained - the target variables being summarized, the grouped
variables by row and column, the filter conditions necessary applied to
the table and each layer. As a user, you provide this information to
create the summary. But what about after the results are produced?
Summarizing data inevitably leads to new questions. Within clinical
summaries, you may want to know which subjects experienced an adverse
event, or why the lab summaries of a particular visit’s descriptive
statistics are abnormal. Normally, you’d write a query to recreate the
data that lead to that particular summary. **Tplyr** now allows you to
immediately extract the input data or metadata that created an output
result, thus providing traceability from the result back to the source.

## Generating the Metadata

Consider the following example:

``` r
t <- tplyr_table(tplyr_adsl, TRT01P, where = SAFFL == "Y") %>% 
  add_layer(
    group_count(RACE)
  ) %>% 
  add_layer(
    group_desc(AGE, where = EFFFL == "Y")
  )

dat <- t %>% build(metadata=TRUE)

kable(dat)
```

| row_id | row_label1                       | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:-------|:---------------------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|
| c1_1   | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               1 |           1 |
| c2_1   | BLACK OR AFRICAN AMERICAN        | 8 ( 9.3%)    | 9 ( 10.7%)                | 6 ( 7.1%)                |               1 |           2 |
| c3_1   | WHITE                            | 78 ( 90.7%)  | 74 ( 88.1%)               | 78 ( 92.9%)              |               1 |           3 |
| d1_2   | n                                | 79           | 74                        | 81                       |               2 |           1 |
| d2_2   | Mean (SD)                        | 75.0 ( 8.43) | 73.9 ( 7.87)              | 76.1 ( 8.02)             |               2 |           2 |
| d3_2   | Median                           | 76.0         | 75.5                      | 78.0                     |               2 |           3 |
| d4_2   | Q1, Q3                           | 69.5, 81.0   | 70.2, 79.0                | 71.0, 82.0               |               2 |           4 |
| d5_2   | Min, Max                         | 52, 88       | 56, 88                    | 51, 88                   |               2 |           5 |
| d6_2   | Missing                          | 0            | 0                         | 0                        |               2 |           6 |

To trigger the creation of metadata, the
[`build()`](https://atorus-research.github.io/Tplyr/reference/build.md)
function has a new argument `metadata`. By specifying `TRUE`, the
underlying metadata within **Tplyr** are prepared in an extractable
format. This is the only action a user needs to specify for this action
to take place.

When the `metadata` argument is used, a new column will be produced in
the output dataframe called `row_id`. The `row_id` variable provides a
persistent reference to a row of interest, even if the output dataframe
is sorted. If you review
[`vignette("styled-table")`](https://atorus-research.github.io/Tplyr/articles/styled-table.md),
note that we expect a certain amount of post processing and styling of
the built data frame from Tplyr, to let you use whatever other packages
you prefer. As such, this reference ID is necessary.

## Extracting The Input Source

So, let’s cut to the chase. The most likely way you would use this
metadata is to pull out the source data that created a cell. For this,
we’ve provided the function
[`get_meta_subset()`](https://atorus-research.github.io/Tplyr/reference/get_meta_subset.md).
The only information that you need is the `row_id` and column name of
the result cell of interest. For example, looking at the result above,
what if we want to know who the 8 subjects in the Placebo group who
where Black or African American:

``` r
get_meta_subset(t, 'c2_1', 'var1_Placebo') %>% 
  kable()
```

| USUBJID     | TRT01P  | SAFFL | RACE                      |
|:------------|:--------|:------|:--------------------------|
| 01-701-1203 | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-701-1363 | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-705-1282 | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-706-1041 | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-708-1286 | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-708-1296 | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-708-1378 | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-711-1036 | Placebo | Y     | BLACK OR AFRICAN AMERICAN |

By using the `row_id` and column, the dataframe is pulled right out for
us. Notice that `USUBJID` was included by default, even though **Tplyr**
there’s no reference anywhere in the
[`tplyr_table()`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)
to the variable `USUBJID`. This is because
[`get_meta_subset()`](https://atorus-research.github.io/Tplyr/reference/get_meta_subset.md)
has an additional argument `add_cols` that allows you to specify
additional columns you want included in the resulting dataframe, and has
a default of USUBJID. So let’s say we want additionally include the
variable `SEX`.

``` r
get_meta_subset(t, 'c2_1', 'var1_Placebo', add_cols = vars(USUBJID, SEX)) %>% 
  kable()
```

| USUBJID     | SEX | TRT01P  | SAFFL | RACE                      |
|:------------|:----|:--------|:------|:--------------------------|
| 01-701-1203 | F   | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-701-1363 | F   | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-705-1282 | F   | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-706-1041 | F   | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-708-1286 | F   | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-708-1296 | M   | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-708-1378 | M   | Placebo | Y     | BLACK OR AFRICAN AMERICAN |
| 01-711-1036 | M   | Placebo | Y     | BLACK OR AFRICAN AMERICAN |

Variables should be provided using
[`dplyr::vars()`](https://dplyr.tidyverse.org/reference/vars.html), just
like the `cols` argument on
[`tplyr_table()`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)
and the `by` arguments in each layer type.

As mentioned, the input source data can be extracted for any result cell
created by Tplyr. So let’s say we want to know the subjects relevant for
the descriptive statistics around age in the Xanomeline High Dose group:

``` r
get_meta_subset(t, 'd1_2', 'var1_Xanomeline High Dose') %>% 
  head(10) %>% 
  kable()
```

| USUBJID     | TRT01P               | EFFFL | SAFFL | AGE |
|:------------|:---------------------|:------|:------|----:|
| 01-701-1028 | Xanomeline High Dose | Y     | Y     |  71 |
| 01-701-1034 | Xanomeline High Dose | Y     | Y     |  77 |
| 01-701-1133 | Xanomeline High Dose | Y     | Y     |  81 |
| 01-701-1146 | Xanomeline High Dose | Y     | Y     |  75 |
| 01-701-1148 | Xanomeline High Dose | Y     | Y     |  57 |
| 01-701-1180 | Xanomeline High Dose | Y     | Y     |  56 |
| 01-701-1181 | Xanomeline High Dose | Y     | Y     |  79 |
| 01-701-1239 | Xanomeline High Dose | Y     | Y     |  56 |
| 01-701-1275 | Xanomeline High Dose | Y     | Y     |  61 |
| 01-701-1287 | Xanomeline High Dose | Y     | Y     |  56 |

*Note: Trimmed for space*

Notice how the columns returned are different. First off, within the
summary above, we pulled results from the descriptive statistics layer.
The target variable for this layer was `AGE`, and as such `AGE` is
returned in the resulting output. Additionally, a layer level `where`
argument was used to subset to `EFFFL == "Y"`, which leads to `EFFFL`
being included in the output as well.

## Extracting a Result Cell’s Metadata

To extract the dataframe in
[`get_meta_subset()`](https://atorus-research.github.io/Tplyr/reference/get_meta_subset.md),
the metadata of the result cell needs to first be extracted. This
metadata can be directly accessed using the function
[`get_meta_result()`](https://atorus-research.github.io/Tplyr/reference/get_meta_result.md).
Using the last example of
[`get_meta_subset()`](https://atorus-research.github.io/Tplyr/reference/get_meta_subset.md)
above:

``` r
get_meta_result(t, 'd1_2', 'var1_Xanomeline High Dose')
#> tplyr_meta: 4 names, 3 filters
#> Names:
#>      TRT01P, EFFFL, SAFFL, AGE 
#> Filters:
#>      TRT01P == c("Xanomeline High Dose"), EFFFL == "Y", SAFFL == "Y"
```

The resulting output is a new object **Tplyr** called
[`tplyr_meta()`](https://atorus-research.github.io/Tplyr/reference/tplyr_meta.md).
This is a container of a relevent metadata for a specific result. The
object itself is a list with two elements: `names` and `filters`.

The `names` element contains quosures for each variable relevant to a
specific result. This will include the target variable, the `by`
variables used on the layer, the `cols` variables used on the table, and
all variables included in any filter condition relevant to create the
result.

The `filters` element contains each filter condition (provided as calls)
necessary to create a particular cell. This will include the table level
`where` argument, the layer level `where` argument, the filter condition
for the specific value of any `by` variable or `cols` variable necessary
to create the cell, and similarly the filter for the treatment group of
interest.

The results are provided this was so that they can be unpacked directly
into `dplyr` syntax when necessary, which is exactly what happens in
[`get_meta_subset()`](https://atorus-research.github.io/Tplyr/reference/get_meta_subset.md).
For example:

``` r
m <- get_meta_result(t, 'd1_2', 'var1_Xanomeline High Dose')

tplyr_adsl %>% 
  filter(!!!m$filters) %>% 
  select(!!!m$names) %>% 
  head(10) %>% 
  kable()
```

| TRT01P               | EFFFL | SAFFL | AGE |
|:---------------------|:------|:------|----:|
| Xanomeline High Dose | Y     | Y     |  71 |
| Xanomeline High Dose | Y     | Y     |  77 |
| Xanomeline High Dose | Y     | Y     |  81 |
| Xanomeline High Dose | Y     | Y     |  75 |
| Xanomeline High Dose | Y     | Y     |  57 |
| Xanomeline High Dose | Y     | Y     |  56 |
| Xanomeline High Dose | Y     | Y     |  79 |
| Xanomeline High Dose | Y     | Y     |  56 |
| Xanomeline High Dose | Y     | Y     |  61 |
| Xanomeline High Dose | Y     | Y     |  56 |

*Note: Trimmed for space*

But - who says you can’t let your imagination run wild?

``` r
cat(c("tplyr_adsl %>%\n",
  "   filter(\n      ",
  paste(purrr::map_chr(m$filters, ~ rlang::as_label(.)), collpase=",\n      "),
  ") %>%\n",
  paste("   select(", paste(purrr::map_chr(m$names, rlang::as_label), collapse=", "), ")", sep="")
))
```

### Anti Joins

Most data presented within a table refers back to the target dataset
from which data are being summarized. In some cases, data presented may
refer to information *excluded* from the summary. This is the case when
you use the **Tplyr** function
[`add_missing_subjects_row()`](https://atorus-research.github.io/Tplyr/reference/add_missing_subjects_row.md).
In this case, the counts presented refer to data excluded from the
target which are present in the population data. The metadata thus needs
to refer to that excluded data. To handle this, there’s an additional
field called an ‘Anti Join’. Consider this example:

``` r
t <- tplyr_table(tplyr_adae, TRTA) %>%
  set_pop_data(tplyr_adsl) %>%
  set_pop_treat_var(TRT01A) %>%
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>%
      set_distinct_by(USUBJID) %>%
      add_missing_subjects_row(f_str("xx (XX.x%)", distinct_n, distinct_pct), sort_value = Inf)
  )

x <- build(t, metadata=TRUE)

tail(x) %>% 
  select(starts_with('row'), var1_Placebo) %>% 
  kable()
```

| row_id | row_label1                             | row_label2          | var1_Placebo |
|:-------|:---------------------------------------|:--------------------|:-------------|
| c18_1  | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | SKIN EXFOLIATION    | 0 ( 0.0%)    |
| c19_1  | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | SKIN IRRITATION     | 3 ( 3.5%)    |
| c20_1  | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | SKIN ODOUR ABNORMAL | 0 ( 0.0%)    |
| c21_1  | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | SKIN ULCER          | 1 ( 1.2%)    |
| c22_1  | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | URTICARIA           | 0 ( 0.0%)    |
| c23_1  | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | Missing             | 85 (98.8%)   |

The missing row in this example counts the subjects within their
respective treatment groups who do *not* have any adverse events for the
body system “SKIN AND SUBCUTANEOUS TISSUE DISORDERS”. Here’s what the
metadata for the result for the Placebo treatment group looks like.

``` r
m <- get_meta_result(t, 'c23_1', 'var1_Placebo')
m
#> tplyr_meta: 3 names, 4 filters
#> Names:
#>      TRTA, AEBODSYS, AEDECOD 
#> Filters:
#>      TRTA == c("Placebo"), AEBODSYS == c("SKIN AND SUBCUTANEOUS TISSUE DISORDERS"), TRUE, TRUE 
#> Anti-join:
#>     Join Meta:
#>         tplyr_meta: 1 names, 3 filters
#>         Names:
#>              TRT01A 
#>         Filters:
#>              TRT01A == c("Placebo"), TRUE, TRUE 
#>     On:
#>         USUBJID
```

This result has the addition field of ‘Anti-join’. This element has two
fields, which are the join metadata, and the “on” field, which specifies
a merging variable to be used when “anti-joining” with the target data.
The join metadata here refers to the data of interest from the
population data. Note that while the metadata for the target data has
variable names and filter conditions referring to AEBODSYS and AEDECOD,
these variables are *not* present within the join metadata, because that
information is not present within the population data.

While the usual joins we work with focus on the overlap between two
sets, an anti-join looks at the non-overlap. The metadata provided here
will specifically give us “The subjects within the Placebo treatment
group who do **not** have an adverse event within the body system ‘SKIN
AND SUBCUTANEOUS TISSUE DISORDERS’”.

Extracting this metadata works very much the same way as extracting
other results.

``` r
head(get_meta_subset(t, 'c23_1', 'var1_Placebo'))
#> # A tibble: 6 × 2
#>   USUBJID     TRT01A 
#>   <chr>       <chr>  
#> 1 01-701-1015 Placebo
#> 2 01-701-1047 Placebo
#> 3 01-701-1118 Placebo
#> 4 01-701-1153 Placebo
#> 5 01-701-1203 Placebo
#> 6 01-701-1234 Placebo
```

If you’re not working with the `tplyr_table` object, then there’s some
additional information you need to provide to the function.

``` r
head(get_meta_subset(t$metadata, 'c23_1', 'var1_Placebo', 
                     target=t$target, pop_data=t$pop_data))
#> # A tibble: 6 × 2
#>   USUBJID     TRT01A 
#>   <chr>       <chr>  
#> 1 01-701-1015 Placebo
#> 2 01-701-1047 Placebo
#> 3 01-701-1118 Placebo
#> 4 01-701-1153 Placebo
#> 5 01-701-1203 Placebo
#> 6 01-701-1234 Placebo
```

    tplyr_adsl %>%
        filter(
           TRTA == c("Placebo") ,
           AEBODSYS == c("SKIN AND SUBCUTANEOUS TISSUE DISORDERS") ,
           TRUE ,
           TRUE ,
           ) %>%
        select(TRTA, AEBODSYS, AEDECOD)

## So, What Does This Get Me?

So we get get metadata around a result cell, and we can get the exact
results from a result cell. You just need a row ID and a column name.
But - what does that get you? You can query your tables - and that’s
great. But how do you *use* that.

The idea behind this is really to support
[Shiny](https://shiny.posit.co/). Consider this minimal application.
Click any of the result cells within the table and see what happens.

*Source code available
[here](https://github.com/atorus-research/Tplyr-shiny-demo)*

*That’s* what this is all about. The persistent row_id and column
selection enables you to use something like Shiny to automatically query
a cell based on its position in a table. Using click events and a
package like [reactable](https://glin.github.io/reactable/), you can
pick up the row and column selected and pass that information into
[`get_meta_result()`](https://atorus-research.github.io/Tplyr/reference/get_meta_result.md).
Once you get the resulting data frame, it’s up to you what you do with
it, and you have the world of Shiny at the tip of your fingers.
