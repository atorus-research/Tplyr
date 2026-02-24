# Post-Processing

We’ve made a large effort to make **Tplyr** tables flexible, but not
everything can (or, in some cases, we think should) be handled during
table construction itself. To address this, **Tplyr** has several
post-processing functions that help put finishing touches on your data
to help with presentation.

## String wrapping

Certain types of output formats don’t elegantly handle string wrapping
of text. For some formats, this could simply be the width of the text
before a word wraps. Other formats, such as LaTex outputs to PDF
(depending on the rendering engine) may wrap white space fine, but words
whose character length is longer than the allotted width may print wider
than the table cell.

To address this, in **Tplyr** we’ve added the function
[`str_indent_wrap()`](https://atorus-research.github.io/Tplyr/reference/str_indent_wrap.md).
This is largely built on top of the function
[`stringr::str_wrap()`](https://stringr.tidyverse.org/reference/str_wrap.html)
to preserve as much efficiency as possible, but two issues common in
clinical outputs have been addressed:

- Preceding indentation of a word is preserved. So for example, in a
  nested adverse event table, if the preferred term is indented, this
  function will wrap but preserve that words indentation with each new
  line.
- Words that exceed the specified width will be wrapped with
  hyphenation.

As a post-processing function, note that this function works on a
**tibble or data.frame** object, and not a **Tplyr** table.

Let’s look at an example.

``` r
dat <- tibble(
  row_label1 = c("RENAL AND URINARY DISORDERS", "   NEPHROLITHIASIS"),
  var1_Placebo = c(" 5 (50.0%)", " 3 (30.0%)")
)

dat %>% 
  mutate(
    row_label1 = str_indent_wrap(row_label1, width = 10)
  )
#> # A tibble: 2 × 2
#>   row_label1                      var1_Placebo
#>   <chr>                           <chr>       
#> 1 "RENAL AND\nURINARY\nDISORDERS" " 5 (50.0%)"
#> 2 "   NEPHROLIT-\n   HIASIS"      " 3 (30.0%)"
```

*Note: We’re viewing the data frame output here because HTML based
outputs eliminate duplicate white spaces, which makes it difficult to
see things like padded indentation*

## Row Masking

Row masking is the process blanking of repeat row values within a data
frame to give the appearance of grouping variables. Some table packages,
such as [**gt**](https://gt.rstudio.com/), will handle this for you.
Other packages, like
[**huxtable**](https://hughjonesd.github.io/huxtable/), have options
like merging cells, but this may be a more simplistic approach.
Furthermore, this is a common approach in clinical tables when data
validation is done on an output dataframe.

``` r
dat <- tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(RACE, by = "Race n (%)")
  ) %>% 
  build() %>% 
  select(1:3)

kable(dat)
```

| row_label1 | row_label2                       | var1_Placebo |
|:-----------|:---------------------------------|:-------------|
| Race n (%) | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)    |
| Race n (%) | BLACK OR AFRICAN AMERICAN        | 8 ( 9.3%)    |
| Race n (%) | WHITE                            | 78 ( 90.7%)  |

In this example, note that “Race n (%)” is duplicated for each row. We
can blank this out using \`apply_row_masks()

``` r
dat %>% 
  apply_row_masks() %>% 
  kable()
```

| row_label1 | row_label2                       | var1_Placebo |
|:-----------|:---------------------------------|:-------------|
| Race n (%) | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)    |
|            | BLACK OR AFRICAN AMERICAN        | 8 ( 9.3%)    |
|            | WHITE                            | 78 ( 90.7%)  |

A second feature of
[`apply_row_masks()`](https://atorus-research.github.io/Tplyr/reference/apply_row_masks.md)
is the ability to apply row breaks between different groups of data, for
example, different layers of a table.

``` r
dat <- tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(RACE, by = "Race n (%)")
  ) %>% 
  add_layer(
    group_desc(AGE, by = "Age (years)")
  ) %>% 
  build()
  
dat %>% 
  apply_row_masks(row_breaks=TRUE) %>% 
  kable()
```

| row_label1  | row_label2                       | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 | ord_layer_2 | ord_break |
|:------------|:---------------------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|------------:|----------:|
| Race n (%)  | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               1 |           1 |           1 |         1 |
|             | BLACK OR AFRICAN AMERICAN        | 8 ( 9.3%)    | 9 ( 10.7%)                | 6 ( 7.1%)                |               1 |           1 |           2 |         1 |
|             | WHITE                            | 78 ( 90.7%)  | 74 ( 88.1%)               | 78 ( 92.9%)              |               1 |           1 |           3 |         1 |
|             |                                  |              |                           |                          |               1 |          NA |          NA |         2 |
| Age (years) | n                                | 86           | 84                        | 84                       |               2 |           1 |           1 |         1 |
|             | Mean (SD)                        | 75.2 ( 8.59) | 74.4 ( 7.89)              | 75.7 ( 8.29)             |               2 |           1 |           2 |         1 |
|             | Median                           | 76.0         | 76.0                      | 77.5                     |               2 |           1 |           3 |         1 |
|             | Q1, Q3                           | 69.2, 81.8   | 70.8, 80.0                | 71.0, 82.0               |               2 |           1 |           4 |         1 |
|             | Min, Max                         | 52, 89       | 56, 88                    | 51, 88                   |               2 |           1 |           5 |         1 |
|             | Missing                          | 0            | 0                         | 0                        |               2 |           1 |           6 |         1 |
|             |                                  |              |                           |                          |               2 |          NA |          NA |         2 |

The row breaks are inserted as blank rows. Additionally, when row breaks
are inserted you’ll have the additional variable `ord_break` added to
the dataframe, where the value is 1 for table data rows and 2 for the
newly added break rows. Character variables will have blank values
(i.e. `""`) and the numeric sorting values will be `NA`.

There are a few considerations when using
[`apply_row_masks()`](https://atorus-research.github.io/Tplyr/reference/apply_row_masks.md):

- This function is order dependent, so make sure your data are sorted
  before submitting to
  [`apply_row_masks()`](https://atorus-research.github.io/Tplyr/reference/apply_row_masks.md)
- When inserting row breaks, by default the Tpylr variable
  `ord_layer_index` is used. You can submit other variables via the
  ellipsis parameter (`...`) if you’d like to use a different variable
  grouping to insert rows

## Collapsing Row Labels

Different table formats call for different handling of row labels,
depending on the preferences of an individual organization and the
specifics of the table at hand. **Tplyr** inherently creates row labels
as separate columns, but similar to the way that count layers nest the
inner and the outer layer, we also offer the
[`collapse_row_labels()`](https://atorus-research.github.io/Tplyr/reference/collapse_row_labels.md)
function to pull multiple row labels into a single column.

``` r
dat <- tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(RACE, by = vars("Race n (%)", SEX))
  ) %>% 
  add_layer(
    group_desc(AGE, by = vars("Age (years)", SEX))
  ) %>% 
  build()

collapse_row_labels(dat, row_label1, row_label2, row_label3) %>% 
  select(row_label, var1_Placebo)
#> # A tibble: 26 × 2
#>    row_label                              var1_Placebo 
#>    <chr>                                  <chr>        
#>  1 "Race n (%)"                           ""           
#>  2 "  F"                                  ""           
#>  3 "    AMERICAN INDIAN OR ALASKA NATIVE" " 0 (  0.0%)"
#>  4 "    BLACK OR AFRICAN AMERICAN"        " 5 (  5.8%)"
#>  5 "    WHITE"                            "48 ( 55.8%)"
#>  6 "Race n (%)"                           ""           
#>  7 "  M"                                  ""           
#>  8 "    AMERICAN INDIAN OR ALASKA NATIVE" " 0 (  0.0%)"
#>  9 "    BLACK OR AFRICAN AMERICAN"        " 3 (  3.5%)"
#> 10 "    WHITE"                            "30 ( 34.9%)"
#> # ℹ 16 more rows
```

By default, indentation is set to 2 spaces, but by using the `indent`
parameter you can change this to any string you desire.

``` r
collapse_row_labels(dat, row_label1, row_label2, row_label3, indent = "&nbsp;&nbsp;") %>% 
  select(row_label, var1_Placebo) %>% 
  kable(escape=FALSE)
```

| row_label                            | var1_Placebo |
|:-------------------------------------|:-------------|
| Race n (%)                           |              |
|   F                                  |              |
|     AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)    |
|     BLACK OR AFRICAN AMERICAN        | 5 ( 5.8%)    |
|     WHITE                            | 48 ( 55.8%)  |
| Race n (%)                           |              |
|   M                                  |              |
|     AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)    |
|     BLACK OR AFRICAN AMERICAN        | 3 ( 3.5%)    |
|     WHITE                            | 30 ( 34.9%)  |
| Age (years)                          |              |
|   F                                  |              |
|     n                                | 53           |
|     Mean (SD)                        | 76.4 ( 8.73) |
|     Median                           | 78.0         |
|     Q1, Q3                           | 70.0, 84.0   |
|     Min, Max                         | 59, 89       |
|     Missing                          | 0            |
| Age (years)                          |              |
|   M                                  |              |
|     n                                | 33           |
|     Mean (SD)                        | 73.4 ( 8.15) |
|     Median                           | 74.0         |
|     Q1, Q3                           | 69.0, 80.0   |
|     Min, Max                         | 52, 85       |
|     Missing                          | 0            |

You also have control over which columns you collapse, allowing you to
keep separate row labels if you don’t want all collapsed together

``` r
collapse_row_labels(dat, row_label1, row_label2, indent = "&nbsp;&nbsp;") %>% 
  select(row_label, row_label3, var1_Placebo) %>% 
  head() %>% 
  kable()
```

| row_label  | row_label3                       | var1_Placebo |
|:-----------|:---------------------------------|:-------------|
| Race n (%) |                                  |              |
|   F        | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)    |
|   F        | BLACK OR AFRICAN AMERICAN        | 5 ( 5.8%)    |
|   F        | WHITE                            | 48 ( 55.8%)  |
|   M        | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)    |
|   M        | BLACK OR AFRICAN AMERICAN        | 3 ( 3.5%)    |

## Leading Spaces in HTML Files

Another helper function we’ve made available is
[`replace_leading_whitespace()`](https://atorus-research.github.io/Tplyr/reference/replace_leading_whitespace.md).
In the table created above, note that the `indent` parameter was set
using `&nbsp;`, which is a non-breaking space. This can be used in HTML
files to preserve leading white spaces instead of automatically
stripping them in the display, as viewing utilities usually do. Ever
noticed that in your data viewers you typically don’t see leading
spaces? Yeah - that’s why!

Let’s take the example from above and not change the `indent` parameter.

``` r
collapse_row_labels(dat, row_label1, row_label2) %>% 
  select(row_label, row_label3, var1_Placebo) %>% 
  kable()
```

| row_label   | row_label3                       | var1_Placebo |
|:------------|:---------------------------------|:-------------|
| Race n (%)  |                                  |              |
| F           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)    |
| F           | BLACK OR AFRICAN AMERICAN        | 5 ( 5.8%)    |
| F           | WHITE                            | 48 ( 55.8%)  |
| M           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)    |
| M           | BLACK OR AFRICAN AMERICAN        | 3 ( 3.5%)    |
| M           | WHITE                            | 30 ( 34.9%)  |
| Age (years) |                                  |              |
| F           | n                                | 53           |
| F           | Mean (SD)                        | 76.4 ( 8.73) |
| F           | Median                           | 78.0         |
| F           | Q1, Q3                           | 70.0, 84.0   |
| F           | Min, Max                         | 59, 89       |
| F           | Missing                          | 0            |
| M           | n                                | 33           |
| M           | Mean (SD)                        | 73.4 ( 8.15) |
| M           | Median                           | 74.0         |
| M           | Q1, Q3                           | 69.0, 80.0   |
| M           | Min, Max                         | 52, 85       |
| M           | Missing                          | 0            |

In indented rows, the spaces still exist, and we can see that in the
dataframe output itself.

``` r
collapse_row_labels(dat, row_label1, row_label2) %>% 
  select(row_label, row_label3, var1_Placebo) %>% 
  head()
#> # A tibble: 6 × 3
#>   row_label    row_label3                         var1_Placebo 
#>   <chr>        <chr>                              <chr>        
#> 1 "Race n (%)" ""                                 ""           
#> 2 "  F"        "AMERICAN INDIAN OR ALASKA NATIVE" " 0 (  0.0%)"
#> 3 "  F"        "BLACK OR AFRICAN AMERICAN"        " 5 (  5.8%)"
#> 4 "  F"        "WHITE"                            "48 ( 55.8%)"
#> 5 "  M"        "AMERICAN INDIAN OR ALASKA NATIVE" " 0 (  0.0%)"
#> 6 "  M"        "BLACK OR AFRICAN AMERICAN"        " 3 (  3.5%)"
```

But the HTML view strips them off when we pass it into the `kable()`
function.
[`replace_leading_whitespace()`](https://atorus-research.github.io/Tplyr/reference/replace_leading_whitespace.md)
will take care of this for us by converting the spaces. Note that you’ll
see the `&nbsp;` in the raw data itself.

``` r
collapse_row_labels(dat, row_label1, row_label2) %>% 
  select(row_label, row_label3, var1_Placebo) %>% 
  mutate(
    across(where(is.character), ~ replace_leading_whitespace(.))
  ) %>% 
  head()
#> # A tibble: 6 × 3
#>   row_label     row_label3                         var1_Placebo      
#>   <chr>         <chr>                              <chr>             
#> 1 Race n (%)    ""                                 ""                
#> 2 &nbsp;&nbsp;F "AMERICAN INDIAN OR ALASKA NATIVE" "&nbsp;0 (  0.0%)"
#> 3 &nbsp;&nbsp;F "BLACK OR AFRICAN AMERICAN"        "&nbsp;5 (  5.8%)"
#> 4 &nbsp;&nbsp;F "WHITE"                            "48 ( 55.8%)"     
#> 5 &nbsp;&nbsp;M "AMERICAN INDIAN OR ALASKA NATIVE" "&nbsp;0 (  0.0%)"
#> 6 &nbsp;&nbsp;M "BLACK OR AFRICAN AMERICAN"        "&nbsp;3 (  3.5%)"
```

But now when we want to use this in a display, the `&nbsp;` characters
will show as leading whitespace within our HTML table. Note that you’ll
need to prevent escaping special characters for this to work, or the raw
text will display. In `kable()` you can use `escape=FALSE` do this.

``` r
collapse_row_labels(dat, row_label1, row_label2) %>% 
  select(row_label, row_label3, var1_Placebo) %>% 
  mutate(
    across(where(is.character), ~ replace_leading_whitespace(.))
  ) %>% 
  head() %>% 
  kable(escape=FALSE)
```

| row_label  | row_label3                       | var1_Placebo |
|:-----------|:---------------------------------|:-------------|
| Race n (%) |                                  |              |
|   F        | AMERICAN INDIAN OR ALASKA NATIVE |  0 ( 0.0%)   |
|   F        | BLACK OR AFRICAN AMERICAN        |  5 ( 5.8%)   |
|   F        | WHITE                            | 48 ( 55.8%)  |
|   M        | AMERICAN INDIAN OR ALASKA NATIVE |  0 ( 0.0%)   |
|   M        | BLACK OR AFRICAN AMERICAN        |  3 ( 3.5%)   |

## Conditional Formatting

In some circumstances, like
[`add_total_row()`](https://atorus-research.github.io/Tplyr/reference/add_total_row.md),
**Tplyr** lets you specify special formats separate from those in
[`set_format_strings()`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md).
But within the table body there’s no other way to set specific,
conditional formats based on the table data itself. To address this,
we’ve added the post-processing function
[`apply_conditional_format()`](https://atorus-research.github.io/Tplyr/reference/apply_conditional_format.md)
to allow you to set conditional formats on result cells.

[`apply_conditional_format()`](https://atorus-research.github.io/Tplyr/reference/apply_conditional_format.md)
operates on a character vector, so it can generally be used within the
context of
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
like any other character modifying function. It will make a 1:1
replacement of values, so it will return a character vector of equal
length. Let’s look at two examples.

``` r
string <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)")

apply_conditional_format(string, 2, x == 0, " 0        ", full_string=TRUE)
#> [1] " 0        " " 8  (9.3%)" "78 (90.7%)"

apply_conditional_format(string, 2, x == 0, "")
#> [1] " 0        " " 8  (9.3%)" "78 (90.7%)"
```

The two examples above achieve the same result, but they work slightly
differently. Let’s walk throug the syntax:

- The first parameter is the input character vector
- The second parameter refers to the format group. The format group is
  the index of the number within the result cell you’d like to target.
  So in the result `8 (9.3%)`, there are two format groups. The value of
  the first is 8. The value of the second is 9.3. This controls the
  number that we’ll use to establish our condition.
- The third parameter is the condition that will evaluate to establish
  if the conditional format is applied. Here, use the variable name `x`,
  which takes on the value of the from your chosen format group. This
  condition should be a filter condition, so it must return a boolean
  vector of TRUE/FALSE.
- The fourth parameter is the string value used as your replacement.

Finally, the last parameter is `full_string`, and this is the difference
between the first and second examples.
[`apply_conditional_format()`](https://atorus-research.github.io/Tplyr/reference/apply_conditional_format.md)
can do two types of replacements. The first example is a full string
replacement. In this case, whatever value you provide as the replacement
is used verbatim when the condition evaluates to `TRUE`. If this is set
to false, the **only the format group specified is replaced**. For more
context, let’s look at a third example.

``` r
apply_conditional_format(string, 2, x < 1, "(<1%)")
#> [1] " 0   (<1%)" " 8  (9.3%)" "78 (90.7%)"
```

In this example we target the percent field using format group 2, and
now our replacement text is `(<1%)`. If `full_string` uses its default
value of `FALSE`, only the format group specified is replaced.
[`apply_conditional_format()`](https://atorus-research.github.io/Tplyr/reference/apply_conditional_format.md)
establishes the width of the format group targeted, and the replacement
text will right align within that targetted portion of the string to
ensure that the alignment of the string is preserved.

For any example within a **Tplyr** result dataframe, let’s look at the
example dataset from earlier. Let’s say that we have `n (%)` values
within the first count layer to conditional format. Using some fancy
**dplyr** code, and
[`apply_conditional_format()`](https://atorus-research.github.io/Tplyr/reference/apply_conditional_format.md),
we can make it happen.

``` r
dat_new <- dat %>% 
  mutate(
    across(starts_with('var'),  # Apply to variables that start with `var`
    ~ if_else(
      ord_layer_index == 1,     # Target the count layer
      apply_conditional_format( 
        string = .,             # This is dplyr::across syntax
        format_group = 2,       # The percent field
        condition = x == 0,     # Our condition
        replacement = ""        # Replacement value
      ),
      .
      )
    )
  )

kable(dat_new)
```

| row_label1  | row_label2 | row_label3                       | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 | ord_layer_2 | ord_layer_3 |
|:------------|:-----------|:---------------------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|------------:|------------:|
| Race n (%)  | F          | AMERICAN INDIAN OR ALASKA NATIVE | 0            | 0                         | 0                        |               1 |           1 |           1 |           1 |
| Race n (%)  | F          | BLACK OR AFRICAN AMERICAN        | 5 ( 5.8%)    | 6 ( 7.1%)                 | 6 ( 7.1%)                |               1 |           1 |           1 |           2 |
| Race n (%)  | F          | WHITE                            | 48 ( 55.8%)  | 34 ( 40.5%)               | 44 ( 52.4%)              |               1 |           1 |           1 |           3 |
| Race n (%)  | M          | AMERICAN INDIAN OR ALASKA NATIVE | 0            | 1 ( 1.2%)                 | 0                        |               1 |           1 |           2 |           1 |
| Race n (%)  | M          | BLACK OR AFRICAN AMERICAN        | 3 ( 3.5%)    | 3 ( 3.6%)                 | 0                        |               1 |           1 |           2 |           2 |
| Race n (%)  | M          | WHITE                            | 30 ( 34.9%)  | 40 ( 47.6%)               | 34 ( 40.5%)              |               1 |           1 |           2 |           3 |
| Age (years) | F          | n                                | 53           | 40                        | 50                       |               2 |           1 |           1 |           1 |
| Age (years) | F          | Mean (SD)                        | 76.4 ( 8.73) | 74.7 ( 7.67)              | 75.7 ( 8.09)             |               2 |           1 |           1 |           2 |
| Age (years) | F          | Median                           | 78.0         | 76.0                      | 77.5                     |               2 |           1 |           1 |           3 |
| Age (years) | F          | Q1, Q3                           | 70.0, 84.0   | 72.0, 79.0                | 72.0, 81.0               |               2 |           1 |           1 |           4 |
| Age (years) | F          | Min, Max                         | 59, 89       | 56, 88                    | 54, 87                   |               2 |           1 |           1 |           5 |
| Age (years) | F          | Missing                          | 0            | 0                         | 0                        |               2 |           1 |           1 |           6 |
| Age (years) | M          | n                                | 33           | 44                        | 34                       |               2 |           1 |           2 |           1 |
| Age (years) | M          | Mean (SD)                        | 73.4 ( 8.15) | 74.1 ( 8.16)              | 75.6 ( 8.69)             |               2 |           1 |           2 |           2 |
| Age (years) | M          | Median                           | 74.0         | 77.0                      | 77.5                     |               2 |           1 |           2 |           3 |
| Age (years) | M          | Q1, Q3                           | 69.0, 80.0   | 69.0, 80.2                | 68.2, 82.0               |               2 |           1 |           2 |           4 |
| Age (years) | M          | Min, Max                         | 52, 85       | 56, 86                    | 51, 88                   |               2 |           1 |           2 |           5 |
| Age (years) | M          | Missing                          | 0            | 0                         | 0                        |               2 |           1 |           2 |           6 |

The syntax here gets a bit complicated, by using
[`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html)
we can apply the same function across each of the result variables,
which in this the variable names start with `var`. The function here is
using a [**purrr**](https://purrr.tidyverse.org/index.html) style
anonymous function for simplicity. There are a couple ways you can do
this in R. Referencing the documentation of
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html):

- A formula, e.g. `~ . + 1`. You must use `.` to refer to the first
  argument. Only recommended if you require backward compatibility with
  older versions of R.
- An anonymous function, e.g. `\(x) x + 1` or `function(x) x + 1`.

Within that function, we’re additionally using
[`if_else()`](https://dplyr.tidyverse.org/reference/if_else.html) to
only apply this function on the first layer by using the
`ord_layer_index` variable. All this together, we’re effectively running
the `apply_conditional_formats()` function only on the first layer, and
running it across all of the variables that start with `var`.

## Extracting a Format Group

When **Tplyr** outputs a result, using
[`set_format_strings()`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md)
and
[`f_str()`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
the result are concatenated together within a single result cell. For
example, within a count layer in a **Tplyr** table, there’s no way to
directly output the `n` and `pct` values in separate columns. If this is
a result you want, then in a post-processing step you can use the
function
[`str_extract_fmt_group()`](https://atorus-research.github.io/Tplyr/reference/str_extractors.md).

[`str_extract_fmt_group()`](https://atorus-research.github.io/Tplyr/reference/str_extractors.md)
allows you to reach within a result string and extract an individual
format group. Consider this example:

``` r
string <- c(" 5  (5.8%)", " 8  (9.3%)", "78 (90.7%)")

# Get the n counts
str_extract_fmt_group(string, 1)
#> [1] " 5" " 8" "78"

# Get the pct counts
str_extract_fmt_group(string, 2)
#> [1] "(5.8%)"  "(9.3%)"  "(90.7%)"
```

In the first call to
[`str_extract_fmt_group()`](https://atorus-research.github.io/Tplyr/reference/str_extractors.md),
we target the n counts. The first format group from each string is
extracted, preserving the allotted width of that portion of the string.
Similarly, in the second group we extract all the percent counts,
including the surround parentheses.

In practice,
[`str_extract_fmt_group()`](https://atorus-research.github.io/Tplyr/reference/str_extractors.md)
can then be used to separate format groups into their own columns.

``` r
dat <- tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    group_count(RACE)
  ) %>% 
  build()

dat %>% 
  mutate(
    across(starts_with('var'), 
           ~ str_extract_fmt_group(., 1),
           .names = "{.col}_n"),
    across(starts_with('var'), 
           ~ str_extract_fmt_group(., 2),
           .names = "{.col}_pct")
  ) %>% 
  select(row_label1, var1_Placebo_n, var1_Placebo_pct) %>% 
  kable()
```

| row_label1                       | var1_Placebo_n | var1_Placebo_pct |
|:---------------------------------|:---------------|:-----------------|
| AMERICAN INDIAN OR ALASKA NATIVE | 0              | ( 0.0%)          |
| BLACK OR AFRICAN AMERICAN        | 8              | ( 9.3%)          |
| WHITE                            | 78             | ( 90.7%)         |

For the sake of display, in this output I only select the Placebo
column, but note that we were able to dynamically separate out the `n`
results from the `pct` results. In some cases, functions such as
[`tidyr::separate()`](https://tidyr.tidyverse.org/reference/separate.html)
could also be used to get a result like this, but
[`str_extract_fmt_group()`](https://atorus-research.github.io/Tplyr/reference/str_extractors.md)
specifically targets the expected formatting of format groups, without
having to craft a specific expression that may get confused over things
like spaces in unexpected places.

## Highly Customized Sort Variables

In very much the same vein as
[`str_extract_fmt_group()`](https://atorus-research.github.io/Tplyr/reference/str_extractors.md),
the function
[`str_extract_num()`](https://atorus-research.github.io/Tplyr/reference/str_extractors.md)
allows you to target a format group and extract the number from within.
This can be used in any circumstance where you may want to pull a number
out of a result cell, but probably the best example of this would be for
a highly specific sort sequence.

Consider an adverse event table. In
[`vignette("sort")`](https://atorus-research.github.io/Tplyr/articles/sort.md)
we go over circumstances where you may want to sort by the descending
occurrence of a result. We’ve received questions about how to establish
tie breakers in this scenario, where ties should be broken sorting
descending occurrence of an adverse event within the high dose group,
then the low dose group, and finally the placebo group. **Tplyr**
doesn’t allow you to output these order variables by default, but
getting these numbers is quite simple with
[`str_extract_num()`](https://atorus-research.github.io/Tplyr/reference/str_extractors.md).
Let’s consider a simplified scenario

``` r
dat <- tplyr_table(tplyr_adae, TRTA) %>% 
  set_pop_data(tplyr_adsl) %>% 
  set_pop_treat_var(TRT01A) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_format_strings(f_str("xx (XX.x%) [A]", distinct_n, distinct_pct, n))  %>% 
      set_distinct_by(USUBJID)
  ) %>% 
  build()

dat %>% 
  head() %>% 
  kable()
```

| row_label1         | var1_Placebo   | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:-------------------|:---------------|:--------------------------|:-------------------------|----------------:|------------:|
| ACTINIC KERATOSIS  | 0 (0.0%) \[0\] | 1 (1.2%) \[1\]            | 0 (0.0%) \[0\]           |               1 |           1 |
| ALOPECIA           | 1 (1.2%) \[1\] | 0 (0.0%) \[0\]            | 0 (0.0%) \[0\]           |               1 |           2 |
| BLISTER            | 0 (0.0%) \[0\] | 1 (1.2%) \[2\]            | 5 (6.0%) \[8\]           |               1 |           3 |
| COLD SWEAT         | 1 (1.2%) \[3\] | 0 (0.0%) \[0\]            | 0 (0.0%) \[0\]           |               1 |           4 |
| DERMATITIS ATOPIC  | 1 (1.2%) \[1\] | 0 (0.0%) \[0\]            | 0 (0.0%) \[0\]           |               1 |           5 |
| DERMATITIS CONTACT | 0 (0.0%) \[0\] | 0 (0.0%) \[0\]            | 1 (1.2%) \[2\]           |               1 |           6 |

Given this data, let’s say we want to sort by descending occurrence of
the event, using the number of subjects. That would be the first format
group. And then we want to sort using high dose, then low dose, then
placebo. Let’s create the order variables.

``` r
dat_ord <- dat  %>% 
  mutate(
    across(starts_with('var1'),
           ~str_extract_num(., 1),
           .names = "{.col}_ord")
  ) 

dat_ord %>% 
  head() %>% 
  select(row_label1, matches('^var.*ord$'))
#> # A tibble: 6 × 4
#>   row_label1      var1_Placebo_ord var1_Xanomeline High…¹ var1_Xanomeline Low …²
#>   <chr>                      <dbl>                  <dbl>                  <dbl>
#> 1 ACTINIC KERATO…                0                      1                      0
#> 2 ALOPECIA                       1                      0                      0
#> 3 BLISTER                        0                      1                      5
#> 4 COLD SWEAT                     1                      0                      0
#> 5 DERMATITIS ATO…                1                      0                      0
#> 6 DERMATITIS CON…                0                      0                      1
#> # ℹ abbreviated names: ¹​`var1_Xanomeline High Dose_ord`,
#> #   ²​`var1_Xanomeline Low Dose_ord`
```

Now we effectively have additional order variables necessary to do the
sort sequence desired.

## External Data Formatting

The last post processing function worth mentioning isn’t necessarily
meant for post-processing data from **Tplyr** itself. We understand that
**Tplyr** can’t produce every single summary you’d need for a clinical
trial - and we never intended it to be able to do this. But we built
**Tplyr** to try to work effectively with other packages and tools.
**Tplyr**’s string formatting tools work quite well, so we’ve
externalized this capability using the function
[`apply_formats()`](https://atorus-research.github.io/Tplyr/reference/apply_formats.md).

As a basic example, let’s look at the `mtcars` data.

``` r
mtcars %>% 
  mutate(
    new_column = apply_formats("xx (xx.x)", gear, mpg)
  ) %>% 
  select(gear, mpg, new_column) %>% 
  head() %>% 
  kable()
```

|                   | gear |  mpg | new_column |
|:------------------|-----:|-----:|:-----------|
| Mazda RX4         |    4 | 21.0 | 4 (21.0)   |
| Mazda RX4 Wag     |    4 | 21.0 | 4 (21.0)   |
| Datsun 710        |    4 | 22.8 | 4 (22.8)   |
| Hornet 4 Drive    |    3 | 21.4 | 3 (21.4)   |
| Hornet Sportabout |    3 | 18.7 | 3 (18.7)   |
| Valiant           |    3 | 18.1 | 3 (18.1)   |

Here we were able to leverage the string formatting available in
[`f_str()`](https://atorus-research.github.io/Tplyr/reference/f_str.md),
but apply it generically in another data frame within
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).
This allows you to format other data, outside of **Tplyr**, but still
bring some of the quality of life that Tplyr has to offer.
