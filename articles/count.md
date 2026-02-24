# Count Layers

At the surface, counting sounds pretty simple, right? You just want to
know how many occurrences of something there are. Well - unfortunately,
it’s not that easy. And in clinical reports, there’s quite a bit of
nuance that goes into the different types of frequency tables that need
to be created. Fortunately, we’ve added a good bit of flexibility into
[`group_count()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md)
to help you get what you need when creating these reports, whether
you’re creating a demographics table, adverse events, or lab results.

## A Simple Example

Let’s start with a basic example. This table demonstrates the
distribution of subject disposition across treatment groups.
Additionally, we’re sorting by descending total occurrences using the
“Total” group.

``` r
t <- tplyr_table(tplyr_adsl, TRT01P, where = SAFFL == "Y") %>%
  add_total_group() %>%
  add_treat_grps(Treated = c("Xanomeline Low Dose", "Xanomeline High Dose")) %>%
  add_layer(
    group_count(DCDECOD) %>%
      set_order_count_method("bycount") %>%
      set_ordering_cols(Total)
  ) %>%
  build() %>%
  arrange(desc(ord_layer_1)) %>%
  select(starts_with("row"), var1_Placebo, `var1_Xanomeline Low Dose`,
         `var1_Xanomeline High Dose`, var1_Treated, var1_Total)

kable(t)
```

| row_label1                  | var1_Placebo | var1_Xanomeline Low Dose | var1_Xanomeline High Dose | var1_Treated | var1_Total   |
|:----------------------------|:-------------|:-------------------------|:--------------------------|:-------------|:-------------|
| COMPLETED                   | 58 ( 67.4%)  | 25 ( 29.8%)              | 27 ( 32.1%)               | 52 ( 31.0%)  | 110 ( 43.3%) |
| ADVERSE EVENT               | 8 ( 9.3%)    | 44 ( 52.4%)              | 40 ( 47.6%)               | 84 ( 50.0%)  | 92 ( 36.2%)  |
| WITHDRAWAL BY SUBJECT       | 9 ( 10.5%)   | 10 ( 11.9%)              | 8 ( 9.5%)                 | 18 ( 10.7%)  | 27 ( 10.6%)  |
| STUDY TERMINATED BY SPONSOR | 2 ( 2.3%)    | 2 ( 2.4%)                | 3 ( 3.6%)                 | 5 ( 3.0%)    | 7 ( 2.8%)    |
| PROTOCOL VIOLATION          | 2 ( 2.3%)    | 1 ( 1.2%)                | 3 ( 3.6%)                 | 4 ( 2.4%)    | 6 ( 2.4%)    |
| LACK OF EFFICACY            | 3 ( 3.5%)    | 0 ( 0.0%)                | 1 ( 1.2%)                 | 1 ( 0.6%)    | 4 ( 1.6%)    |
| DEATH                       | 2 ( 2.3%)    | 1 ( 1.2%)                | 0 ( 0.0%)                 | 1 ( 0.6%)    | 3 ( 1.2%)    |
| PHYSICIAN DECISION          | 1 ( 1.2%)    | 0 ( 0.0%)                | 2 ( 2.4%)                 | 2 ( 1.2%)    | 3 ( 1.2%)    |
| LOST TO FOLLOW-UP           | 1 ( 1.2%)    | 1 ( 1.2%)                | 0 ( 0.0%)                 | 1 ( 0.6%)    | 2 ( 0.8%)    |

## Distinct Versus Event Counts

Another exceptionally important consideration within count layers is
whether you should be using distinct counts, non-distinct counts, or
some combination of both. Adverse event tables are a perfect example.
Often, you’re concerned about how many subjects had an adverse event in
particular instead of just the number of occurrences of that adverse
event. Similarly, the number occurrences of an event isn’t necessarily
relevant when compared to the total number of adverse events that
occurred. For this reason, what you likely want to look at is instead
the number of subjects who experienced an event compared to the total
number of subjects in that treatment group.

**Tplyr** allows you to focus on these distinct counts and distinct
percents within some grouping variable, like subject. Additionally, you
can mix and match with the distinct counts with non-distinct counts in
the same row too. The
[`set_distinct_by()`](https://atorus-research.github.io/Tplyr/reference/set_distinct_by.md)
function sets the variables used to calculate the distinct occurrences
of some value using the specified `distinct_by` variables.

``` r
t <- tplyr_table(tplyr_adae, TRTA) %>%
  add_layer(
    group_count(AEDECOD) %>%
      set_distinct_by(USUBJID) %>%
      set_format_strings(f_str("xxx (xx.xx%) [xxx]", distinct_n, distinct_pct, n))
  ) %>%
  build() %>%
  head()

kable(t)
```

| row_label1         | var1_Placebo      | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:-------------------|:------------------|:--------------------------|:-------------------------|----------------:|------------:|
| ACTINIC KERATOSIS  | 0 ( 0.00%) \[ 0\] | 1 ( 2.38%) \[ 1\]         | 0 ( 0.00%) \[ 0\]        |               1 |           1 |
| ALOPECIA           | 1 ( 4.76%) \[ 1\] | 0 ( 0.00%) \[ 0\]         | 0 ( 0.00%) \[ 0\]        |               1 |           2 |
| BLISTER            | 0 ( 0.00%) \[ 0\] | 1 ( 2.38%) \[ 2\]         | 5 (11.90%) \[ 8\]        |               1 |           3 |
| COLD SWEAT         | 1 ( 4.76%) \[ 3\] | 0 ( 0.00%) \[ 0\]         | 0 ( 0.00%) \[ 0\]        |               1 |           4 |
| DERMATITIS ATOPIC  | 1 ( 4.76%) \[ 1\] | 0 ( 0.00%) \[ 0\]         | 0 ( 0.00%) \[ 0\]        |               1 |           5 |
| DERMATITIS CONTACT | 0 ( 0.00%) \[ 0\] | 0 ( 0.00%) \[ 0\]         | 1 ( 2.38%) \[ 2\]        |               1 |           6 |

You may have seen tables before like the one above. This display shows
the number of subjects who experienced an adverse event, the percentage
of subjects within the given treatment group who experienced that event,
and then the total number of occurrences of that event. Using
[`set_distinct_by()`](https://atorus-research.github.io/Tplyr/reference/set_distinct_by.md)
triggered the derivation of `distinct_n` and `distinct_pct` in addition
to the `n` and `pct` created within `group_count`. The display of the
values is then controlled by the
[`f_str()`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
call in
[`set_format_strings()`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md).

An additional option for formatting the numbers above would be using
‘parenthesis hugging’. To trigger this, on the integer side of a number
use a capital ‘X’ or a capital ‘A’. For example:

``` r
t <- tplyr_table(tplyr_adae, TRTA) %>%
  add_layer(
    group_count(AEDECOD) %>%
      set_distinct_by(USUBJID) %>%
      set_format_strings(f_str("xxx (XXX.xx%) [A]", distinct_n, distinct_pct, n))
  ) %>%
  build() %>%
  head() %>% 
  select(row_label1, `var1_Xanomeline Low Dose`)

t
#> # A tibble: 6 × 2
#>   row_label1         `var1_Xanomeline Low Dose`
#>   <chr>              <chr>                     
#> 1 ACTINIC KERATOSIS  "  0   (0.00%)  [0]"      
#> 2 ALOPECIA           "  0   (0.00%)  [0]"      
#> 3 BLISTER            "  5  (11.90%)  [8]"      
#> 4 COLD SWEAT         "  0   (0.00%)  [0]"      
#> 5 DERMATITIS ATOPIC  "  0   (0.00%)  [0]"      
#> 6 DERMATITIS CONTACT "  1   (2.38%)  [2]"
```

As can be seen above, when using parenthesis hugging, the width of a
specified format group is preserved, but the preceding character (or
characters) to the left of the ‘X’ or ‘A’ is pulled to the right to
‘hug’ the specified number.

## Nested Count Summaries

Certain summary tables present counts within groups. One example could
be in a disposition table where a disposition reason of “Other”
summarizes what those other reasons were. A very common example is an
Adverse Event table that displays counts for body systems, and then the
events within those body systems. This is again a nuanced situation -
there are two variables being summarized: The body system counts, and
the advert event counts.

One way to approach this would be creating two summaries. One
summarizing the body system, and another summarizing the preferred terms
by body system, and then merging the two together. But we don’t want you
to have to do that. Instead, we handle this complexity for you. This is
done in
[`group_count()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md)
by submitting two target variables with
[`dplyr::vars()`](https://dplyr.tidyverse.org/reference/vars.html). The
first variable should be your grouping variable that you want
summarized, which we refer to as the “Outside” variable, and the second
should have the narrower scope, which we call the “Inside” variable.

The example below demonstrates how to do a nested summary. Look at the
first row - here `row_label1` and `row_label2` are both “CARDIAC
DISORDERS”. This line is the summary for `AEBODSYS.` In the rows below
that, `row_label1` continues on with the value “CARDIAC DISORDERS”, but
`row_label2` changes. These are the summaries for `AEDECOD`.

``` r
tplyr_table(tplyr_adae, TRTA) %>%
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD))
  ) %>%
  build() %>%
  head() %>% 
  kable()
```

| row_label1                             | row_label2                             | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:---------------------------------------|:---------------------------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|------------:|
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | SKIN AND SUBCUTANEOUS TISSUE DISORDERS | 47 (100.0%)  | 111 (100.0%)              | 118 (100.0%)             |               1 |           1 |         Inf |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | ACTINIC KERATOSIS                      | 0 ( 0.0%)    | 1 ( 0.9%)                 | 0 ( 0.0%)                |               1 |           1 |           1 |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | ALOPECIA                               | 1 ( 2.1%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           1 |           2 |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | BLISTER                                | 0 ( 0.0%)    | 2 ( 1.8%)                 | 8 ( 6.8%)                |               1 |           1 |           3 |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | COLD SWEAT                             | 3 ( 6.4%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           1 |           4 |
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | DERMATITIS ATOPIC                      | 1 ( 2.1%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           1 |           5 |

This accomplishes what we needed, but it’s not exactly the presentation
you might hope for. We have a solution for this as well.

``` r
tplyr_table(tplyr_adae, TRTA) %>%
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>% 
      set_nest_count(TRUE) %>% 
      set_indentation("--->")
  ) %>%
  build() %>%
  head() %>% 
  kable()
```

| row_label1                             | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:---------------------------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|------------:|
| SKIN AND SUBCUTANEOUS TISSUE DISORDERS | 47 (100.0%)  | 111 (100.0%)              | 118 (100.0%)             |               1 |           1 |         Inf |
| —\>ACTINIC KERATOSIS                   | 0 ( 0.0%)    | 1 ( 0.9%)                 | 0 ( 0.0%)                |               1 |           1 |           1 |
| —\>ALOPECIA                            | 1 ( 2.1%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           1 |           2 |
| —\>BLISTER                             | 0 ( 0.0%)    | 2 ( 1.8%)                 | 8 ( 6.8%)                |               1 |           1 |           3 |
| —\>COLD SWEAT                          | 3 ( 6.4%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           1 |           4 |
| —\>DERMATITIS ATOPIC                   | 1 ( 2.1%)    | 0 ( 0.0%)                 | 0 ( 0.0%)                |               1 |           1 |           5 |

By using
[`set_nest_count()`](https://atorus-research.github.io/Tplyr/reference/set_nest_count.md),
this triggers **Tplyr** to drop row_label1, and indent all of the
AEDECOD values within row_label2. The columns are renamed appropriately
as well. The default indentation used will be 3 spaces, but as you can
see here - you can set the indentation however you like. This let’s you
use tab strings for different language-specific output types, stick with
spaces, indent wider or smaller - whatever you wish. All of the existing
order variables remain, so this has no impact on your ability to sort
the table.

There’s a lot more to counting! So be sure to check out our vignettes on
sorting, shift tables, and denominators.
