The Beta Release
================

Three weeks ago, we released the Alpha release of Tplyr. Since then,
we’ve been hard at work introducing some fresh features to make Tplyr
more flexible, more effective, and more useful as a tool for you and
your organization. Some of these additions are less alluring from the
surface, but introduce critical functionality needed for Tplyr to
accomplish the goals we’ve laid out.

The enhancements that we’re going to cover in this document are as
follows: - General updates: - Calculate your header N counts based on
the population dataset or the target dataset. The alpha release had an
option to set the population data but this wasn’t actually used anywhere
in the internals. - Use these header N counts as token replacements when
using the `add_column_headers` function. - Order variables are now added
to the built dataset to allow you to sort the output dataset as you wish
with numeric variables. - Count layer updates: - Optionally use the
population data N counts as denominators for percent calculation. - For
multi-level count summaries, nest the row label columns together to
present both row labels in a single column - You can now present both
distinct and non-distinct counts instead of one or the other - Sorting
options allow you to order results from the target variable values or
from derived counts within a specified column - Risk difference
calculations can now be added as additional columns, with flexible
options for presentation - Descriptive statistics layer updates: - The
custom summary functionality has been updated to apply to multi-variable
summaries, which results in an interface change - Automatic decimal
precision has been added to allow you to base the presentation on the
precision of the data as collected

Let’s dig into each of these updates one by one.

Count layer updates
-------------------

The general updates are best presented within the context of a count
layer. An important feature that was missing from the alpha release was
the capability to control the denominator being used in a summary. For
example, if you’re summarizing adverse events, not all subjects may have
been included adverse events dataset. A subject only exists in the
adverse events dataset if they had an adverse event. Therefore, if
you’re counting the total subjects for your denominator, the denominator
will be missing subjects. This is exactly why we included bindings for a
separate population dataset in the first place, and now the population
data are properly utilized.

Consider the following:

``` r

t <- tplyr_table(adae, TRTA, where= SAFFL == "Y") %>%
  set_pop_data(adsl) %>%  # Specify the population dataset
  set_pop_treat_var(TRT01A) %>% # Specify the treatment variable within the population
  add_layer(
    group_count(AEDECOD) %>% # Create the count layer
      set_distinct_by(USUBJID) %>% # Specify the variable to determine a distinct count by
      set_format_strings(f_str('xx (xx.x%)', distinct, distinct_pct)) # Set up the presentation
  )

invisible(build(t))

header_n(t)
#> # A tibble: 3 x 2
#>   TRT01A                   n
#>   <chr>                <dbl>
#> 1 Placebo                 86
#> 2 Xanomeline High Dose    84
#> 3 Xanomeline Low Dose     84
```

You can see that these counts are indeed coming from ADSL and not ADAE:

``` r
adae %>% 
  filter(SAFFL == "Y") %>%
  distinct(TRTA, USUBJID) %>% 
  count(TRTA)
#> # A tibble: 3 x 2
#>   TRTA                     n
#>   <chr>                <int>
#> 1 Placebo                 69
#> 2 Xanomeline High Dose    79
#> 3 Xanomeline Low Dose     77
```

Note also the use of the new `f_str` variable names `distinct` and
`distinct_pct`. We made these names separate from `n` and `pct` to make
it explicit when you want to use one type of count or the other. This
further allows you to create tables where you mix and match. Consider
this common AE table structure:

``` r

t <- tplyr_table(adae, TRTA, where= SAFFL == "Y") %>%
  set_pop_data(adsl) %>%  # Specify the population dataset
  set_pop_treat_var(TRT01A) %>% # Specify the treatment variable within the population
  add_layer(
    group_count(AEDECOD) %>% # Create the count layer
      set_distinct_by(USUBJID) %>% # Specify the variable to determine a distinct count by
      set_format_strings(f_str('xx (xx.x%) [x]', distinct, distinct_pct, n)) # Set up the presentation - with event counts
  )

kable(head(build(t)))
```

| row\_label1          | var1\_Placebo   | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|
|:---------------------|:----------------|:---------------------------|:--------------------------|------------------:|--------------:|
| ABDOMINAL DISCOMFORT | 0 ( 0.0%) \[0\] | 1 ( 1.2%) \[1\]            | 0 ( 0.0%) \[0\]           |                  1|              1|
| ABDOMINAL PAIN       | 1 ( 1.2%) \[1\] | 1 ( 1.2%) \[2\]            | 3 ( 3.6%) \[3\]           |                  1|              2|
| ACROCHORDON EXCISION | 0 ( 0.0%) \[0\] | 1 ( 1.2%) \[1\]            | 0 ( 0.0%) \[0\]           |                  1|              3|
| ACTINIC KERATOSIS    | 0 ( 0.0%) \[0\] | 1 ( 1.2%) \[1\]            | 0 ( 0.0%) \[0\]           |                  1|              4|
| AGITATION            | 2 ( 2.3%) \[2\] | 1 ( 1.2%) \[1\]            | 2 ( 2.4%) \[2\]           |                  1|              5|
| ALCOHOL USE          | 0 ( 0.0%) \[0\] | 1 ( 1.2%) \[1\]            | 0 ( 0.0%) \[0\]           |                  1|              6|

In this table, we present the distinct counts by subject, the distinct
percent (i.e. number of subjects who experienced an adverse event within
the total treatment group, as determined by the population dataset), and
then the total number of events. When the population dataset is
specified, those N counts from each treatment group will be used within
the denominator for `distinct_pct`. Otherwise, the count within target
dataset (in this case, ADAE), of the subjects will be used for
`distinct_pct`.

In addition to population data updates, we’ve also enhanced the
capabilities of `add_column_headers` to work with the `header_n` values.
It’s extremely common to print the header\_n counts within your column
headers. Now, we’ve made that easy.

``` r
t <- tplyr_table(adae, TRTA, where= SAFFL == "Y") %>%
  set_pop_data(adsl) %>%  # Specify the population dataset
  set_pop_treat_var(TRT01A) %>% # Specify the treatment variable within the population
  set_pop_where(SAFFL == "Y") %>%
  add_layer(
    group_count(AEDECOD) %>% # Create the count layer
      set_distinct_by(USUBJID) %>% # Specify the variable to determine a distinct count by
      set_format_strings(f_str('xx (xx.x%)', distinct, distinct_pct)) # Set up the presentation
  )

build(t) %>% 
  select(-starts_with('ord')) %>% 
  add_column_headers(' | Placebo N=(**Placebo**) | Xan.Low N=(**`Xanomeline High Dose`**) | Xan.High N=(**`Xanomeline Low Dose`**)',
                     header_n(t)) %>% 
  head() %>% 
  kable()
```

| row\_label1          | var1\_Placebo  | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |
|:---------------------|:---------------|:---------------------------|:--------------------------|
|                      | Placebo N=(86) | Xan.Low N=(84)             | Xan.High N=(84)           |
| ABDOMINAL DISCOMFORT | 0 ( 0.0%)      | 1 ( 1.2%)                  | 0 ( 0.0%)                 |
| ABDOMINAL PAIN       | 1 ( 1.2%)      | 1 ( 1.2%)                  | 3 ( 3.6%)                 |
| ACROCHORDON EXCISION | 0 ( 0.0%)      | 1 ( 1.2%)                  | 0 ( 0.0%)                 |
| ACTINIC KERATOSIS    | 0 ( 0.0%)      | 1 ( 1.2%)                  | 0 ( 0.0%)                 |
| AGITATION            | 2 ( 2.3%)      | 1 ( 1.2%)                  | 2 ( 2.4%)                 |

After the table is built and you order your columns, using
`add_column_headers`, you can extract the header\_n’s from your
`tplyr_table` object using the `header_n` function. Using this as a
parameter to `add_column_headers`, the function will now use the names
and values from the `header_n` output as token replacements. Simply use
the names of the `header_n` vector as your token, surrounded by "\*\*"
on both sides. Those values will be replaced as you see in the output
above. Any named numeric vector will work in the `header_n` parameter of
`add_column_headers`, but the `header_n` function in Tplyr makes this
simple.

Sorting
-------

You may have noticed the addition of `ord` columns on the datasets
output so far. Tplyr now provides order columns built into the table
returned to you via the `build` function. This follows a few basic
principles:

-   Each layer will have its own index based on the order in which it
    was attached to the `tplyr_table` object.
-   `by` variables will be sorted based on:
    -   If there there is a ‘&lt;VAR&gt;N’ variable in the dataset
        (i.e. RACE &lt;-&gt; RACEN, ETHNIC &lt;-&gt; ETHNICN), then the
        N variable will used
    -   If no ‘&lt;VAR&gt;N’ variable exists, but the variable is a
        factor, the factor level order will be used
    -   Otherwise, alphanumeric sorting of the `by` variable will be
        used.

All layers will function similarly up until this point. The sorting of
results is where things differ based on the layer type. Descriptive
statistic layers are simple - the order in which the format strings are
presented controls the order in which the results will be displayed.
This means that if you enter the format strings in the order ‘n’, ‘Mean
(SD)’, ‘Median’ - then the rows will order to present the summaries in
that order.

Count layers are where things get more complicated. There are multiple
scenarios for how one might wish to sort a count table. You may want:

-   Alphanumeric sorting of the target variable
-   A pre-specified sort order, for things like a disposition table, or
    AEs of special interest.
-   To order based on the result of one of the summary count in
    particular, within a specific treatment group (i.e. descending
    number of events from the Xanomeline High treatment group).

Tplyr’s got your back! We support each of these different scenarios.
Let’s use count layers to explore how all of this works

``` r
t <- tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_count(RACE, by = SEX)
  ) %>% 
  add_layer(
    group_count(ETHNIC, by = SEX)
  )

build(t) %>%
  kable()
```

| row\_label1 | row\_label2                      | var1\_Placebo | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:------------|:---------------------------------|:--------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| F           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)     | 0 ( 0.0%)                  | 0 ( 0.0%)                 |                  1|              1|              1|
| F           | BLACK OR AFRICAN AMERICAN        | 5 ( 5.8%)     | 6 ( 7.1%)                  | 6 ( 7.1%)                 |                  1|              1|              2|
| F           | WHITE                            | 48 ( 55.8%)   | 34 ( 40.5%)                | 44 ( 52.4%)               |                  1|              1|              3|
| M           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)     | 1 ( 1.2%)                  | 0 ( 0.0%)                 |                  1|              2|              1|
| M           | BLACK OR AFRICAN AMERICAN        | 3 ( 3.5%)     | 3 ( 3.6%)                  | 0 ( 0.0%)                 |                  1|              2|              2|
| M           | WHITE                            | 30 ( 34.9%)   | 40 ( 47.6%)                | 34 ( 40.5%)               |                  1|              2|              3|
| F           | HISPANIC OR LATINO               | 2 ( 2.3%)     | 1 ( 1.2%)                  | 4 ( 4.8%)                 |                  2|              1|              1|
| F           | NOT HISPANIC OR LATINO           | 51 ( 59.3%)   | 39 ( 46.4%)                | 46 ( 54.8%)               |                  2|              1|              2|
| M           | HISPANIC OR LATINO               | 1 ( 1.2%)     | 2 ( 2.4%)                  | 2 ( 2.4%)                 |                  2|              2|              1|
| M           | NOT HISPANIC OR LATINO           | 32 ( 37.2%)   | 42 ( 50.0%)                | 32 ( 38.1%)               |                  2|              2|              2|

In this very simple example, you see two order columns:

-   ord\_layer\_index
    -   This indicates the layer order. RACE was added first, so the
        value is 1. ETHNIC was added second, so its layer value is 2.
-   ord\_layer\_1
    -   This ties back to the by variable SEX. It is indicated as 1 to
        relate back to row\_label1. No SEXN variable is in ADSL, so it
        sorts alphanumerically, with F coming before M.
-   order\_layer\_2
    -   This associates with the target variable, which is displayed in
        row\_label2. Note that alphanumeric sorting was using, as these
        orders do not actually coincide with the RACEN values in the
        data, and there’s no ETHNICN in the data. The target variable
        sort order is an area where more caution should be used, so we
        have helper functions to let you be more explicit in how the
        target variable sorting should be handled:

``` r

t <- tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_count(RACE, by = SEX) %>% 
      set_order_count_method('byvarn') # Specify to look for a RACEN for result sorting
  )

build(t) %>% 
  arrange(ord_layer_index, ord_layer_1, ord_layer_2) %>% # Sort using the Tplyr order variables
  kable()
```

| row\_label1 | row\_label2                      | var1\_Placebo | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:------------|:---------------------------------|:--------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| F           | WHITE                            | 48 ( 55.8%)   | 34 ( 40.5%)                | 44 ( 52.4%)               |                  1|              1|              1|
| F           | BLACK OR AFRICAN AMERICAN        | 5 ( 5.8%)     | 6 ( 7.1%)                  | 6 ( 7.1%)                 |                  1|              1|              2|
| F           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)     | 0 ( 0.0%)                  | 0 ( 0.0%)                 |                  1|              1|              6|
| M           | WHITE                            | 30 ( 34.9%)   | 40 ( 47.6%)                | 34 ( 40.5%)               |                  1|              2|              1|
| M           | BLACK OR AFRICAN AMERICAN        | 3 ( 3.5%)     | 3 ( 3.6%)                  | 0 ( 0.0%)                 |                  1|              2|              2|
| M           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)     | 1 ( 1.2%)                  | 0 ( 0.0%)                 |                  1|              2|              6|

RACEN already exists within ADSL. Using the `byvarn` method of sorting,
`ord_layer_2` is built by simply using the RACEN variable values. This
allows you to leverage common variable values already built into CDISC.
If you’d like to work in a more R like world, then converting your input
variables to ordered factors works as well:

``` r
# Create a ordered factor for RACE
adsl$RACE <- factor(adsl$RACE, c('BLACK OR AFRICAN AMERICAN', 'AMERICAN INDIAN OR ALASKA NATIVE', 'WHITE'))

t <- tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_count(RACE, by = SEX) %>% 
      set_order_count_method('byfactor') # Specify to use RACE's factor order for the Tplyr order variable
  )

build(t) %>% 
  arrange(ord_layer_index, ord_layer_1, ord_layer_2) %>% 
  kable()
```

| row\_label1 | row\_label2                      | var1\_Placebo | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:------------|:---------------------------------|:--------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| F           | BLACK OR AFRICAN AMERICAN        | 5 ( 5.8%)     | 6 ( 7.1%)                  | 6 ( 7.1%)                 |                  1|              1|              1|
| F           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)     | 0 ( 0.0%)                  | 0 ( 0.0%)                 |                  1|              1|              2|
| F           | WHITE                            | 48 ( 55.8%)   | 34 ( 40.5%)                | 44 ( 52.4%)               |                  1|              1|              3|
| M           | BLACK OR AFRICAN AMERICAN        | 3 ( 3.5%)     | 3 ( 3.6%)                  | 0 ( 0.0%)                 |                  1|              2|              1|
| M           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)     | 1 ( 1.2%)                  | 0 ( 0.0%)                 |                  1|              2|              2|
| M           | WHITE                            | 30 ( 34.9%)   | 40 ( 47.6%)                | 34 ( 40.5%)               |                  1|              2|              3|

And finally, let’s jump to an AE table for the last example - sorting
based on the calculated counts. Here, we need some more input from the
user to understand what they want:

-   What column do we want a number from
-   What number within the column should be used

Let’s take a look.

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID) %>% 
      set_format_strings(f_str('xx (xx%) [x]', distinct, pct, n)) %>% 
      set_order_count_method('bycount') %>% # Specify to use the resulting counts for sorting
      set_ordering_cols('Xanomeline High Dose') %>% # Use the counts from Xanomeline High Dose treatment group
      set_result_order_var(n) # Use the raw numeric value of the n counts
  )

build(t) %>% 
  head() %>% 
  kable()
```

| row\_label1          | var1\_Placebo | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|
|:---------------------|:--------------|:---------------------------|:--------------------------|------------------:|--------------:|
| ABDOMINAL DISCOMFORT | 0 ( 0%) \[0\] | 1 ( 0%) \[1\]              | 0 ( 0%) \[0\]             |                  1|              1|
| ABDOMINAL PAIN       | 1 ( 0%) \[1\] | 1 ( 0%) \[2\]              | 3 ( 1%) \[3\]             |                  1|              2|
| ACROCHORDON EXCISION | 0 ( 0%) \[0\] | 1 ( 0%) \[1\]              | 0 ( 0%) \[0\]             |                  1|              1|
| ACTINIC KERATOSIS    | 0 ( 0%) \[0\] | 1 ( 0%) \[1\]              | 0 ( 0%) \[0\]             |                  1|              1|
| AGITATION            | 2 ( 1%) \[2\] | 1 ( 0%) \[1\]              | 2 ( 0%) \[2\]             |                  1|              1|
| ALCOHOL USE          | 0 ( 0%) \[0\] | 1 ( 0%) \[1\]              | 0 ( 0%) \[0\]             |                  1|              1|

Take a look at `order_layer_1` and compare it to the values within
`var1_Xanomeline High Dose` in the third position. Tplyr pulled out the
raw numeric values, so you can use them to sort. Looking through each of
the methods:

-   First, you need to specify that you want to use the `bycount` method
    of sorting. This tells Tplyr that you’re looking for the raw numeric
    values.
-   Next, you need to specify which column from the result variables you
    want to use. If you use the `cols` argument, simply provide multiple
    values, first by using the treatment group value desired, and then
    the `cols` arguments in order.
-   Last, you need to specify which value from the formatted results you
    want to extract. If not specified, this will default to the first
    value. Also note, the raw numeric value will be used - not the
    string formatted value converted back to numeric.

Let’s look at one last situation - multi-level count summaries.

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>% # Now doing a multi-level summary
      set_distinct_by(USUBJID) %>% 
      set_format_strings(f_str('xx (xx%) [x]', distinct, pct, n)) %>% 
      set_order_count_method('bycount') %>% # Specify to use the resulting counts for sorting
      set_ordering_cols('Xanomeline High Dose') %>% # Use the counts from Xanomeline High Dose treatment group
      set_result_order_var(n) # Use the raw numeric value of the n counts
  )

build(t) %>% 
  head() %>% 
  kable()
```

| row\_label1                                          | var1\_Placebo   | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:-----------------------------------------------------|:----------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | 21 (16%) \[48\] | 40 (27%) \[124\]           | 47 (28%) \[120\]          |                  1|              1|            Inf|
| APPLICATION SITE BLEEDING                            | 0 ( 0%) \[0\]   | 0 ( 0%) \[0\]              | 1 ( 0%) \[1\]             |                  1|              1|              0|
| APPLICATION SITE DERMATITIS                          | 5 ( 3%) \[9\]   | 7 ( 3%) \[12\]             | 9 ( 3%) \[15\]            |                  1|              1|             12|
| APPLICATION SITE DESQUAMATION                        | 0 ( 0%) \[0\]   | 0 ( 0%) \[0\]              | 1 ( 0%) \[1\]             |                  1|              1|              0|
| APPLICATION SITE DISCHARGE                           | 0 ( 0%) \[0\]   | 1 ( 0%) \[1\]              | 0 ( 0%) \[0\]             |                  1|              1|              1|
| APPLICATION SITE DISCOLOURATION                      | 0 ( 0%) \[0\]   | 0 ( 0%) \[0\]              | 1 ( 0%) \[1\]             |                  1|              1|              0|

Tplyr can handle these as well, but this is an area we’re going to
improve in the next release. Currently, Tplyr defaults to using the
VARN/Factor/alphanumeric sorting method for the outer variable, AEBODSYS
(just like the `by` variables, starting with searching for a
&lt;VAR&gt;N variable, then to factor variable, last to alphanumeric).
For the inside variable, the specified sorting method is used. This
currently covers many situations just fine - but we’re going to add the
additional flexibility to allow count based sorting for both the inside
and outside variable.

Figuring out how we wanted to approach sorting was itself quite a large
task, as we constantly try to balance flexibility with complexity of
use. Implementing these order methods was an even larger task. Kudos to
Eli for getting this as far as we have!

While we’re on the subject of multi-level counts, we’ve also added some
enhanced capabilities for presenting these situations by offering
nesting - as you can see in the summary above. The second variable is
nested inside the first. You’re able to turn this setting off:

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>% 
      set_nest_count(FALSE) 
  ) 

build(t) %>% 
  head() 
#> # A tibble: 6 x 8
#> # Groups:   ord_layer_1 [1]
#>   row_label1   row_label2    var1_Placebo `var1_Xanomelin… `var1_Xanomelin… ord_layer_index ord_layer_1 ord_layer_2
#>   <chr>        <chr>         <chr>        <chr>            <chr>                      <int>       <dbl>       <dbl>
#> 1 GENERAL DIS… "GENERAL DIS… " 48 ( 15.9… "124 ( 27.3%)"   "120 ( 27.6%)"                 1           1         Inf
#> 2 GENERAL DIS… "\tAPPLICATI… "  0 (  0.0… "  0 (  0.0%)"   "  1 (  0.2%)"                 1           1           0
#> 3 GENERAL DIS… "\tAPPLICATI… "  9 (  3.0… " 12 (  2.6%)"   " 15 (  3.4%)"                 1           1           9
#> 4 GENERAL DIS… "\tAPPLICATI… "  0 (  0.0… "  0 (  0.0%)"   "  1 (  0.2%)"                 1           1           0
#> 5 GENERAL DIS… "\tAPPLICATI… "  0 (  0.0… "  1 (  0.2%)"   "  0 (  0.0%)"                 1           1           0
#> 6 GENERAL DIS… "\tAPPLICATI… "  0 (  0.0… "  0 (  0.0%)"   "  1 (  0.2%)"                 1           1           0
```

You can also change the character used to set the indentation, which
defaults to `\t` for a tab.

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD))
  ) %>% 
  set_indentation("--->")

build(t) %>% 
  head() %>% 
  kable()
```

| row\_label1                                          | var1\_Placebo | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:-----------------------------------------------------|:--------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | 48 ( 15.9%)   | 124 ( 27.3%)               | 120 ( 27.6%)              |                  1|              1|            Inf|
| —&gt;APPLICATION SITE BLEEDING                       | 0 ( 0.0%)     | 0 ( 0.0%)                  | 1 ( 0.2%)                 |                  1|              1|              0|
| —&gt;APPLICATION SITE DERMATITIS                     | 9 ( 3.0%)     | 12 ( 2.6%)                 | 15 ( 3.4%)                |                  1|              1|              9|
| —&gt;APPLICATION SITE DESQUAMATION                   | 0 ( 0.0%)     | 0 ( 0.0%)                  | 1 ( 0.2%)                 |                  1|              1|              0|
| —&gt;APPLICATION SITE DISCHARGE                      | 0 ( 0.0%)     | 1 ( 0.2%)                  | 0 ( 0.0%)                 |                  1|              1|              0|
| —&gt;APPLICATION SITE DISCOLOURATION                 | 0 ( 0.0%)     | 0 ( 0.0%)                  | 1 ( 0.2%)                 |                  1|              1|              0|

Risk difference
---------------

Tplyr does not support, nor do we intended to support, a wide array of
statistical methods. Our goal is rather to take your focus as an analyst
off the mundane summaries so you can focus on the interesting analysis.
That said, there are some things that are common enough that we feel
that it’s reasonable for us to include. So let’s take a look at risk
difference.

Our current implementation of risk difference is solely built on top of
the base R function `prop.test`. For any and all questions about this
method, please review the `prop.test` documentation within R.

Risk difference is built on top of count layers, as it’s a comparison of
proportions. To add risk difference into a count layer, you simply use
the function `add_risk_diff`. We made a large effort to make this flow
very naturally with the count layer construction, so let’s walk through
it step by step.

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by('USUBJID') %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo')
      )
  )

suppressWarnings(build(t)) %>% 
  head() %>% 
  kable()
```

| row\_label1          | var1\_Placebo | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index| rdiff\_Xanomeline High Dose\_Placebo | rdiff\_Xanomeline Low Dose\_Placebo |  ord\_layer\_1|
|:---------------------|:--------------|:---------------------------|:--------------------------|------------------:|:-------------------------------------|:------------------------------------|--------------:|
| ABDOMINAL DISCOMFORT | 0 ( 0.0%)     | 1 ( 0.2%)                  | 0 ( 0.0%)                 |                  1|                                      |                                     |              1|
| ABDOMINAL PAIN       | 1 ( 0.3%)     | 2 ( 0.4%)                  | 3 ( 0.7%)                 |                  1| -0.001 (-0.010, 0.008)               | -0.001 (-0.010, 0.008)              |              2|
| ACROCHORDON EXCISION | 0 ( 0.0%)     | 1 ( 0.2%)                  | 0 ( 0.0%)                 |                  1|                                      |                                     |              3|
| ACTINIC KERATOSIS    | 0 ( 0.0%)     | 1 ( 0.2%)                  | 0 ( 0.0%)                 |                  1|                                      |                                     |              4|
| AGITATION            | 2 ( 0.7%)     | 1 ( 0.2%)                  | 2 ( 0.5%)                 |                  1| -0.001 (-0.010, 0.008)               | -0.001 (-0.010, 0.008)              |              5|
| ALCOHOL USE          | 0 ( 0.0%)     | 1 ( 0.2%)                  | 0 ( 0.0%)                 |                  1|                                      |                                     |              6|

Comparisons are specified with two-element character vectors. These are
simply your comparison group - the first element, and your reference
group - the second. This coincides with how you might see risk
difference specified in the header of your mock, where you’ll see
something like T1-Placebo. You can provide as many comparisons as you
want - the values specified in the comparison just need to be valid
treatment groups within your data. This works with any treatment group
built using `add_treat_group` or `add_total_group` as well.

The risk difference calculations are displayed in the `rdiff` columns.
There will be an `rdiff` column for every comparison that is made,
following the convention `rdiff_<comparison>_<reference>`.

Note the use of `suppressWarnings` - if the counts used in `prop.test`
are too low, you’ll get a warning that says “Chi-squared approximation
may be incorrect” for every time `prop.test` is run with counts that are
too low… This could happen a lot, but the warning is perfectly valid.

The default values that are displayed will be:

-   The difference
-   95% confidence interval low
-   95% confidence interval high

You have a good bit of control over these values though, and this can be
controlled in the same way you format the count summaries - using
`set_format_strings`.

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by('USUBJID') %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo')
      ) %>% 
      set_format_strings(
        'n_counts' = f_str('xx (xx.x) [x]', distinct, distinct_pct, n),
        'riskdiff' = f_str('xx.xxx, xx.xxx, xx.xxx, xx.xxx, xx.xxx', comp, ref, dif, low, high)
      )
  )

suppressWarnings(build(t)) %>% 
  head() %>% 
  kable()
```

| row\_label1          | var1\_Placebo  | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index| rdiff\_Xanomeline High Dose\_Placebo | rdiff\_Xanomeline Low Dose\_Placebo |  ord\_layer\_1|
|:---------------------|:---------------|:---------------------------|:--------------------------|------------------:|:-------------------------------------|:------------------------------------|--------------:|
| ABDOMINAL DISCOMFORT | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              1|
| ABDOMINAL PAIN       | 1 ( 0.3) \[1\] | 1 ( 0.2) \[2\]             | 1 ( 0.2) \[3\]            |                  1| 0.002, 0.003, -0.001, -0.010, 0.008  | 0.002, 0.003, -0.001, -0.010, 0.008 |              2|
| ACROCHORDON EXCISION | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              3|
| ACTINIC KERATOSIS    | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              4|
| AGITATION            | 1 ( 0.3) \[2\] | 1 ( 0.2) \[1\]             | 1 ( 0.2) \[2\]            |                  1| 0.002, 0.003, -0.001, -0.010, 0.008  | 0.002, 0.003, -0.001, -0.010, 0.008 |              5|
| ALCOHOL USE          | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              6|

Take a look at the `rdiff` columns now - you’ll see they have 5 values.
These are:

-   The comparison proportion (i.e. the estimate\[1\] output from a
    `prop.test` object)
-   The reference proportion (i.e. the estimate\[2\] output from a
    `prop.test` object)
-   The difference (i.e. estimate\[1\] - estimate\[2\])
-   The lower end of the confidence interval
-   The upper end of the confidence interval

You have the same control over the formatting of the display of these
values here as you do with the count summaries. Taking things a step
further, you can also pass forward arguments to `prop.test` using a
named list and the `args` argument in `add_risk_diff`. This wasn’t done
using the ellipsis (i.e. `...`) like typical R functions because it’s
already used to capture a varying number of comparisons, but it’s not
much more difficult to use:

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by('USUBJID') %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo'),
        args = list(conf.level=0.95, alternative='less', correct=FALSE)
      ) %>% 
      set_format_strings(
        'n_counts' = f_str('xx (xx.x) [x]', distinct, distinct_pct, n),
        'riskdiff' = f_str('xx.xxx, xx.xxx, xx.xxx, xx.xxx, xx.xxx', comp, ref, dif, low, high)
      )
  )

suppressWarnings(build(t)) %>% 
  head() %>% 
  kable()
```

| row\_label1          | var1\_Placebo  | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index| rdiff\_Xanomeline High Dose\_Placebo | rdiff\_Xanomeline Low Dose\_Placebo |  ord\_layer\_1|
|:---------------------|:---------------|:---------------------------|:--------------------------|------------------:|:-------------------------------------|:------------------------------------|--------------:|
| ABDOMINAL DISCOMFORT | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              1|
| ABDOMINAL PAIN       | 1 ( 0.3) \[1\] | 1 ( 0.2) \[2\]             | 1 ( 0.2) \[3\]            |                  1| 0.002, 0.003, -0.001, -1.000, 0.005  | 0.002, 0.003, -0.001, -1.000, 0.006 |              2|
| ACROCHORDON EXCISION | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              3|
| ACTINIC KERATOSIS    | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              4|
| AGITATION            | 1 ( 0.3) \[2\] | 1 ( 0.2) \[1\]             | 1 ( 0.2) \[2\]            |                  1| 0.002, 0.003, -0.001, -1.000, 0.005  | 0.002, 0.003, -0.001, -1.000, 0.006 |              5|
| ALCOHOL USE          | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              6|

One more note - the default of `add_risk_diff` works on the distinct
counts available within the count summary. If for whatever reason you’d
like to run risk difference on the non-distinct counts, switch the
`distinct` argument to FALSE. `add_risk_diff` also will function on
multi-level summaries no different than single level, so no concerns
there either.

Descriptive Statistic Layer Updates
-----------------------------------

You can see that there was a lot of attention given to counting over the
past three weeks - but descriptive statistics weren’t neglected either.
Let’s start simple - custom summaries no work properly on multi-variable
summaries:

``` r
tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_desc(vars(AGE, HEIGHTBL), by = "Sepal Length") %>%
      set_custom_summaries(
        geometric_mean = exp(sum(log(.var[.var > 0]), na.rm=TRUE) / length(.var))
      ) %>%
      set_format_strings(
        'Geometric Mean (SD)' = f_str('xx.xx (xx.xxx)', geometric_mean, sd)
      )
  ) %>% 
  build() %>% 
  kable()
```

| row\_label1  | row\_label2         | var1\_Placebo  | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose | var2\_Placebo   | var2\_Xanomeline High Dose | var2\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_2|
|:-------------|:--------------------|:---------------|:---------------------------|:--------------------------|:----------------|:---------------------------|:--------------------------|------------------:|--------------:|
| Sepal Length | Geometric Mean (SD) | 74.70 ( 8.590) | 73.94 ( 7.886)             | 75.18 ( 8.286)            | 162.17 (11.522) | 165.51 (10.131)            | 163.11 (10.419)           |                  1|              1|

Not much more to it! Just use `.var` instead of the a distinct variable
name.

### Auto Precision

The more interesting development for descriptive statistics was the
addition of auto-precision. Auto-precision allows you to format your
numeric summaries based on the precision of the data collected.
Particularly when working with labs results, different tests may have
difference necessities for decimal precision depending on the numeric
range of the tests, the units the data are collected in, etc. So it is
common practice to vary the precision of the data being presented based
on the data collected. Furthermore, depending on the summary being
presented, you may wish to increase the precision further. For example,
you may want the mean to be at collected precision +1 decimal place, for
standard deviation +2.

Tplyr now handles these cases, and in Tplyr style, it’s intuitive and
easy to control. This has all been built into the format strings,
because a natural place to specify your desired format is where you
specify how you want your data presented. Now - if you wish to use
auto-precision, use `a` instead of `x` when creating your summaries.
Note that only one `a` is needed. To use increased precision, use `a+n`
where `n` is the number of additional spaces you wish to add.

``` r

tplyr_table(adlb, TRTA) %>% 
  add_layer(
    group_desc(AVAL, by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd)
      )
  ) %>% 
  build() %>% 
  kable()
```

| row\_label1 | row\_label2 | var1\_Placebo         | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:------------|:------------|:----------------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| ALB         | Mean (SD)   | 39.5 ( 3.22)          | 39.7 ( 2.61)               | 39.1 ( 2.79)              |                  1|              1|              1|
| ALP         | Mean (SD)   | 78.2 ( 66.55)         | 71.6 ( 43.11)              | 71.9 ( 22.01)             |                  1|              2|              2|
| ALT         | Mean (SD)   | 17.7 ( 11.89)         | 20.5 ( 10.08)              | 18.2 ( 8.93)              |                  1|              3|              3|
| AST         | Mean (SD)   | 23.5 ( 13.10)         | 23.5 ( 7.26)               | 23.0 ( 8.94)              |                  1|              4|              4|
| BILI        | Mean (SD)   | 10.485 ( 9.7213)      | 11.156 ( 5.5878)           | 9.417 ( 4.3167)           |                  1|              5|              5|
| BUN         | Mean (SD)   | 5.8354 ( 1.48851)     | 5.8530 ( 1.97958)          | 6.3194 ( 1.84470)         |                  1|              6|              6|
| CA          | Mean (SD)   | 2.283812 (0.0934973)  | 2.280342 (0.0994236)       | 2.286532 (0.1075204)      |                  1|              7|              7|
| CHOL        | Mean (SD)   | 5.566333 ( 0.9900913) | 5.510214 ( 0.9425105)      | 5.449229 ( 0.9427905)     |                  1|              8|              8|
| CK          | Mean (SD)   | 97.9 ( 99.81)         | 98.8 ( 92.46)              | 95.5 ( 64.31)             |                  1|              9|              9|
| CL          | Mean (SD)   | 105.7 ( 3.26)         | 105.2 ( 3.27)              | 105.7 ( 3.05)             |                  1|             10|             10|
| CREAT       | Mean (SD)   | 99.374 ( 17.2946)     | 104.902 ( 20.1506)         | 103.441 ( 19.6494)        |                  1|             11|             11|
| GGT         | Mean (SD)   | 24.0 ( 41.34)         | 23.7 ( 19.12)              | 22.6 ( 15.10)             |                  1|             12|             12|
| GLUC        | Mean (SD)   | 5.686409 ( 1.8253883) | 5.878607 ( 2.2279225)      | 5.507944 ( 1.6561578)     |                  1|             13|             13|
| K           | Mean (SD)   | 4.24 (0.403)          | 4.25 (0.364)               | 4.28 (0.390)              |                  1|             14|             14|
| PHOS        | Mean (SD)   | 1.156331 (0.1540930)  | 1.166697 (0.1637434)       | 1.145270 (0.1533291)      |                  1|             15|             15|
| PROT        | Mean (SD)   | 70.0 ( 4.58)          | 70.2 ( 4.23)               | 69.5 ( 4.63)              |                  1|             16|             16|
| SODIUM      | Mean (SD)   | 140.8 ( 2.66)         | 140.3 ( 3.08)              | 140.6 ( 2.62)             |                  1|             17|             17|
| URATE       | Mean (SD)   | 288.3687 ( 70.64847)  | 295.0609 ( 79.47892)       | 295.3140 ( 72.15064)      |                  1|             18|             18|

As you can see, the decimal precision is now varying depending on the
test being performed. Notice that both the integer and the decimal side
of each number fluctuate as well. Tpylr collects both the integer and
decimal precision, and you can specify both separately. For example, you
could use `x`’s to specify a default number of spaces for your integers
that are used consistently across by variables, but vary the decimal
precision based on collected data. You can also increment the number of
spaces for both integer and decimal separately.

But - this is kind of ugly, isn’t it? Do we really need all 5 decimal
places collected for CA? For this reason, you’re able to set a cap on
the precision that’s displayed:

``` r
tplyr_table(adlb, TRTA) %>% 
  add_layer(
    group_desc(AVAL, by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd),
        cap = c(int=3, dec=2)
      )
  ) %>% 
  build() %>% 
  head() %>% 
  kable()
```

| row\_label1 | row\_label2 | var1\_Placebo    | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:------------|:------------|:-----------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| ALB         | Mean (SD)   | 39.5 ( 3.22)     | 39.7 ( 2.61)               | 39.1 ( 2.79)              |                  1|              1|              1|
| ALP         | Mean (SD)   | 78.2 ( 66.55)    | 71.6 ( 43.11)              | 71.9 ( 22.01)             |                  1|              2|              2|
| ALT         | Mean (SD)   | 17.7 ( 11.89)    | 20.5 ( 10.08)              | 18.2 ( 8.93)              |                  1|              3|              3|
| AST         | Mean (SD)   | 23.5 ( 13.10)    | 23.5 ( 7.26)               | 23.0 ( 8.94)              |                  1|              4|              4|
| BILI        | Mean (SD)   | 10.485 ( 9.7213) | 11.156 ( 5.5878)           | 9.417 ( 4.3167)           |                  1|              5|              5|
| BUN         | Mean (SD)   | 5.835 ( 1.4885)  | 5.853 ( 1.9796)            | 6.319 ( 1.8447)           |                  1|              6|              6|

Now that looks better. The `cap` argument is part of
`set_format_strings`. You need to specify the integer and decimal caps
separately. Note that integer precision might not behave like you expect
- it doesn’t make sense to truncate an integer if it’s value is too
high, so if the integer exceeds the allotted space, then the length of
the string will increase and the full value will be displayed. But
values that are short enough will only pad to the capped number of
spaces. We plan to implement a warning in future releases if integers
exceed the set display space allocation.

This was a basic situation, but if you’re paying close attention, you
may have some questions. What if you have more by variables, like by
visit AND test. Do we then calculate precision by visit and test? What
if collected precision is different per visit and we don’t want that?
What about multiple summary variable? How do we determine precision
then? We have modifier functions for this:

``` r
tplyr_table(adlb, TRTA, where = SAFFL=='Y' & AVISIT != '') %>% 
  add_layer(
    group_desc(vars(AVAL, CHG, BASE), by = vars(AVISIT,PARAMCD)) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd),
        cap = c(int=3, dec=2)
      ) %>% 
      set_precision_on(AVAL) %>% 
      set_precision_by(PARAMCD)
  ) %>%
  build() %>% 
  head() %>% 
  kable()
```

| row\_label1 | row\_label2 | row\_label3 | var1\_Placebo   | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose | var2\_Placebo | var2\_Xanomeline High Dose | var2\_Xanomeline Low Dose | var3\_Placebo   | var3\_Xanomeline High Dose | var3\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|  ord\_layer\_3|
|:------------|:------------|:------------|:----------------|:---------------------------|:--------------------------|:--------------|:---------------------------|:--------------------------|:----------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|--------------:|
| Baseline    | ALB         | Mean (SD)   | 39.8 ( 2.81)    | 40.3 ( 2.84)               | 39.8 ( 2.56)              | ()            | ()                         | ()                        | 39.8 ( 2.81)    | 40.3 ( 2.84)               | 39.8 ( 2.56)              |                  1|              0|              1|              1|
| Baseline    | ALP         | Mean (SD)   | 77.7 ( 58.11)   | 71.0 ( 38.85)              | 73.3 ( 20.72)             | ()            | ()                         | ()                        | 77.7 ( 58.11)   | 71.0 ( 38.85)              | 73.3 ( 20.72)             |                  1|              0|              2|              2|
| Baseline    | ALT         | Mean (SD)   | 17.6 ( 9.22)    | 19.2 ( 10.05)              | 18.0 ( 8.72)              | ()            | ()                         | ()                        | 17.6 ( 9.22)    | 19.2 ( 10.05)              | 18.0 ( 8.72)              |                  1|              0|              3|              3|
| Baseline    | AST         | Mean (SD)   | 23.2 ( 7.50)    | 23.1 ( 6.61)               | 23.4 ( 8.24)              | ()            | ()                         | ()                        | 23.2 ( 7.50)    | 23.1 ( 6.61)               | 23.4 ( 8.24)              |                  1|              0|              4|              4|
| Baseline    | BILI        | Mean (SD)   | 9.703 ( 3.9645) | 11.034 ( 5.3512)           | 9.447 ( 4.0146)           | ()            | ()                         | ()                        | 9.703 ( 3.9645) | 11.034 ( 5.3512)           | 9.447 ( 4.0146)           |                  1|              0|              5|              5|
| Baseline    | BUN         | Mean (SD)   | 5.538 ( 1.3851) | 5.754 ( 1.8837)            | 6.374 ( 1.9691)           | ()            | ()                         | ()                        | 5.538 ( 1.3851) | 5.754 ( 1.8837)            | 6.374 ( 1.9691)           |                  1|              0|              6|              6|

Three variables are being summarized here - AVAL, CHG, and BASE. So
which should be used for precision? `set_precision_on` allows you to
specify this, where the `precision_on` variable must be one of the
variables within `target_var`. Similarly, `set_precision_by` changes the
`by` variables used to determine collected precision. If no
`precision_on` variable is specified, the first variable in `target_var`
is used. If not `precision_by` variables are specified, then the default
`by` variables are used.

Next Steps
==========

Our next release of Tplyr will be our first full release. This means
that:

-   We will be posting to CRAN
-   We’re going to include a full and indpendently tested UAT document
-   We’ll have a full suite of vignettes included to enhance our
    available documentation

The next wave of updates is mostly going to be tweaks and sugar, where
we will try to make sure that Tplyr is as user friendly and practical as
possible. In the mean time, if you have any comments, feedback, or find
some bugs - drop us an issue. We’d love to hear any feedback if you’ve
taken Tplyr for a test drive.
