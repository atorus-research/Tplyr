# A grammar of summary data for clinical reports

\`r lifecycle::badge("experimental")\`

## Details

'Tplyr' is a package dedicated to simplifying the data manipulation
necessary to create clinical reports. Clinical data summaries can often
be broken down into two factors - counting discrete variables (or
counting shifts in state), and descriptive statistics around a
continuous variable. Many of the reports that go into a clinical report
are made up of these two scenarios. By abstracting this process away,
'Tplyr' allows you to rapidly build these tables without worrying about
the underlying data manipulation.

'Tplyr' takes this process a few steps further by abstracting away most
of the programming that goes into proper presentation, which is where a
great deal of programming time is spent. For example, 'Tplyr' allows you
to easily control:

- **String formatting**:

  Different reports warrant different presentation of your strings.
  Programming this can get tedious, as you typically want to make sure
  that your decimals properly align. 'Tplyr' abstracts this process away
  and provides you with a simple interface to specify how you want your
  data presented

- **Treatment groups**:

  Need a total column? Need to group summaries of multiple treatments?
  'Tplyr' makes it simple to add additional treatment groups into your
  report

- **Denominators**:

  n (%) counts often vary based on the summary being performed. 'Tplyr'
  allows you to easily control what denominators are used based on a few
  common scenarios

- **Sorting**:

  Summarizing data is one thing, but ordering it for presentation. Tplyr
  automatically derives sorting variable to give you the data you need
  to order your table properly. This process is flexible so you can
  easily get what you want by leveraging your data or characteristics of
  R.

Another powerful aspect of 'Tplyr' are the objects themselves. 'Tplyr'
does more than format your data. Metadata about your table is kept under
the hood, and functions allow you to access information that you need.
For example, 'Tplyr' allows you to calculate and access the raw numeric
data of calculations as well, and easily pick out just the pieces of
information that you need.

Lastly, 'Tplyr' was built to be flexible, yet intuitive. A common
pitfall of building tools like this is over automation. By doing to
much, you end up not doing enough. 'Tplyr' aims to hit the sweet spot in
between. Additionally, we designed our function interfaces to be clean.
Modifier functions offer you flexibility when you need it, but defaults
can be set to keep the code concise. This allows you to quickly assemble
your table, and easily make changes where necessary.

## See also

Useful links:

- <https://atorus-research.github.io/Tplyr/>

- <https://github.com/atorus-research/Tplyr>

- Report bugs at <https://github.com/atorus-research/Tplyr/issues>

## Author

**Maintainer**: Mike Stackhouse <mike.stackhouse@atorusresearch.com>
([ORCID](https://orcid.org/0000-0001-6030-723X))

Authors:

- Eli Miller <Eli.Miller@AtorusResearch.com>
  ([ORCID](https://orcid.org/0000-0002-2127-9456))

- Ashley Tarasiewicz <Ashley.Tarasiewicz@atorusresearch.com>

Other contributors:

- Nathan Kosiba <Nathan.Kosiba@atorusresearch.com>
  ([ORCID](https://orcid.org/0000-0001-5359-4234)) \[contributor\]

- Sadchla Mascary <sadchla.mascary@atorusresearch.com> \[contributor\]

- Andrew Bates <andrew.bates@atorusresearch.com> \[contributor\]

- Shiyu Chen <shiyu.chen@atorusresearch.com> \[contributor\]

- Oleksii Mikryukov <alex.mikryukov@atorusresearch.com> \[contributor\]

- Atorus Research LLC \[copyright holder\]

## Examples

``` r
# Load in pipe
library(magrittr)

# Use just the defaults
tplyr_table(mtcars, gear) %>%
  add_layer(
    group_desc(mpg, by=cyl)
  ) %>%
  add_layer(
    group_count(carb, by=cyl)
  ) %>%
  build()
#> # A tibble: 36 × 8
#>    row_label1 row_label2 var1_3        var1_4 var1_5 ord_layer_index ord_layer_1
#>    <chr>      <chr>      <chr>         <chr>  <chr>            <int>       <dbl>
#>  1 4          n          "  1"         "  8"  "  2"                1           1
#>  2 4          Mean (SD)  "21.50 (    … "26.9… "28.2…               1           1
#>  3 4          Median     "21.50"       "25.8… "28.2…               1           1
#>  4 4          Q1, Q3     "21.50, 21.5… "22.8… "27.1…               1           1
#>  5 4          Min, Max   "21.5, 21.5"  "21.4… "26.0…               1           1
#>  6 4          Missing    "  0"         "  0"  "  0"                1           1
#>  7 6          n          "  2"         "  4"  "  1"                1           2
#>  8 6          Mean (SD)  "19.75 ( 2.3… "19.7… "19.7…               1           2
#>  9 6          Median     "19.75"       "20.1… "19.7…               1           2
#> 10 6          Q1, Q3     "18.92, 20.5… "18.8… "19.7…               1           2
#> # ℹ 26 more rows
#> # ℹ 1 more variable: ord_layer_2 <dbl>

# Customize and modify
tplyr_table(mtcars, gear) %>%
  add_layer(
    group_desc(mpg, by=cyl) %>%
      set_format_strings(
        "n"         = f_str("xx", n),
        "Mean (SD)" = f_str("a.a+1 (a.a+2)", mean, sd, empty='NA'),
        "Median"    = f_str("a.a+1", median),
        "Q1, Q3"    = f_str("a, a", q1, q3, empty=c(.overall='NA')),
        "Min, Max"  = f_str("a, a", min, max),
        "Missing"   = f_str("xx", missing)
      )
  ) %>%
  add_layer(
    group_count(carb, by=cyl) %>%
      add_risk_diff(
        c('5', '3'),
        c('4', '3')
      ) %>%
      set_format_strings(
        n_counts = f_str('xx (xx%)', n, pct),
        riskdiff = f_str('xx.xxx (xx.xxx, xx.xxx)', dif, low, high)
      ) %>%
      set_order_count_method("bycount") %>%
      set_ordering_cols('4') %>%
      set_result_order_var(pct)
  ) %>%
  build()
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> # A tibble: 36 × 10
#>    row_label1 row_label2 var1_3        var1_4 var1_5 ord_layer_index ord_layer_1
#>    <chr>      <chr>      <chr>         <chr>  <chr>            <int>       <dbl>
#>  1 4          n          " 1"          " 8"   " 2"                 1           1
#>  2 4          Mean (SD)  "21.50 (    … "26.9… "28.2…               1           1
#>  3 4          Median     "21.50"       "25.8… "28.2…               1           1
#>  4 4          Q1, Q3     "22, 22"      "23, … "27, …               1           1
#>  5 4          Min, Max   "22, 22"      "21, … "26, …               1           1
#>  6 4          Missing    " 0"          " 0"   " 0"                 1           1
#>  7 6          n          " 2"          " 4"   " 1"                 1           2
#>  8 6          Mean (SD)  "19.75 ( 2.3… "19.7… "19.7…               1           2
#>  9 6          Median     "19.75"       "20.1… "19.7…               1           2
#> 10 6          Q1, Q3     "19, 21"      "19, … "20, …               1           2
#> # ℹ 26 more rows
#> # ℹ 3 more variables: ord_layer_2 <dbl>, rdiff_5_3 <chr>, rdiff_4_3 <chr>

# A Shift Table
tplyr_table(mtcars, am) %>%
  add_layer(
    group_shift(vars(row=gear, column=carb), by=cyl) %>%
    set_format_strings(f_str("xxx (xx.xx%)", n, pct))
  ) %>%
  build()
#> # A tibble: 9 × 17
#>   row_label1 row_label2 var1_0_1    var1_0_2 var1_0_3 var1_0_4 var1_0_6 var1_0_8
#>   <chr>      <chr>      <chr>       <chr>    <chr>    <chr>    <chr>    <chr>   
#> 1 4          3          "  1 (33.3… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 2 4          4          "  0 ( 0.0… "  2 (6… "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 3 4          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 4 6          3          "  2 (50.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 5 6          4          "  0 ( 0.0… "  0 ( … "  0 ( … "  2 (5… "  0 ( … "  0 ( …
#> 6 6          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 7 8          3          "  0 ( 0.0… "  4 (3… "  3 (2… "  5 (4… "  0 ( … "  0 ( …
#> 8 8          4          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> 9 8          5          "  0 ( 0.0… "  0 ( … "  0 ( … "  0 ( … "  0 ( … "  0 ( …
#> # ℹ 9 more variables: var1_1_1 <chr>, var1_1_2 <chr>, var1_1_3 <chr>,
#> #   var1_1_4 <chr>, var1_1_6 <chr>, var1_1_8 <chr>, ord_layer_index <int>,
#> #   ord_layer_1 <dbl>, ord_layer_2 <dbl>
```
