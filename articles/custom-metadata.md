# Creating Custom Tplyr Metadata

As covered in
[`vignette('metadata')`](https://atorus-research.github.io/Tplyr/articles/metadata.md),**Tplyr**
can produce metadata for any result that it calculates. But what about
data that **Tplyr** can’t produce, such as a efficacy results or some
sort of custom analysis? You may still want that drill down capability
either on your own or paired with an existing **Tplyr** table.

Take for instance Table 14-3.01 from the [CDISC
Pilot](https://github.com/atorus-research/CDISC_pilot_replication).
Skipping the actual construction of the table, here’s the output data
from **Tplyr** and some manual calculation:

``` r
kable(full_data)
```

| row_id | row_label1                             | row_label2     | var1_Placebo | var1_Xanomeline Low Dose | var1_Xanomeline High Dose |
|:-------|:---------------------------------------|:---------------|:-------------|:-------------------------|:--------------------------|
| d1_1   | Baseline                               | n              | 79           | 81                       | 74                        |
| d2_1   |                                        | Mean (SD)      | 24.1 (12.19) | 24.4 (12.92)             | 21.3 (11.74)              |
| d3_1   |                                        | Median (Range) | 21.0 ( 5;61) | 21.0 ( 5;57)             | 18.0 ( 3;57)              |
|        |                                        |                |              |                          |                           |
| d1_2   | Week 24                                | n              | 79           | 81                       | 74                        |
| d2_2   |                                        | Mean (SD)      | 26.7 (13.79) | 26.4 (13.18)             | 22.8 (12.48)              |
| d3_2   |                                        | Median (Range) | 24.0 ( 5;62) | 25.0 ( 6;62)             | 20.0 ( 3;62)              |
|        |                                        |                |              |                          |                           |
| d1_3   | Change from Baseline                   | n              | 79           | 81                       | 74                        |
| d2_3   |                                        | Mean (SD)      | 2.5 ( 5.80)  | 2.0 ( 5.55)              | 1.5 ( 4.26)               |
| d3_3   |                                        | Median (Range) | 2.0 (-11;16) | 2.0 (-11;17)             | 1.0 ( -7;13)              |
|        |                                        |                |              |                          |                           |
| x4_1   | p-value(Dose Response) \[1\]\[2\]      |                |              |                          | 0.245                     |
| x4_2   |                                        |                |              |                          |                           |
| x4_3   | p-value(Xan - Placebo) \[1\]\[3\]      |                |              | 0.569                    | 0.233                     |
| x4_4   | Diff of LS Means (SE)                  |                |              | -0.5 (0.82)              | -1.0 (0.84)               |
| x4_5   | 95% CI                                 |                |              | (-2.1;1.1)               | (-2.7;0.7)                |
| x4_6   |                                        |                |              |                          |                           |
| x4_7   | p-value(Xan High - Xan Low) \[1\]\[3\] |                |              |                          | 0.520                     |
| x4_8   | Diff of LS Means (SE)                  |                |              |                          | -0.5 (0.84)               |
| x4_9   | 95% CI                                 |                |              |                          | (-2.2;1.1)                |

This is the primary efficacy table from the trial. The top portion of
this table is fairly straightforward with **Tplyr** and can be done
using descriptive statistic layers. Once you hit the p-values on the
lower house, this becomes beyond Tplyr’s remit. To produce the table,
you can combine **Tplyr** output with a separate data frame analyzed and
formatted yourself (but note you can still use some help from **Tplyr**
tools like
[`apply_formats()`](https://atorus-research.github.io/Tplyr/reference/apply_formats.md)).

But what about the metadata? How do you get the drill down capabilities
for that lower half of the table? We’ve provided a couple additional
tools in **Tplyr** to allow you to construct your own metadata and
append existing metadata present in a **Tplyr** table.

## Build a `tplyr_meta` object

As covered in
[`vignette('metadata')`](https://atorus-research.github.io/Tplyr/articles/metadata.md),
a `tplyr_meta` object consists of two different fields: A list of
variable names, and a list of filter conditions. You provide both of
these fields as a list of quosures:

``` r
m <- tplyr_meta(
  names = quos(a, b, c),
  filters = quos(a==1, b==2, c==3)
)
m
#> tplyr_meta: 3 names, 3 filters
#> Names:
#>      a, b, c 
#> Filters:
#>      a == 1, b == 2, c == 3
```

The
[`tplyr_meta()`](https://atorus-research.github.io/Tplyr/reference/tplyr_meta.md)
function can take these fields immediately upon creation. If you need to
dynamically create a `tplyr_meta` object such as how **Tplyr**
constructs the objects internally), the functions
[`add_variables()`](https://atorus-research.github.io/Tplyr/reference/metadata_additions.md)
and
[`add_filters()`](https://atorus-research.github.io/Tplyr/reference/metadata_additions.md)
are available to extend an existing `tplyr_meta` object:

``` r
m <- m %>% 
  add_variables(quos(x)) %>% 
  add_filters(quos(x == 'a'))

m
#> tplyr_meta: 4 names, 4 filters
#> Names:
#>      a, b, c, x 
#> Filters:
#>      a == 1, b == 2, c == 3, x == "a"
```

## Building your own metadata table

Now that we can create our own `tplyr_meta` objects, let’s assemble the
metadata for the bottom portion of Table 14-3.01:

``` r
# Overall model subset of data
meta <- tplyr_meta(
  names = quos(TRTP, EFFFL, ITTFL, ANL01FL, SITEGR1, AVISIT, AVISITN, PARAMCD, AVAL, BASE, CHG),
  filters = quos(EFFFL == "Y", ITTFL == "Y", PARAMCD == "ACTOT", ANL01FL == "Y", AVISITN == 24)
)

# Xan High / Placebo contrast
meta_xhp <- meta %>% 
  add_filters(quos(TRTP %in% c("Xanomeline High Dose", "Placebo")))

# Xan Low / Placbo Contrast
meta_xlp <- meta %>% 
  add_filters(quos(TRTP %in% c("Xanomeline Low Dose", "Placebo")))

# Xan High / Xan Low Contrast
meta_xlh <- meta %>% 
  add_filters(quos(TRTP %in% c("Xanomeline High Dose", "Xanomeline Low Dose")))

eff_meta <- tibble::tribble(
  ~"row_id",  ~"row_label1",                       ~"var1_Xanomeline Low Dose", ~"var1_Xanomeline High Dose",
  "x4_1",    "p-value(Dose Response) [1][2]",      NULL,                        meta,
  "x4_3",    "p-value(Xan - Placebo) [1][3]",        meta_xlp,                    meta_xhp,
  "x4_4",    "   Diff of LS Means (SE)",           meta_xlp,                    meta_xhp,
  "x4_5",    "   95% CI",                          meta_xlp,                    meta_xhp,
  "x4_7",    "p-value(Xan High - Xan Low) [1][3]", NULL,                        meta_xlh,
  "x4_8",    "   Diff of LS Means (SE)",           NULL,                        meta_xlh,
  "x4_9",    "   95% CI",                          NULL,                        meta_xlh
)
```

Let’s break down what happened here:

- First, we assemble the the overarching metadata object for the model.
  A lot of this metadata is shared across each of the different result
  cells for all of the efficacy data, so we can start by collecting this
  information into a `tplyr_meta` object.
- Next, we can use that starting point to build `tplyr_meta` objects for
  the other result cells. The model data contains contrasts of each of
  the different treatment group comparisons. By using
  [`add_filters()`](https://atorus-research.github.io/Tplyr/reference/metadata_additions.md),
  we can create those additional three `tplyr_meta` objects using the
  starting point and attaching an additional filter condition.
- Lastly, to extend the metadata in the original `tplyr_table` object
  that created the summary portion of this table, we need a data frame.
  There’s a lot of ways to do this, but I like the display and
  explicitness of
  [`tibble::tribble()`](https://tibble.tidyverse.org/reference/tribble.html).

When building a data frame for use with `tplyr_table` metadata, there
are really only two rules:

- You need a column in the data frame called `row_id`
- The `row_id` values cannot be duplicates of any other value within the
  existing metadata.

The `row_id` values built by **Tplyr** will always follow the format
“n_n”, where the first letter of the layer type will either be “c”, “d”,
or “s”. The next number is the layer number (i.e. the order in which the
layer was inserted to the **Tplyr** table), and then finally the row of
that layer within the output. For example, the third row of a count
layer that was the second layer in the table would have a `row_id` of
“c2_3”. In this example, I chose “x4_n” as the format for the “x” to
symbolize custom, and these data can be thought of as the fourth layer.
That said, these values would typically be masked by the viewer of the
table so they really just need to be unique - so you can choose whatever
you want.

### Anti-joins

If the custom metadata you’re constructing requires references to data
outside your target dataset, this is also possible with a `tplyr_meta`
object. If you’re looking for non-overlap with the target dataset, you
can use an anti-join. Anti-joins can be added to a `tplyr_meta` object
using the
[`add_anti_join()`](https://atorus-research.github.io/Tplyr/reference/add_anti_join.md)
function.

``` r
meta %>% 
  add_anti_join(
    join_meta = tplyr_meta(
      names = quos(TRT01P, EFFFL, ITTFL, SITEGR1),
      filters = quos(EFFFL == "Y", ITTFL == "Y")
    ),
    on = quos(USUBJID)
  )
#> tplyr_meta: 11 names, 5 filters
#> Names:
#>      TRTP, EFFFL, ITTFL, ANL01FL, SITEGR1, AVISIT, AVISITN, PARAMCD, AVAL, BASE, CHG 
#> Filters:
#>      EFFFL == "Y", ITTFL == "Y", PARAMCD == "ACTOT", ANL01FL == "Y", AVISITN == 24 
#> Anti-join:
#>     Join Meta:
#>         tplyr_meta: 4 names, 2 filters
#>         Names:
#>              TRT01P, EFFFL, ITTFL, SITEGR1 
#>         Filters:
#>              EFFFL == "Y", ITTFL == "Y" 
#>     On:
#>         USUBJID
```

## Appending Existing **Tplyr** Metadata

Now that we’ve created our custom extension of the **Tplyr** metadata,
let’s extend the existing data frame. To do this, **Tplyr** has the
function
[`append_metadata()`](https://atorus-research.github.io/Tplyr/reference/append_metadata.md):

``` r
t <- append_metadata(t, eff_meta)
```

Behind the scenes, this function simply binds the new metadata with the
old in the proper section of the `tplyr_table` object. You can view the
the `tplyr_table` metadata with the function
[`get_metadata()`](https://atorus-research.github.io/Tplyr/reference/get_metadata.md):

``` r
get_metadata(t)
#> # A tibble: 16 × 6
#>    row_id row_label1              row_label2 var1_Placebo var1_Xanomeline High…¹
#>    <chr>  <chr>                   <chr>      <list>       <list>                
#>  1 d1_1   "Baseline"              n          <tplyr_mt>   <tplyr_mt>            
#>  2 d2_1   "Baseline"              Mean (SD)  <tplyr_mt>   <tplyr_mt>            
#>  3 d3_1   "Baseline"              Median (R… <tplyr_mt>   <tplyr_mt>            
#>  4 d1_2   "Week 24"               n          <tplyr_mt>   <tplyr_mt>            
#>  5 d2_2   "Week 24"               Mean (SD)  <tplyr_mt>   <tplyr_mt>            
#>  6 d3_2   "Week 24"               Median (R… <tplyr_mt>   <tplyr_mt>            
#>  7 d1_3   "Change from Baseline"  n          <tplyr_mt>   <tplyr_mt>            
#>  8 d2_3   "Change from Baseline"  Mean (SD)  <tplyr_mt>   <tplyr_mt>            
#>  9 d3_3   "Change from Baseline"  Median (R… <tplyr_mt>   <tplyr_mt>            
#> 10 x4_1   "p-value(Dose Response… NA         <NULL>       <tplyr_mt>            
#> 11 x4_3   "p-value(Xan - Placebo… NA         <NULL>       <tplyr_mt>            
#> 12 x4_4   "   Diff of LS Means (… NA         <NULL>       <tplyr_mt>            
#> 13 x4_5   "   95% CI"             NA         <NULL>       <tplyr_mt>            
#> 14 x4_7   "p-value(Xan High - Xa… NA         <NULL>       <tplyr_mt>            
#> 15 x4_8   "   Diff of LS Means (… NA         <NULL>       <tplyr_mt>            
#> 16 x4_9   "   95% CI"             NA         <NULL>       <tplyr_mt>            
#> # ℹ abbreviated name: ¹​`var1_Xanomeline High Dose`
#> # ℹ 1 more variable: `var1_Xanomeline Low Dose` <list>
```

Finally, as with the automatically created metadata from Tplyr, we can
query these result cells just the same:

``` r
get_meta_subset(t, 'x4_1', "var1_Xanomeline High Dose") %>% 
  head() %>% 
  kable()
```

| USUBJID     | TRTP                 | EFFFL | ITTFL | ANL01FL | SITEGR1 | AVISIT  | AVISITN | PARAMCD | AVAL | BASE | CHG |
|:------------|:---------------------|:------|:------|:--------|:--------|:--------|--------:|:--------|-----:|-----:|----:|
| 01-701-1015 | Placebo              | Y     | Y     | Y       | 701     | Week 24 |      24 | ACTOT   |    8 |   13 |  -5 |
| 01-701-1023 | Placebo              | Y     | Y     | Y       | 701     | Week 24 |      24 | ACTOT   |   12 |   13 |  -1 |
| 01-701-1028 | Xanomeline High Dose | Y     | Y     | Y       | 701     | Week 24 |      24 | ACTOT   |    3 |    3 |   0 |
| 01-701-1033 | Xanomeline Low Dose  | Y     | Y     | Y       | 701     | Week 24 |      24 | ACTOT   |    7 |    7 |   0 |
| 01-701-1034 | Xanomeline High Dose | Y     | Y     | Y       | 701     | Week 24 |      24 | ACTOT   |   11 |   11 |   0 |
| 01-701-1047 | Placebo              | Y     | Y     | Y       | 701     | Week 24 |      24 | ACTOT   |   19 |   10 |   9 |

## Metadata Without Tplyr

You very well may have a scenario where you want to use these metadata
functions outside of **Tplyr** in general. As such, there are S3 methods
available to query metadata from a dataframe instead of a **Tplyr**
table, and parameters to provide your own target data frame:

``` r
get_meta_subset(eff_meta, 'x4_1', "var1_Xanomeline High Dose", target=tplyr_adas) %>% 
  head() %>% 
  kable()
```

| USUBJID     | TRTP                 | EFFFL | ITTFL | ANL01FL | SITEGR1 | AVISIT  | AVISITN | PARAMCD | AVAL | BASE | CHG |
|:------------|:---------------------|:------|:------|:--------|:--------|:--------|--------:|:--------|-----:|-----:|----:|
| 01-701-1015 | Placebo              | Y     | Y     | Y       | 701     | Week 24 |      24 | ACTOT   |    8 |   13 |  -5 |
| 01-701-1023 | Placebo              | Y     | Y     | Y       | 701     | Week 24 |      24 | ACTOT   |   12 |   13 |  -1 |
| 01-701-1028 | Xanomeline High Dose | Y     | Y     | Y       | 701     | Week 24 |      24 | ACTOT   |    3 |    3 |   0 |
| 01-701-1033 | Xanomeline Low Dose  | Y     | Y     | Y       | 701     | Week 24 |      24 | ACTOT   |    7 |    7 |   0 |
| 01-701-1034 | Xanomeline High Dose | Y     | Y     | Y       | 701     | Week 24 |      24 | ACTOT   |   11 |   11 |   0 |
| 01-701-1047 | Placebo              | Y     | Y     | Y       | 701     | Week 24 |      24 | ACTOT   |   19 |   10 |   9 |

As with the **Tplyr** metadata, the only strict criteria here is that
your custom metadata have a `row_id` column.

## Tying it Together

The vignette wouldn’t be complete without the final contextual example -
so here we go. Ultimately these pieces an all fit together in the
context of a Shiny application and give you the desired click-through
experience.

*Source code available
[here](https://github.com/atorus-research/Tplyr-efficacy-shiny-demo)*
