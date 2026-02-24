# Layer Templates

There are several scenarios where a layer template may be useful. Some
tables, like demographics tables, may have many layers that will all
essentially look the same. Categorical variables will have the same
count layer settings, and continuous variables will have the same desc
layer settings. A template allows a user to build those settings once
per layer, then reference the template when the **Tplyr** table is
actually built. Another scenario might be building a set of company
layer templates that are built for standard tables to reduce the
footprint of code across analyses. In either of these cases, the idea is
to reduce the amount of redundant code necessary to create a table.

Tplyr has already has mechanisms to reduce redundant application of
formats. For example, `vignettes('tplyr_options')` shows how the options
`tplyr.count_layer_default_formats`, `tplyr.desc_layer_default_formats`,
and `tplyr.shift_layer_default_formats` can be used to create default
format string settings. Additionally, you can set formats table-wide
using
[`set_count_layer_formats()`](https://atorus-research.github.io/Tplyr/reference/table_format_defaults.md),
[`set_desc_layer_formats()`](https://atorus-research.github.io/Tplyr/reference/table_format_defaults.md),
or
[`set_shift_layer_formats()`](https://atorus-research.github.io/Tplyr/reference/table_format_defaults.md).
But what these functions and options *don’t* allow you to do is pre-set
and reuse the settings for an entire layer, so all of the additional
potential layer-modifying functions are ignored. This is where layer
templates come in.

## Basic Templates

The functions
[`new_layer_template()`](https://atorus-research.github.io/Tplyr/reference/layer_templates.md)
and
[`use_template()`](https://atorus-research.github.io/Tplyr/reference/layer_templates.md)
allow a user to create and use layer templates. Layer templates allow a
user to pre-build and reuse an entire layer configuration, from the
layer constructor down to all modifying functions. Furthermore, users
can specify parameters they may want to be interchangeable.
Additionally, layer templates are extensible, so a template can be used
and then further extended with additional layer-modifying functions.

Consider the following example:

``` r
new_layer_template(
  "example_template", 
  group_count(...) %>% 
    set_format_strings(f_str("xx (xx%)", n, pct))
)
```

In this example, we’ve created a basic layer template. The template is
named “example_template”, and this is the name we’ll use to reference
the template when we want to use it. When the template is created, we
start with the function `group_count(...)`. Note the use of the ellipsis
(i.e. `...`). This is a required part of a layer template. Templates
must start with a **Tplyr** layer constructor, which is one of the
functions
[`group_count()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md),
[`group_desc()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md),
or
[`group_shift()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md).
The ellipsis is necessary because when the template is used, we are able
to pass arguments directly into the layer constructor. For example:

``` r
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    use_template("example_template", RACE, by=ETHNIC)
  ) %>% 
  build() %>% 
  kable()
```

| row_label1             | row_label2                       | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 | ord_layer_2 |
|:-----------------------|:---------------------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|------------:|
| HISPANIC OR LATINO     | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0%)      | 0 ( 0%)                   | 0 ( 0%)                  |               1 |           1 |           1 |
| HISPANIC OR LATINO     | BLACK OR AFRICAN AMERICAN        | 0 ( 0%)      | 0 ( 0%)                   | 0 ( 0%)                  |               1 |           1 |           2 |
| HISPANIC OR LATINO     | WHITE                            | 3 ( 3%)      | 3 ( 4%)                   | 6 ( 7%)                  |               1 |           1 |           3 |
| NOT HISPANIC OR LATINO | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0%)      | 1 ( 1%)                   | 0 ( 0%)                  |               1 |           2 |           1 |
| NOT HISPANIC OR LATINO | BLACK OR AFRICAN AMERICAN        | 8 ( 9%)      | 9 (11%)                   | 6 ( 7%)                  |               1 |           2 |           2 |
| NOT HISPANIC OR LATINO | WHITE                            | 75 (87%)     | 71 (85%)                  | 72 (86%)                 |               1 |           2 |           3 |

Within
[`use_template()`](https://atorus-research.github.io/Tplyr/reference/layer_templates.md),
the first parameter is the template name. After that, we supply
arguments as we normally would into
[`group_count()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md),
[`group_desc()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md),
or
[`group_shift()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md).
Additionally, note that our formats have been applied just as they would
be if we used
[`set_format_strings()`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md)
as specified in the template. Our template was applied, and the table
built with all of the settings appropriately.

An additional feature of layer templates is that they act just as any
other function would in a **Tplyr** layer. This means that they’re also
extensible and can be expanded on directly within a **Tplyr** table. For
example:

``` r
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    use_template("example_template", RACE) %>% 
      add_total_row()
  ) %>% 
  build() %>% 
  kable()
```

| row_label1                       | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:---------------------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|
| AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0%)      | 1 ( 1%)                   | 0 ( 0%)                  |               1 |           1 |
| BLACK OR AFRICAN AMERICAN        | 8 ( 9%)      | 9 (11%)                   | 6 ( 7%)                  |               1 |           2 |
| WHITE                            | 78 (91%)     | 74 (88%)                  | 78 (93%)                 |               1 |           3 |
| Total                            | 86 (100%)    | 84 (100%)                 | 84 (100%)                |               1 |           4 |

Here we show two things - first, that we called the template without the
*by* variable argument from the previous example. This allows a template
to have some flexibility depending on the context of its usage.
Furthermore, we added the additional modifier function
[`add_total_row()`](https://atorus-research.github.io/Tplyr/reference/add_total_row.md).
In this example, we took the layer as constructed by the template and
then modified that layer further. This may be useful if most but not all
of a layer is reusable. The reusable portions can be put in a template,
and the rest added using normal **Tplyr** syntax.

### Templates With Parameters

It’s also possible to add interchangeable parameters into a layer
template beyond the group constructor arguments. But this requires some
special syntax. Consider the following template:

``` r
new_layer_template("example_params",
  group_count(...) %>% 
    set_format_strings(f_str("xx (xx.x%)", n, pct)) %>% 
    set_order_count_method({sort_meth}) %>% 
    set_ordering_cols({sort_col})
 )
```

In this example, we create a template similar to the first example. But
now we add two more modifying functions,
[`set_order_count_method()`](https://atorus-research.github.io/Tplyr/reference/ordering.md)
and
[`set_ordering_cols()`](https://atorus-research.github.io/Tplyr/reference/ordering.md).
Within these functions, we’ve supplied interchangeable parameters to the
template function, which are `sort_meth` and `sort_col`. In a **Tplyr**
layer template, these parameters are supplied using curly brackets
(i.e. {}).

To specify these arguments when using the templater, we use the
[`use_template()`](https://atorus-research.github.io/Tplyr/reference/layer_templates.md)
argument `add_params`. For example:

``` r
tplyr_table(tplyr_adsl, TRT01P) %>% 
  add_layer(
    use_template('example_params', RACE, add_params = 
                   list(
                     sort_meth = "bycount",
                     sort_col = Placebo
                   ))
  ) %>% 
  build() %>% 
  kable()
```

| row_label1                       | var1_Placebo | var1_Xanomeline High Dose | var1_Xanomeline Low Dose | ord_layer_index | ord_layer_1 |
|:---------------------------------|:-------------|:--------------------------|:-------------------------|----------------:|------------:|
| AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)    | 1 ( 1.2%)                 | 0 ( 0.0%)                |               1 |           0 |
| BLACK OR AFRICAN AMERICAN        | 8 ( 9.3%)    | 9 (10.7%)                 | 6 ( 7.1%)                |               1 |           8 |
| WHITE                            | 78 (90.7%)   | 74 (88.1%)                | 78 (92.9%)               |               1 |          78 |

In the `add_params` parameter, you must supply a list. That list must
also be named, where the element names (in this example, `sort_meth` and
`sort_col`) match the parameter names in the template itself. If there’s
any mismatch between a template’s parameters and the parameters provided
to `add_params`, you will encounter an error. The values supplied to
`add_param` are then exactly the arguments that you would supply to the
matching field within the template (i.e. there’s no extra quoting using
[`quo()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
necessary to pass a symbol).

## Viewing and Removing Templates

If you want to view any available templates in your session, use the
function
[`get_layer_templates()`](https://atorus-research.github.io/Tplyr/reference/layer_templates.md).

``` r
get_layer_templates()
#> $example_template
#> Template name: example_template
#> Template parameters: None
#> Template code:
#> {
#> group_count(...) %>% set_format_strings(f_str("xx (xx%)", n, pct))
#> } 
#> 
#> $example_params
#> Template name: example_params
#> Template parameters: sort_meth, sort_col
#> Template code:
#> {
#> group_count(...) %>% set_format_strings(f_str("xx (xx.x%)", n, pct)) %>% set_order_count_method({
#>     sort_meth
#> }) %>% set_ordering_cols({
#>     sort_col
#> })
#> }
```

You can view a specific template using
[`get_layer_template()`](https://atorus-research.github.io/Tplyr/reference/layer_templates.md).

``` r
get_layer_template("example_params")
#> Template name: example_params
#> Template parameters: sort_meth, sort_col
#> Template code:
#> {
#> group_count(...) %>% set_format_strings(f_str("xx (xx.x%)", n, pct)) %>% set_order_count_method({
#>     sort_meth
#> }) %>% set_ordering_cols({
#>     sort_col
#> })
#> }
```

Note that layer templates are of class `tplyr_layer_template`. They
additionally carry the attribute `params` that specifies which
parameters are available in the template, which can be seen in the
output above.

Finally, if you want to remove a layer from your session, use the
function
[`remove_layer_template()`](https://atorus-research.github.io/Tplyr/reference/layer_templates.md)

``` r
remove_layer_template("example_params")
get_layer_templates()
#> $example_template
#> Template name: example_template
#> Template parameters: None
#> Template code:
#> {
#> group_count(...) %>% set_format_strings(f_str("xx (xx%)", n, pct))
#> }
```
