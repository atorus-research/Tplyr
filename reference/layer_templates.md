# Create, view, extract, remove, and use Tplyr layer templates

There are several scenarios where a layer template may be useful. Some
tables, like demographics tables, may have many layers that will all
essentially look the same. Categorical variables will have the same
count layer settings, and continuous variables will have the same desc
layer settings. A template allows a user to build those settings once
per layer, then reference the template when the Tplyr table is actually
built.

## Usage

``` r
new_layer_template(name, template)

remove_layer_template(name)

get_layer_template(name)

get_layer_templates()

use_template(name, ..., add_params = NULL)
```

## Arguments

- name:

  Template name

- template:

  Template layer syntax, starting with a layer constructor
  `group_count|desc|shift`. This function should be called with an
  ellipsis argument (i.e. group_count(...)).

- ...:

  Arguments passed directly into a layer constructor, matching the
  target, by, and where parameters.

- add_params:

  Additional parameters passed into layer modifier functions. These
  arguments are specified in a template within curly brackets such as
  {param}. Supply as a named list, where the element name is the
  parameter.

## Details

This suite of functions allows a user to create and use layer templates.
Layer templates allow a user to pre-build and reuse an entire layer
configuration, from the layer constructor down to all modifying
functions. Furthermore, users can specify parameters they may want to be
interchangeable. Additionally, layer templates are extensible, so a
template can be use and then further extended with additional layer
modifying functions.

Layers are created using `new_layer_template()`. To use a layer, use the
function `use_template()` in place of `group_count|desc|shift()`. If you
want to view a specific template, use `get_layer_template()`. If you
want to view all templates, use `get_layer_templates()`. And to remove a
layer template use `remove_layer_template()`. Layer templates themselves
are stored in the option `tplyr.layer_templates`, but a user should not
access this directly and instead use the Tplyr supplied functions.

When providing the template layer syntax, the layer must start with a
layer constructor. These are one of the function
[`group_count()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md),
[`group_desc()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md),
or
[`group_shift()`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md).
Instead of passing arguments into these function, templates are
specified using an ellipsis in the constructor, i.e. `group_count(...)`.
This is required, as after the template is built a user supplies these
arguments via `use_template()`

`use_template()` takes the `group_count|desc|shift()` arguments by
default. If a user specified additional arguments in the template, these
are provided in a list throught the argument `add_params`. Provide these
arguments exactly as you would in a normal layer. When creating the
template, these parameters can be specified by using curly brackets. See
the examples for details.

## Examples

``` r
op <- options()

new_layer_template(
  "example_template",
  group_count(...) %>%
    set_format_strings(f_str('xx (xx%)', n, pct))
)

get_layer_templates()
#> $example_template
#> Template name: example_template
#> Template parameters: None
#> Template code:
#> {
#> group_count(...) %>% set_format_strings(f_str("xx (xx%)", n, pct))
#> } 
#> 

get_layer_template("example_template")
#> Template name: example_template
#> Template parameters: None
#> Template code:
#> {
#> group_count(...) %>% set_format_strings(f_str("xx (xx%)", n, pct))
#> } 

tplyr_table(mtcars, vs) %>%
  add_layer(
    use_template("example_template", gear)
  ) %>%
  build()
#> # A tibble: 3 × 5
#>   row_label1 var1_0     var1_1     ord_layer_index ord_layer_1
#>   <chr>      <chr>      <chr>                <int>       <dbl>
#> 1 3          "12 (67%)" " 3 (21%)"               1           1
#> 2 4          " 2 (11%)" "10 (71%)"               1           2
#> 3 5          " 4 (22%)" " 1 ( 7%)"               1           3

remove_layer_template("example_template")

new_layer_template(
  "example_template",
  group_count(...) %>%
    set_format_strings(f_str('xx (xx%)', n, pct)) %>%
    set_order_count_method({sort_meth}) %>%
    set_ordering_cols({sort_cols})
)

get_layer_template("example_template")
#> Template name: example_template
#> Template parameters: sort_meth, sort_cols
#> Template code:
#> {
#> group_count(...) %>% set_format_strings(f_str("xx (xx%)", n, pct)) %>% set_order_count_method({
#>     sort_meth
#> }) %>% set_ordering_cols({
#>     sort_cols
#> })
#> } 

tplyr_table(mtcars, vs) %>%
  add_layer(
    use_template("example_template", gear, add_params =
                   list(
                     sort_meth = "bycount",
                     sort_cols = `1`
                   ))
  ) %>%
  build()
#> # A tibble: 3 × 5
#>   row_label1 var1_0     var1_1     ord_layer_index ord_layer_1
#>   <chr>      <chr>      <chr>                <int>       <dbl>
#> 1 3          "12 (67%)" " 3 (21%)"               1           3
#> 2 4          " 2 (11%)" "10 (71%)"               1          10
#> 3 5          " 4 (22%)" " 1 ( 7%)"               1           1

remove_layer_template("example_template")

options(op)
```
