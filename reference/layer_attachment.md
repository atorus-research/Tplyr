# Attach a layer to a `tplyr_table` object

`add_layer` attaches a `tplyr_layer` to a `tplyr_table` object. This
allows for a tidy style of programming (using `magrittr` piping, i.e.
`%>%`) with a secondary advantage - the construction of the layer object
may consist of a series of piped functions itself.

`Tplyr` encourages a user to view the construction of a table as a
series of "layers". The construction of each of these layers are
isolated and independent of one another - but each of these layers are
children of the table itself. `add_layer` isolates the construction of
an individual layer and allows the user to construct that layer and
insert it back into the parent. The syntax for this is intuitive and
allows for tidy piping. Simply pipe the current table object in, and
write the code to construct your layer within the `layer` parameter.

`add_layers` is another approach to attaching layers to a `tplyr_table`.
Instead of constructing the entire table at once, `add_layers` allows
you to construct layers as different objects. These layers can then be
attached into the `tplyr_table` all at once.

`add_layer` and `add_layers` both additionally allow you to name the
layers as you attach them. This is helpful when using functions like
[`get_numeric_data`](https://atorus-research.github.io/Tplyr/reference/get_numeric_data.md)
or
[`get_stats_data`](https://atorus-research.github.io/Tplyr/reference/get_stats_data.md)
when you can access information from a layer directly. `add_layer` has a
name parameter, and layers can be named in `add_layers` by submitting
the layer as a named argument.

## Usage

``` r
add_layer(parent, layer, name = NULL)

add_layers(parent, ...)
```

## Arguments

- parent:

  A `tplyr_table` or `tplyr_layer`/`tplyr_subgroup_layer` object

- layer:

  A layer construction function and associated modifier functions

- name:

  A name to provide the layer in the table layers container

- ...:

  Layers to be added

## Value

A `tplyr_table` or `tplyr_layer`/`tplyr_subgroup_layer` with a new layer
inserted into the `layer` binding

## See also

\[tplyr_table(), tplyr_layer(), group_count(), group_desc(),
group_shift()\]

## Examples

``` r
# Load in pipe
library(magrittr)

## Single layer
t <- tplyr_table(mtcars, cyl) %>%
  add_layer(
    group_desc(target_var=mpg)
  )

## Single layer with name
t <- tplyr_table(mtcars, cyl) %>%
  add_layer(name='mpg',
    group_desc(target_var=mpg)
  )

# Using add_layers
t <- tplyr_table(mtcars, cyl)
l1 <- group_desc(t, target_var=mpg)
l2 <- group_count(t, target_var=cyl)

t <- add_layers(t, l1, 'cyl' = l2)
```
