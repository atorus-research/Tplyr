# Create a `tplyr_layer` object

This object is the workhorse of the `tplyr` package. A `tplyr_layer` can
be thought of as a block, or "layer" of a table. Summary tables
typically consist of different sections that require different
summaries. When programming these section, your code will create
different layers that need to be stacked or merged together. A
`tplyr_layer` is the container for those isolated building blocks.

When building the `tplyr_table`, each layer will execute independently.
When all of the data processing has completed, the layers are brought
together to construct the output.

`tplyr_layer` objects are not created directly, but are rather created
using the layer constructor functions
[`group_count`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md),
[`group_desc`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md),
and
[`group_shift`](https://atorus-research.github.io/Tplyr/reference/layer_constructors.md).

## Usage

``` r
tplyr_layer(parent, target_var, by, where, type, ...)
```

## Arguments

- parent:

  `tplyr_table` or `tplyr_layer`. Required. The parent environment of
  the layer. This must be either the `tplyr_table` object that the layer
  is contained within, or another `tplyr_layer` object of which the
  layer is a subgroup.

- target_var:

  Symbol. Required, The variable name on which the summary is to be
  performed. Must be a variable within the target dataset. Enter
  unquoted - i.e. target_var = AEBODSYS.

- by:

  A string, a variable name, or a list of variable names supplied using
  [`dplyr::vars`](https://dplyr.tidyverse.org/reference/vars.html)

- where:

  Call. Filter logic used to subset the target data when performing a
  summary.

- type:

  "count", "desc", or "shift". Required. The category of layer - either
  "counts" for categorical counts, "desc" for descriptive statistics, or
  "shift" for shift table counts

- ...:

  Additional arguments

## Value

A `tplyr_layer` environment that is a child of the specified parent. The
environment contains the object as listed below.

## `tplyr_layer` Core Object Structure

- `type`:

  This is an attribute. A string indicating the layer type, which
  controls the summary that will be performed.

- `target_var`:

  A quosure of a name, which is the variable on which a summary will be
  performed.

- `by`:

  A list of quosures representing either text labels or variable names
  used in grouping. Variable names must exist within the target dataset
  Text strings submitted do not need to exist in the target dataset.

- `cols`:

  A list of quosures used to determine the variables that are used to
  display in columns.

- `where`:

  A quosure of a call that containers the filter logic used to subset
  the target dataset. This filtering is in addition to any subsetting
  done based on `where` criteria specified in
  [`tplyr_table`](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)

- `layers`:

  A list with class `tplyr_layer_container`. Initialized as empty, but
  serves as the container for any sublayers of the current layer. Used
  internally.

Different layer types will have some different bindings specific to that
layer's needs.

## See also

[tplyr_table](https://atorus-research.github.io/Tplyr/reference/tplyr_table.md)

## Examples

``` r
tab <- tplyr_table(iris, Sepal.Width)

l <- group_count(tab, by=vars('Label Text', Species),
                 target_var=Species, where= Sepal.Width < 5.5,
                 cols = Species)

```
