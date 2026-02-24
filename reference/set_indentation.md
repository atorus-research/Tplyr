# Set the option to prefix the row_labels in the inner count_layer

When a count layer uses nesting (i.e. triggered by
[`set_nest_count`](https://atorus-research.github.io/Tplyr/reference/set_nest_count.md)),
the `indentation` argument's value will be used as a prefix for the
inner layer's records

## Usage

``` r
set_indentation(e, indentation)
```

## Arguments

- e:

  A `count_layer` object

- indentation:

  A character to prefix the row labels in an inner count layer

## Value

The modified count_layer environment
