# Set the value of a outer nested count layer to Inf or -Inf

Set the value of a outer nested count layer to Inf or -Inf

## Usage

``` r
set_outer_sort_position(e, outer_sort_position)
```

## Arguments

- e:

  A `count_layer` object

- outer_sort_position:

  Either 'asc' or 'desc'. If desc the final ordering helper will be set
  to Inf, if 'asc' the ordering helper is set to -Inf.

## Value

The modified count layer.
