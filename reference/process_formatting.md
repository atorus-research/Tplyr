# Process layers to get formatted and pivoted tables.

This is an internal method, but is exported to support S3 dispatch. Not
intended for direct use by a user.

## Usage

``` r
process_formatting(x, ...)
```

## Arguments

- x:

  A tplyr_layer object

- ...:

  arguments passed to dispatch

## Value

The formatted_table object that is bound to the layer
