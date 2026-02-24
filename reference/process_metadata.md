# Process layers to get metadata tables

This is an internal method, but is exported to support S3 dispatch. Not
intended for direct use by a user.

## Usage

``` r
process_metadata(x, ...)
```

## Arguments

- x:

  A tplyr_layer object

- ...:

  arguments passed to dispatch

## Value

The formatted_meta object that is bound to the layer
