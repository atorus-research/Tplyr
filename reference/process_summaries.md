# Process layers to get numeric results of layer

This is an internal method, but is exported to support S3 dispatch. Not
intended for direct use by a user.

## Usage

``` r
process_summaries(x, ...)
```

## Arguments

- x:

  a tplyr_layer object

- ...:

  arguments passed to dispatch

## Value

The tplyr_layer object with a 'built_table' binding
