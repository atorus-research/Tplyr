# Get or set the default format strings for descriptive statistics layers

Tplyr provides you with the ability to set table-wide defaults of format
strings. You may wish to reuse the same format strings across numerous
layers. `set_desc_layer_formats` and `set_count_layer_formats` allow you
to apply your desired format strings within the entire scope of the
table.

## Usage

``` r
get_desc_layer_formats(obj)

set_desc_layer_formats(obj, ...)

get_count_layer_formats(obj)

set_count_layer_formats(obj, ...)

get_shift_layer_formats(obj)

set_shift_layer_formats(obj, ...)
```

## Arguments

- obj:

  A tplyr_table object

- ...:

  formats to pass forward

## Details

For descriptive statistic layers, you can also use `set_format_strings`
and `set_desc_layer_formats` together within a table, but not within the
same layer. In the absence of specified format strings, first the table
will be checked for any available defaults, and otherwise the
`tplyr.desc_layer_default_formats` option will be used.
`set_format_strings` will always take precedence over either. Defaults
cannot be combined between `set_format_strings`,
`set_desc_layer_formats`, and the `tplyr.desc_layer_default_formats`
because the order of presentation of results is controlled by the format
strings, so relying on combinations of these setting would not be
intuitive.

For count layers, you can override the `n_counts` or `riskdiff` format
strings separately, and the narrowest scope available will be used from
layer, to table, to default options.
