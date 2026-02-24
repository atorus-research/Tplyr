# Set the option to nest count layers

If set to TRUE, the second variable specified in `target_var` will be
nested inside of the first variable. This allows you to create displays
like those commonly used in adverse event tables, where one column holds
both the labels of the outer categorical variable and the inside event
variable (i.e. AEBODSYS and AEDECOD).

## Usage

``` r
set_nest_count(e, nest_count)
```

## Arguments

- e:

  A `count_layer` object

- nest_count:

  A logical value to set the nest option

## Value

The modified layer
