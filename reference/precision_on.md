# Set or return precision_on layer binding

The precision_on variable is the variable used to establish numeric
precision. This variable must be included in the list of `target_var`
variables.

## Usage

``` r
get_precision_on(layer)

set_precision_on(layer, precision_on)
```

## Arguments

- layer:

  A `tplyr_layer` object

- precision_on:

  A string, a variable name, or a list of variable names supplied using
  [`dplyr::vars`](https://dplyr.tidyverse.org/reference/vars.html).

## Value

For `get_precision_on`, the precision_on binding of the supplied layer.
For `set_precision_on` the modified layer environment.

## Examples

``` r
# Load in pipe
library(magrittr)
lay <- tplyr_table(mtcars, gear) %>%
  add_layer(
    group_desc(vars(mpg, disp), by=vars(carb, am)) %>%
    set_precision_on(disp)
  )
```
