# Set or return precision_by layer binding

The precision_by variables are used to collect the integer and decimal
precision when auto-precision is used. These by variables are used to
group the input data and identify the maximum precision available within
the dataset for each by group. The precision_by variables must be a
subset of the by variables

## Usage

``` r
get_precision_by(layer)

set_precision_by(layer, precision_by)
```

## Arguments

- layer:

  A `tplyr_layer` object

- precision_by:

  A string, a variable name, or a list of variable names supplied using
  [`dplyr::vars`](https://dplyr.tidyverse.org/reference/vars.html).

## Value

For `get_precision_by`, the precision_by binding of the supplied layer.
For `set_precision_by` the modified layer environment.

## Examples

``` r
# Load in pipe
library(magrittr)
lay <- tplyr_table(mtcars, gear) %>%
  add_layer(
    group_desc(mpg, by=vars(carb, am)) %>%
    set_precision_by(carb)
  )
```
