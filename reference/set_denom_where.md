# Set Logic for denominator subsetting

By default, denominators in count layers are subset based on the layer
level where logic. In some cases this might not be correct. This
functions allows the user to override this behavior and pass custom
logic that will be used to subset the target dataset when calculating
denominators for the layer.

## Usage

``` r
set_denom_where(e, denom_where)
```

## Arguments

- e:

  A `count_layer/shift_layer` object

- denom_where:

  An expression (i.e. syntax) to be used to subset the target dataset
  for calculating layer denominators. Supply as programming logic (i.e.
  x \< 5 & y == 10). To remove the layer where parameter subsetting for
  the total row and thus the percentage denominators, pass 'TRUE' to
  this function.

## Value

The modified Tplyr layer object

## Examples

``` r
library(magrittr)
t10 <- tplyr_table(mtcars, gear) %>%
  add_layer(
    group_count(cyl, where = cyl != 6) %>%
    set_denom_where(TRUE)
    # The denominators will be based on all of the values, including 6
  ) %>%
 build()
```
