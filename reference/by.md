# Set or return by layer binding

Set or return by layer binding

## Usage

``` r
get_by(layer)

set_by(layer, by)
```

## Arguments

- layer:

  A `tplyr_layer` object

- by:

  A string, a variable name, or a list of variable names supplied using
  [`dplyr::vars`](https://dplyr.tidyverse.org/reference/vars.html).

## Value

For `get_by`, the `by` binding of the supplied layer. For `set_by` the
modified layer environment.

## Examples

``` r
# Load in pipe
library(magrittr)
iris$Species2 <- iris$Species
lay <- tplyr_table(iris, Species) %>%
  group_count(Species) %>%
  set_by(vars(Species2, Sepal.Width))
```
