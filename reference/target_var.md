# Set or return treat_var binding

Set or return treat_var binding

## Usage

``` r
get_target_var(layer)

set_target_var(layer, target_var)
```

## Arguments

- layer:

  A `tplyr_layer` object

- target_var:

  A symbol to perform the analysis on

## Value

For `treat_var`, the treatment variable binding of the layer object. For
`set_treat_var`, the modified layer environment.

## Examples

``` r
# Load in pipe
library(magrittr)
iris$Species2 <- iris$Species
lay <- tplyr_table(iris, Species) %>%
  group_count(Species) %>%
  set_target_var(Species2)
```
