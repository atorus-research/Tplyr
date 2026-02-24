# Select levels to keep in a count layer

In certain cases you only want a layer to include certain values of a
factor. The \`keep_levels()\` function allows you to pass character
values to be included in the layer. The others are ignored. \*\*NOTE:
Denominator calculation is unaffected by this function, see the examples
on how to include this logic in your percentages'\*\*

## Usage

``` r
keep_levels(e, ...)
```

## Arguments

- e:

  A `count_layer` object

- ...:

  Character values to count int he layer

## Value

The modified Tplyr layer object

## Examples

``` r
library(dplyr)
mtcars <- mtcars %>%
  mutate_all(as.character)

t <- tplyr_table(mtcars, gear) %>%
  add_layer(
    group_count(cyl) %>%
      keep_levels("4", "8") %>%
      set_denom_where(cyl %in% c("4", "8"))
 ) %>%
 build()
```
