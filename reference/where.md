# Set or return where binding for layer or table

Set or return where binding for layer or table

## Usage

``` r
# S3 method for class 'tplyr_layer'
get_where(obj)

# S3 method for class 'tplyr_layer'
set_where(obj, where)

get_where(obj)

# S3 method for class 'tplyr_table'
get_where(obj)

set_where(obj, where)

# S3 method for class 'tplyr_table'
set_where(obj, where)

set_pop_where(obj, where)

get_pop_where(obj)
```

## Arguments

- obj:

  A `tplyr_layer` or `tplyr_table` object.

- where:

  An expression (i.e. syntax) to be used to subset the data. Supply as
  programming logic (i.e. x \< 5 & y == 10)

## Value

For `where`, the where binding of the supplied object. For `set_where`,
the modified object

## Examples

``` r
# Load in pipe
library(magrittr)

iris$Species2 <- iris$Species
lay <- tplyr_table(iris, Species) %>%
  group_count(Species) %>%
  set_where(Petal.Length > 3) %>%
  # Set logic for pop_data as well
  set_pop_where(Petal.Length > 3)
```
