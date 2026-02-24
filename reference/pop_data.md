# Return or set population data bindings

The population data is used to gather information that may not be
available from the target dataset. For example, missing treatment
groups, population N counts, and proper N counts for denominators will
be provided through the population dataset. The population dataset
defaults to the target dataset unless otherwise specified using
`set_pop_data`.

## Usage

``` r
pop_data(table)

pop_data(x) <- value

set_pop_data(table, pop_data)
```

## Arguments

- table:

  A `tplyr_table` object

- x:

  A `tplyr_table` object

- value:

  A data.frame with population level information

- pop_data:

  A data.frame with population level information

## Value

For `tplyr_pop_data` the pop_data binding of the `tplyr_table` object.
For `tplyr_pop_data<-` nothing is returned, the pop_data binding is set
silently. For `set_tplyr_pop_data` the modified object.

## Examples

``` r
tab <- tplyr_table(iris, Species)

pop_data(tab) <- mtcars

tab <- tplyr_table(iris, Species) %>%
  set_pop_data(mtcars)
```
