# Extract the subset of data based on result metadata

Given a row_id value and a result column, this function will return the
subset of data referenced by the tplyr_meta object associated with that
'cell', which provides traceability to tie a result to its source.

## Usage

``` r
get_meta_subset(x, row_id, column, add_cols = vars(USUBJID), ...)

# S3 method for class 'data.frame'
get_meta_subset(
  x,
  row_id,
  column,
  add_cols = vars(USUBJID),
  target = NULL,
  pop_data = NULL,
  ...
)

# S3 method for class 'tplyr_table'
get_meta_subset(x, row_id, column, add_cols = vars(USUBJID), ...)
```

## Arguments

- x:

  A built Tplyr table or a dataframe

- row_id:

  The row_id value of the desired cell, provided as a character string

- column:

  The result column of interest, provided as a character string

- add_cols:

  Additional columns to include in subset data.frame output

- ...:

  additional arguments

- target:

  A data frame to be subset (if not pulled from a Tplyr table)

- pop_data:

  A data frame to be subset through an anti-join (if not pulled from a
  Tplyr table)

## Value

A data.frame

## Details

If a Tplyr table is built with the `metadata=TRUE` option specified,
then metadata is assembled behind the scenes to provide traceability on
each result cell derived. The functions
[`get_meta_result()`](https://atorus-research.github.io/Tplyr/reference/get_meta_result.md)
and `get_meta_subset()` allow you to access that metadata by using an ID
provided in the row_id column and the column name of the result you'd
like to access. The purpose is of the row_id variable instead of a
simple row index is to provide a sort resistant reference of the
originating column, so the output Tplyr table can be sorted in any order
but the metadata are still easily accessible.

The `tplyr_meta` object provided a list with two elements - names and
filters. The metadata contain every column from the target data.frame of
the Tplyr table that factored into the specified result cell, and the
filters contains all the necessary filters to subset to data summarized
to create the specified result cell. `get_meta_subset()` additionally
provides a parameter to specify any additional columns you would like to
include in the returned subset data frame.

## Examples

``` r
t <- tplyr_table(mtcars, cyl) %>%
  add_layer(
    group_desc(hp)
  )


dat <- t %>% build(metadata = TRUE)

get_meta_subset(t, 'd1_1', 'var1_4', add_cols = dplyr::vars(carb))
#>                carb cyl  hp
#> Datsun 710        1   4  93
#> Merc 240D         2   4  62
#> Merc 230          2   4  95
#> Fiat 128          1   4  66
#> Honda Civic       2   4  52
#> Toyota Corolla    1   4  65
#> Toyota Corona     1   4  97
#> Fiat X1-9         1   4  66
#> Porsche 914-2     2   4  91
#> Lotus Europa      2   4 113
#> Volvo 142E        2   4 109

m <- t$metadata
dat <- t$target

get_meta_subset(t, 'd1_1', 'var1_4', add_cols = dplyr::vars(carb), target = target)
#>                carb cyl  hp
#> Datsun 710        1   4  93
#> Merc 240D         2   4  62
#> Merc 230          2   4  95
#> Fiat 128          1   4  66
#> Honda Civic       2   4  52
#> Toyota Corolla    1   4  65
#> Toyota Corona     1   4  97
#> Fiat X1-9         1   4  66
#> Porsche 914-2     2   4  91
#> Lotus Europa      2   4 113
#> Volvo 142E        2   4 109
```
