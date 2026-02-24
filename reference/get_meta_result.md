# Extract the result metadata of a Tplyr table

Given a row_id value and a result column, this function will return the
tplyr_meta object associated with that 'cell'.

## Usage

``` r
get_meta_result(x, row_id, column, ...)
```

## Arguments

- x:

  A built Tplyr table or a dataframe

- row_id:

  The row_id value of the desired cell, provided as a character string

- column:

  The result column of interest, provided as a character string

- ...:

  additional arguments

## Value

A tplyr_meta object

## Details

If a Tplyr table is built with the `metadata=TRUE` option specified,
then metadata is assembled behind the scenes to provide traceability on
each result cell derived. The functions `get_meta_result()` and
[`get_meta_subset()`](https://atorus-research.github.io/Tplyr/reference/get_meta_subset.md)
allow you to access that metadata by using an ID provided in the row_id
column and the column name of the result you'd like to access. The
purpose is of the row_id variable instead of a simple row index is to
provide a sort resistant reference of the originating column, so the
output Tplyr table can be sorted in any order but the metadata are still
easily accessible.

The `tplyr_meta` object provided a list with two elements - names and
filters. The metadata contain every column from the target data.frame of
the Tplyr table that factored into the specified result cell, and the
filters contains all the necessary filters to subset to data summarized
to create the specified result cell.
[`get_meta_subset()`](https://atorus-research.github.io/Tplyr/reference/get_meta_subset.md)
additionally provides a parameter to specify any additional columns you
would like to include in the returned subset data frame.

## Examples

``` r
t <- tplyr_table(mtcars, cyl) %>%
  add_layer(
    group_desc(hp)
  )

dat <- t %>% build(metadata = TRUE)

get_meta_result(t, 'd1_1', 'var1_4')
#> tplyr_meta: 2 names, 3 filters
#> Names:
#>      cyl, hp 
#> Filters:
#>      cyl == c("4"), TRUE, TRUE 

m <- t$metadata
dat <- t$target

get_meta_result(t, 'd1_1', 'var1_4')
#> tplyr_meta: 2 names, 3 filters
#> Names:
#>      cyl, hp 
#> Filters:
#>      cyl == c("4"), TRUE, TRUE 
```
