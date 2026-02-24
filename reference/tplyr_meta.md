# Tplyr Metadata Object

If a Tplyr table is built with the \`metadata=TRUE\` option specified,
then metadata is assembled behind the scenes to provide traceability on
each result cell derived. The functions \`get_meta_result()\` and
\`get_meta_subset()\` allow you to access that metadata by using an ID
provided in the row_id column and the column name of the result you'd
like to access. The purpose is of the row_id variable instead of a
simple row index is to provide a sort resistant reference of the
originating column, so the output Tplyr table can be sorted in any order
but the metadata are still easily accessible.

## Usage

``` r
tplyr_meta(names = list(), filters = exprs())
```

## Arguments

- names:

  List of symbols

- filters:

  List of expressions

## Value

tplyr_meta object

## Details

The \`tplyr_meta\` object provided a list with two elements - names and
filters. The names contain every column from the target data.frame of
the Tplyr table that factored into the specified result cell, and the
filters contains all the necessary filters to subset the target data to
create the specified result cell. \`get_meta_subset()\` additionally
provides a parameter to specify any additional columns you would like to
include in the returned subset data frame.

## Examples

``` r
tplyr_meta(
   names = rlang::quos(x, y, z),
   filters = rlang::quos(x == 1, y==2, z==3)
 )
#> tplyr_meta: 3 names, 3 filters
#> Names:
#>      x, y, z 
#> Filters:
#>      x == 1, y == 2, z == 3 
```
