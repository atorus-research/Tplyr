# Return or set header_n binding

The \`header_n()\` functions can be used to automatically pull the
header_n derivations from the table or change them for future use.

## Usage

``` r
header_n(table)

header_n(x) <- value

set_header_n(table, value)
```

## Arguments

- table:

  A `tplyr_table` object

- x:

  A `tplyr_table` object

- value:

  A data.frame with columns with the treatment variable, column
  variabes, and a variable with counts named 'n'.

- header_n:

  A data.frame with columns with the treatment variable, column
  variabes, and a variable with counts named 'n'.

## Value

For `tplyr_header_n` the header_n binding of the `tplyr_table` object.
For `tplyr_header_n<-` and `set_tplyr_header_n` the modified object.

## Details

The \`header_n\` object is created by Tplyr when a table is built and
intended to be used by the \`add_column_headers()\` function when
displaying table level population totals. These methods are intended to
be used for calling the population totals calculated by Tplyr, and to
overwrite them if a user chooses to.

If you have a need to change the header Ns that appear in your table
headers, say you know you are working with a subset of the data that
doesn't represent the totals, you can replace the data used with
\`set_header_n()\`.

## Examples

``` r
tab <- tplyr_table(mtcars, gear)

header_n(tab) <- data.frame(
  gear = c(3, 4, 5),
  n = c(10, 15, 45)
)


```
