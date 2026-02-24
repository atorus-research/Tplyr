# Return or set the treatment variable binding

Return or set the treatment variable binding

## Usage

``` r
treat_var(table)

set_treat_var(table, treat_var)
```

## Arguments

- table:

  A `tplyr_table` object to set or return treatment variable the table
  is split by.

- treat_var:

  Variable containing treatment group assignments. Supply unquoted.

## Value

For `tplyr_treat_var` the treat_var binding of the `tplyr_table` object.
For `set_tplyr_treat_var` the modified object.

## Examples

``` r
tab <- tplyr_table(mtcars, cyl)

set_treat_var(tab, gear)
#> *** tplyr_table ***
#> Target (data.frame):
#>  Name:  mtcars
#>  Rows:  32
#>  Columns:  11 
#> treat_var variable (quosure)
#>  gearpop_treat_var variable (quosure)
#>  cyl
#> 
#> header_n:  header groups
#> treat_grps groupings (list)
#> Table Columns (cols):
#> where: TRUE
#> Number of layer(s): 0
#> layer_output: <Table Not Built Yet>
```
