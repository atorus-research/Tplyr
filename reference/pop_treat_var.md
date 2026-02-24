# Return or set pop_treat_var binding

The treatment variable used in the target data may be different than the
variable within the population dataset. `set_pop_treat_var` allows you
to change this.

## Usage

``` r
pop_treat_var(table)

set_pop_treat_var(table, pop_treat_var)
```

## Arguments

- table:

  A `tplyr_table` object

- pop_treat_var:

  Variable containing treatment group assignments within the `pop_data`
  binding. Supply unquoted.

## Value

For `tplyr_pop_treat_var` the pop_treat_var binding of the `tplyr_table`
object. For `set_tplyr_pop_treat_var` the modified object.

## Examples

``` r
tab <- tplyr_table(iris, Species)

pop_data(tab) <- mtcars
set_pop_treat_var(tab, mpg)
#> *** tplyr_table ***
#> Target (data.frame):
#>  Name:  iris
#>  Rows:  150
#>  Columns:  5 
#> pop_data (data.frame)
#>  Name:  value 
#>  Rows:  32 
#>  Columns:  11 
#> treat_var variable (quosure)
#>  Speciespop_treat_var variable (quosure)
#>  mpg
#> 
#> header_n:  header groups
#> treat_grps groupings (list)
#> Table Columns (cols):
#> where: TRUE
#> Number of layer(s): 0
#> layer_output: <Table Not Built Yet>
```
