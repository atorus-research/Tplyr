# Combine existing treatment groups for summary

Summary tables often present individual treatment groups, but may
additionally have a "Treatment vs. Placebo" or "Total" group added to
show grouped summary statistics or counts. This set of functions offers
an interface to add these groups at a table level and be consumed by
subsequent layers.

## Usage

``` r
add_treat_grps(table, ...)

add_total_group(table, group_name = "Total")

treat_grps(table)
```

## Arguments

- table:

  A `tplyr_table` object

- ...:

  A named vector where names will become the new treatment group names,
  and values will be used to construct those treatment groups

- group_name:

  The treatment group name used for the constructed 'Total' group

## Value

The modified table object

## Details

`add_treat_grps` allows you to specify specific groupings. This is done
by supplying named arguments, where the name becomes the new treatment
group's name, and those treatment groups are made up of the argument's
values.

`add_total_group` is a simple wrapper around `add_treat_grps`. Instead
of producing custom groupings, it produces a "Total" group by the
supplied name, which defaults to "Total". This "Total" group is made up
of all existing treatment groups within the population dataset.

Note that when using `add_treat_grps` or
[`add_total_row()`](https://atorus-research.github.io/Tplyr/reference/add_total_row.md)
with
[`set_pop_data()`](https://atorus-research.github.io/Tplyr/reference/pop_data.md),
you should call
[`add_total_row()`](https://atorus-research.github.io/Tplyr/reference/add_total_row.md)
AFTER calling
[`set_pop_data()`](https://atorus-research.github.io/Tplyr/reference/pop_data.md),
otherwise there is potential for unexpected behaivior with treatment
groups.

The function `treat_grps` allows you to see the custom treatment groups
available in your `tplyr_table` object

## Examples

``` r
tab <- tplyr_table(iris, Species)

# A custom group
add_treat_grps(tab, "Not Setosa" = c("versicolor", "virginica"))
#> *** tplyr_table ***
#> Target (data.frame):
#>  Name:  iris
#>  Rows:  150
#>  Columns:  5 
#> treat_var variable (quosure)
#>  Species
#> header_n:  header groups
#> treat_grps groupings (list)
#>  Not Setosa
#> Table Columns (cols):
#> where: TRUE
#> Number of layer(s): 0
#> layer_output: <Table Not Built Yet>

# Add a total group
add_total_group(tab)
#> *** tplyr_table ***
#> Target (data.frame):
#>  Name:  iris
#>  Rows:  150
#>  Columns:  5 
#> treat_var variable (quosure)
#>  Species
#> header_n:  header groups
#> treat_grps groupings (list)
#>  Not Setosa
#>  Total
#> Table Columns (cols):
#> where: TRUE
#> Number of layer(s): 0
#> layer_output: <Table Not Built Yet>

treat_grps(tab)
#> $`Not Setosa`
#> [1] "versicolor" "virginica" 
#> 
#> $Total
#> [1] "setosa"     "versicolor" "virginica" 
#> 
# Returns:
# $`Not Setosa`
#[1] "versicolor" "virginica"
#
#$Total
#[1] "setosa"     "versicolor" "virginica"
```
