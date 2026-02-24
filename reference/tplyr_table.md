# Create a Tplyr table object

The `tplyr_table` object is the main container upon which a Tplyr table
is constructed. Tplyr tables are made up of one or more layers. Each
layer contains an instruction for a summary to be performed. The
`tplyr_table` object contains those layers, and the general data,
metadata, and logic necessary.

## Usage

``` r
tplyr_table(target, treat_var, where = TRUE, cols = vars())
```

## Arguments

- target:

  Dataset upon which summaries will be performed

- treat_var:

  Variable containing treatment group assignments. Supply unquoted.

- where:

  A general subset to be applied to all layers. Supply as programming
  logic (i.e. x \< 5 & y == 10)

- cols:

  A grouping variable to summarize data by column (in addition to
  treat_var). Provide multiple column variables by using
  [`vars`](https://dplyr.tidyverse.org/reference/vars.html)

## Value

A `tplyr_table` object

## Details

When a `tplyr_table` is created, it will contain the following bindings:

- target - The dataset upon which summaries will be performed

- pop_data - The data containing population information. This defaults
  to the target dataset

- cols - A categorical variable to present summaries grouped by column
  (in addition to treat_var)

- table_where - The `where` parameter provided, used to subset the
  target data

- treat_var - Variable used to distinguish treatment groups.

- header_n - Default header N values based on `treat_var`

- pop_treat_var - The treatment variable for `pop_data` (if different)

- layers - The container for individual layers of a `tplyr_table`

- treat_grps - Additional treatment groups to be added to the summary
  (i.e. Total)

`tplyr_table` allows you a basic interface to instantiate the object.
Modifier functions are available to change individual parameters catered
to your analysis. For example, to add a total group, you can use the
[`add_total_group`](https://atorus-research.github.io/Tplyr/reference/treat_grps.md).

In future releases, we will provide vignettes to fully demonstrate these
capabilities.

## Examples

``` r
tab <- tplyr_table(iris, Species, where = Sepal.Length < 5.8)
```
