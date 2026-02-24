# Add an anti-join onto a tplyr_meta object

An anti-join allows a tplyr_meta object to refer to data that should be
extracted from a separate dataset, like the population data of a Tplyr
table, that is unavailable in the target dataset. The primary use case
for this is the presentation of missing subjects, which in a Tplyr table
is presented using the function
[`add_missing_subjects_row()`](https://atorus-research.github.io/Tplyr/reference/add_missing_subjects_row.md).
The missing subjects themselves are not present in the target data, and
are thus only available in the population data. The `add_anti_join()`
function allows you to provide the meta information relevant to the
population data, and then specify the `on` variable that should be used
to join with the target dataset and find the values present in the
population data that are missing from the target data.

## Usage

``` r
add_anti_join(meta, join_meta, on)
```

## Arguments

- meta:

  A tplyr_meta object referring to the target data

- join_meta:

  A tplyr_meta object referring to the population data

- on:

  A list of quosures containing symbols - most likely set to USUBJID.

## Value

A tplyr_meta object

## Examples

``` r
tm <- tplyr_meta(
  rlang::quos(TRT01A, SEX, ETHNIC, RACE),
  rlang::quos(TRT01A == "Placebo", TRT01A == "SEX", ETHNIC == "HISPANIC OR LATINO")
)

tm %>%
  add_anti_join(
    tplyr_meta(
      rlang::quos(TRT01A, ETHNIC),
      rlang::quos(TRT01A == "Placebo", ETHNIC == "HISPANIC OR LATINO")
    ),
    on = rlang::quos(USUBJID)
  )
#> tplyr_meta: 4 names, 3 filters
#> Names:
#>      TRT01A, SEX, ETHNIC, RACE 
#> Filters:
#>      TRT01A == "Placebo", TRT01A == "SEX", ETHNIC == "HISPANIC OR LATINO" 
#> Anti-join:
#>     Join Meta:
#>         tplyr_meta: 2 names, 2 filters
#>         Names:
#>              TRT01A, ETHNIC 
#>         Filters:
#>              TRT01A == "Placebo", ETHNIC == "HISPANIC OR LATINO" 
#>     On:
#>         USUBJID 
```
