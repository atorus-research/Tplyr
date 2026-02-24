# Add variables to a tplyr_meta object

Add additional variable names to a
[`tplyr_meta()`](https://atorus-research.github.io/Tplyr/reference/tplyr_meta.md)
object.

## Usage

``` r
add_variables(meta, names)

add_filters(meta, filters)
```

## Arguments

- meta:

  A tplyr_meta object

- names:

  A list of names, providing variable names of interest. Provide as a
  list of quosures using
  [`rlang::quos()`](https://rlang.r-lib.org/reference/defusing-advanced.html)

- filters:

  A list of symbols, providing variable names of interest. Provide as a
  list of quosures using \`rlang::quos()\`

## Value

tplyr_meta object

## Examples

``` r
m <- tplyr_meta()
m <- add_variables(m, rlang::quos(a, b, c))
m <- add_filters(m, rlang::quos(a==1, b==2, c==3))
m
#> tplyr_meta: 3 names, 3 filters
#> Names:
#>      a, b, c 
#> Filters:
#>      a == 1, b == 2, c == 3 
```
