# Set precision data

In some cases, there may be organizational standards surrounding decimal
precision. For example, there may be a specific standard around the
representation of precision relating to lab results. As such,
`set_precision_data()` provides an interface to provide integer and
decimal precision from an external data source.

## Usage

``` r
set_precision_data(layer, prec, default = c("error", "auto"))
```

## Arguments

- layer:

  A `tplyr_layer` object

- prec:

  A dataframe following the structure specified in the function details

- default:

  Handling of unspecified by variable groupings. Defaults to 'error'.
  Set to 'auto' to automatically infer any missing groups.

## Details

The ultimate behavior of this feature is just that of the existing auto
precision method, except that the precision is specified in the provided
precision dataset rather than inferred from the source data. At a
minimum, the precision dataset must contain the integer variables
`max_int` and `max_dec`. If by variables are provided, those variables
must be available in the layer by variables.

When the table is built, by default Tplyr will error if the precision
dataset is missing by variable groupings that exist in the target
dataset. This can be overriden using the `default` parameter. If
`default` is set to "auto", any missing values will be automatically
inferred from the source data.

## Examples

``` r
prec <- tibble::tribble(
  ~vs, ~max_int, ~max_dec,
  0,        1,        1,
  1,        2,        2
)

tplyr_table(mtcars, gear) %>%
  add_layer(
    group_desc(wt, by = vs) %>%
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd)
      ) %>%
      set_precision_data(prec) %>%
      set_precision_on(wt)
  ) %>%
  build()
#> # A tibble: 2 × 8
#>   row_label1 row_label2 var1_3         var1_4 var1_5 ord_layer_index ord_layer_1
#>   <chr>      <chr>      <chr>          <chr>  <chr>            <int>       <dbl>
#> 1 0          Mean (SD)  "4.10 (0.768)" "2.75… "2.91…               1           1
#> 2 1          Mean (SD)  " 3.047 ( 0.5… " 2.5… " 1.5…               1           2
#> # ℹ 1 more variable: ord_layer_2 <int>
```
