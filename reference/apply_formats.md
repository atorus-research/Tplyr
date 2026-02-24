# Apply Format Strings outside of a Tplyr table

The `f_str` object in Tplyr is used to drive formatting of the outputs
strings within a Tplyr table. This function allows a user to use the
same interface to apply formatted string on any data frame within a
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
context.

## Usage

``` r
apply_formats(format_string, ..., empty = c(.overall = ""))
```

## Arguments

- format_string:

  The desired display format. X's indicate digits. On the left, the
  number of x's indicates the integer length. On the right, the number
  of x's controls decimal precision and rounding. Variables are inferred
  by any separation of the 'x' values other than a decimal.

- ...:

  The variables to be formatted using the format specified in
  `format_string`. These must be numeric variables.

- empty:

  The string to display when the numeric data is not available. Use a
  single element character vector, with the element named '.overall' to
  instead replace the whole string.

## Value

Character vector of formatted values

## Details

Note that auto-precision is not currently supported within
`apply_formats()`

## Examples

``` r
library(dplyr)

mtcars %>%
  head() %>%
  mutate(
    fmt_example = apply_formats('xxx (xx.x)', hp, wt)
  )
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
#>                   fmt_example
#> Mazda RX4          110 ( 2.6)
#> Mazda RX4 Wag      110 ( 2.9)
#> Datsun 710          93 ( 2.3)
#> Hornet 4 Drive     110 ( 3.2)
#> Hornet Sportabout  175 ( 3.4)
#> Valiant            105 ( 3.5)
```
