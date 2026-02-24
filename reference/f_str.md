# Create a `f_str` object

`f_str` objects are intended to be used within the function
`set_format_strings`. The `f_str` object carries information that powers
a significant amount of layer processing. The `format_string` parameter
is capable of controlling the display of a data point and decimal
precision. The variables provided in `...` control which data points are
used to populate the string formatted output.

## Usage

``` r
f_str(format_string, ..., empty = c(.overall = ""))
```

## Arguments

- format_string:

  The desired display format. X's indicate digits. On the left, the
  number of x's indicates the integer length. On the right, the number
  of x's controls decimal precision and rounding. Variables are inferred
  by any separation of the 'x' values other than a decimal.

- ...:

  The variables to be formatted using the format specified in
  `format_string`.

- empty:

  The string to display when the numeric data is not available. For desc
  layers, an unnamed character vector will populate within the provided
  format string, set to the same width as the fitted numbers. Use a
  single element character vector, with the element named '.overall' to
  instead replace the whole string.

## Value

A `f_str` object

## Details

Format strings are one of the most powerful components of 'Tplyr'.
Traditionally, converting numeric values into strings for presentation
can consume a good deal of time. Values and decimals need to align
between rows, rounding before trimming is sometimes forgotten - it can
become a tedious mess that is realistically not an important part of the
analysis being performed. 'Tplyr' makes this process as simple as we
can, while still allowing flexibility to the user.

Tplyr provides both manual and automatic decimal precision formatting.
The display of the numbers in the resulting data frame is controlled by
the `format_string` parameter. For manual precision, just like dummy
values may be presented on your mocks, integer and decimal precision is
specified by the user providing a string of 'x's for how you'd like your
numbers formatted. If you'd like 2 integers with 3 decimal places, you
specify your string as 'xx.xxx'. 'Tplyr' does the work to get the
numbers in the right place.

To take this a step further, automatic decimal precision can also be
obtained based on the collected precision within the data. When creating
tables where results vary by some parameter, different results may call
for different degrees of precision. To use automatic precision, use a
single 'a' on either the integer and decimal side. If you'd like to use
increased precision (i.e. you'd like mean to be collected precision +1),
use 'a+1'. So if you'd like both integer and and decimal precision to be
based on the data as collected, you can use a format like 'a.a' - or for
collected+1 decimal precision, 'a.a+1'. You can mix and match this with
manual formats as well, making format strings such as 'xx.a+1'.

If you want two numbers on the same line, you provide two sets of x's.
For example, if you're presenting a value like "mean (sd)" - you could
provide the string 'xx.xx (xx.xxx)', or perhaps 'a.a+1 (a.a+2). Note
that you're able to provide different integer lengths and different
decimal precision for the two values. Each format string is independent
and relates only to the format specified.

As described above, when using 'x' or 'a', any other character within
the format string will stay stationary. So for example, if your format
string is 'xx (xxx.x)', your number may format as '12 ( 34.5)'. So the
left side parenthesis stays fixed. In some displays, you may want the
parenthesis to 'hug' your number. Following this example, when allotting
3 spaces for the integer within parentheses, the parentehsis should
shift to the right, making the numbers appear '12 (34.5)'. Using
`f_str()` you can achieve this by using a capital 'X' or 'A'. For this
example, the format string would be 'xx (XXX.x)'.

There are a two rules when using 'parenthesis hugging':

- Capital letters should only be used on the integer side of a number

- A character must precede the capital letter, otherwise there's no
  character to 'hug'

The other parameters of the `f_str` call specify what values should fill
the x's. `f_str` objects are used slightly differently between different
layers. When declaring a format string within a count layer, `f_str()`
expects to see the values `n` or `distinct_n` for event or distinct
counts, `pct` or `distinct_pct` for event or distinct percentages, or
`total` or `distinct_total` for denominator calculations. Note that in
an `f_str()` for a count layer 'A' or 'a' are based on n counts, and
therefore don't make sense to use in percentages. But in descriptive
statistic layers, `f_str` parameters refer to the names of the summaries
being performed, either by built in defaults, or custom summaries
declared using
[`set_custom_summaries()`](https://atorus-research.github.io/Tplyr/reference/set_custom_summaries.md).
See
[`set_format_strings()`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md)
for some more notes about layers specific implementation.

An `f_str()` may also be used outside of a Tplyr table. The function
[`apply_formats()`](https://atorus-research.github.io/Tplyr/reference/apply_formats.md)
allows you to apply an `f_str` within the context of
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
or more generally a vectorized function.

## Valid `f_str()` Variables by Layer Type

Valid variables allowed within the `...` parameter of `f_str()` differ
by layer type.

- Count layers

  - `n`

  - `pct`

  - `total`

  - `distinct_n`

  - `distinct_pct`

  - `distinct_total`

- Shift layers

  - `n`

  - `pct`

  - `total`

- Desc layers

  - `n`

  - `mean`

  - `sd`

  - `median`

  - `var`

  - `min`

  - `max`

  - `iqr`

  - `q1`

  - `q3`

  - `missing`

  - Custom summaries created by
    [`set_custom_summaries()`](https://atorus-research.github.io/Tplyr/reference/set_custom_summaries.md)

## Examples

``` r
f_str("xx.x (xx.x)", mean, sd)
#> *** Format String ***
#> xx.x (xx.x)
#> *** vars, extracted formats, and settings ***
#> mean formated as: xx.x
#>  integer length: 2
#>  decimal length: 1
#> sd formated as: xx.x
#>  integer length: 2
#>  decimal length: 1
#> Total Format Size: 11

f_str("a.a+1 (a.a+2)", mean, sd)
#> *** Format String ***
#> a.a+1 (a.a+2)
#> *** vars, extracted formats, and settings ***
#> mean formated as: a.a+1
#>  integer length: 0
#>  decimal length: 1
#> sd formated as: a.a+2
#>  integer length: 0
#>  decimal length: 2
#> Total Format Size: 13

f_str("xx.a (xx.a+1)", mean, sd)
#> *** Format String ***
#> xx.a (xx.a+1)
#> *** vars, extracted formats, and settings ***
#> mean formated as: xx.a
#>  integer length: 2
#>  decimal length: 0
#> sd formated as: xx.a+1
#>  integer length: 2
#>  decimal length: 1
#> Total Format Size: 13

f_str("xx.x, xx.x, xx.x", q1, median, q3)
#> *** Format String ***
#> xx.x, xx.x, xx.x
#> *** vars, extracted formats, and settings ***
#> q1 formated as: xx.x
#>  integer length: 2
#>  decimal length: 1
#> median formated as: xx.x
#>  integer length: 2
#>  decimal length: 1
#> q3 formated as: xx.x
#>  integer length: 2
#>  decimal length: 1
#> Total Format Size: 16

f_str("xx (XXX.x%)", n, pct)
#> *** Format String ***
#> xx (XXX.x%)
#> *** vars, extracted formats, and settings ***
#> n formated as: xx
#>  integer length: 2
#>  decimal length: 0
#> pct formated as: (XXX.x
#>  integer length: 4
#>  decimal length: 1
#> Total Format Size: 11

f_str("a.a+1 (A.a+2)", mean, sd)
#> *** Format String ***
#> a.a+1 (A.a+2)
#> *** vars, extracted formats, and settings ***
#> mean formated as: a.a+1
#>  integer length: 0
#>  decimal length: 1
#> sd formated as: (A.a+2
#>  integer length: 0
#>  decimal length: 2
#> Total Format Size: 13
```
