# Extract format group strings or numbers

These functions allow you to extract segments of information from within
a result string by targetting specific format groups.
`str_extract_fmt_group()` allows you to pull out the individual format
group string, while `str_extract_num()` allows you to pull out that
specific numeric result.

## Usage

``` r
str_extract_fmt_group(string, format_group)

str_extract_num(string, format_group)
```

## Arguments

- string:

  A string of number results from which to extract format groups

- format_group:

  An integer representing format group that should be extracted

## Value

A character vector

## Details

Format groups refer to individual segments of a string. For example,
given the string ' 5 (34.4%) \[9\]', there are three separate format
groups, which are ' 5', '(34.4%)', and '\[9\]'.

## Examples

``` r
string <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)",  "-1 (-.56, .75) -523%, 56 | -34")

str_extract_fmt_group(string, 2)
#> [1] "(0.0%)"  "(9.3%)"  "(90.7%)" "(-.56," 

str_extract_num(string, 2)
#> [1]  0.00  9.30 90.70 -0.56
```
