# Collapse row labels into a single column

This is a generalized post processing function that allows you to take
groups of by variables and collapse them into a single column. Repeating
values are split into separate rows, and for each level of nesting, a
specified indentation level can be applied.

## Usage

``` r
collapse_row_labels(x, ..., indent = "  ", target_col = row_label)
```

## Arguments

- x:

  Input data frame

- ...:

  Row labels to be collapsed

- indent:

  Indentation string to be used, which is multiplied at each indentation
  level

- target_col:

  The desired name of the output column containing collapsed row labels

## Value

data.frame with row labels collapsed into a single column

## Examples

``` r
x <- tibble::tribble(
~row_label1, ~row_label2, ~row_label3, ~row_label4, ~var1,
  "A",         "C",         "G",         "M",        1L,
  "A",         "C",         "G",         "N",        2L,
  "A",         "C",         "H",         "O",        3L,
  "A",         "D",         "H",         "P",        4L,
  "A",         "D",         "I",         "Q",        5L,
  "A",         "D",         "I",         "R",        6L,
  "B",         "E",         "J",         "S",        7L,
  "B",         "E",         "J",         "T",        8L,
  "B",         "E",         "K",         "U",        9L,
  "B",         "F",         "K",         "V",        10L,
  "B",         "F",         "L",         "W",        11L
)


collapse_row_labels(x, row_label1, row_label2, row_label3, row_label4)
#> # A tibble: 35 × 2
#>    row_label  var1
#>    <chr>     <int>
#>  1 "A"          NA
#>  2 "  C"        NA
#>  3 "    G"      NA
#>  4 "      M"     1
#>  5 "      N"     2
#>  6 "A"          NA
#>  7 "  C"        NA
#>  8 "    H"      NA
#>  9 "      O"     3
#> 10 "A"          NA
#> # ℹ 25 more rows

collapse_row_labels(x, row_label1, row_label2, row_label3)
#> # A tibble: 19 × 3
#>    row_label row_label4  var1
#>    <chr>     <chr>      <int>
#>  1 "A"       ""            NA
#>  2 "  C"     ""            NA
#>  3 "    G"   "M"            1
#>  4 "    G"   "N"            2
#>  5 "    H"   "O"            3
#>  6 "A"       ""            NA
#>  7 "  D"     ""            NA
#>  8 "    H"   "P"            4
#>  9 "    I"   "Q"            5
#> 10 "    I"   "R"            6
#> 11 "B"       ""            NA
#> 12 "  E"     ""            NA
#> 13 "    J"   "S"            7
#> 14 "    J"   "T"            8
#> 15 "    K"   "U"            9
#> 16 "B"       ""            NA
#> 17 "  F"     ""            NA
#> 18 "    K"   "V"           10
#> 19 "    L"   "W"           11

collapse_row_labels(x, row_label1, row_label2, indent = "    ", target_col = rl)
#> # A tibble: 13 × 4
#>    rl      row_label3 row_label4  var1
#>    <chr>   <chr>      <chr>      <int>
#>  1 "A"     ""         ""            NA
#>  2 "    C" "G"        "M"            1
#>  3 "    C" "G"        "N"            2
#>  4 "    C" "H"        "O"            3
#>  5 "    D" "H"        "P"            4
#>  6 "    D" "I"        "Q"            5
#>  7 "    D" "I"        "R"            6
#>  8 "B"     ""         ""            NA
#>  9 "    E" "J"        "S"            7
#> 10 "    E" "J"        "T"            8
#> 11 "    E" "K"        "U"            9
#> 12 "    F" "K"        "V"           10
#> 13 "    F" "L"        "W"           11
```
