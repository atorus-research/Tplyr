# Conditional reformatting of a pre-populated string of numbers

This function allows you to conditionally re-format a string of numbers
based on a numeric value within the string itself. By selecting a
"format group", which is targeting a specific number within the string,
a user can establish a condition upon which a provided replacement
string can be used. Either the entire replacement can be used to replace
the entire string, or the replacement text can refill the "format group"
while preserving the original width and alignment of the target string.

## Usage

``` r
apply_conditional_format(
  string,
  format_group,
  condition,
  replacement,
  full_string = FALSE
)
```

## Arguments

- string:

  Target character vector where text may be replaced

- format_group:

  An integer representing the targeted numeric field within the string,
  numbered from left to right

- condition:

  An expression, using the variable name 'x' as the target variable
  within the condition

- replacement:

  A string to use as the replacement value

- full_string:

  TRUE if the full string should be replaced, FALSE if the replacement
  should be done within the format group

## Value

A character vector

## Examples

``` r
string <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)")

apply_conditional_format(string, 2, x == 0, " 0        ", full_string=TRUE)
#> [1] " 0        " " 8  (9.3%)" "78 (90.7%)"

apply_conditional_format(string, 2, x < 1, "(<1%)")
#> [1] " 0   (<1%)" " 8  (9.3%)" "78 (90.7%)"
```
