# Retrieve one of Tplyr's regular expressions

This function allows you to extract important regular expressions used
inside Tplyr.

## Usage

``` r
get_tplyr_regex(rx = c("format_string", "format_group", "number_group"))
```

## Arguments

- rx:

  A character string with either the value 'format_string' or
  'format_group'

## Value

A regular expression object

## Details

There are three important regular expressions used within Tplyr. The
format_string expression is the expression to parse format strings. This
is what is used to make sense out of strings like 'xx (XX.x%)' or 'a+1
(A.a+2)' by inferring what the user is specifying about number
formatting.

The 'format_group' regex is the opposite of this, and when given a
string of numbers, such as ' 5 (34%) \[9\]' will return the separate
segments of numbers broken into their format groups, which in this
example would be ' 5', '(34%)', and '\[9\]'. Lastly, the 'number_group'
regex has a similar application to the 'format_group' regex, but targets
only numbers

## Examples

``` r
get_tplyr_regex('format_string')
#> [1] "(a(\\+\\d+)?|(\\S+)A(\\+\\d+)?|(\\S+)X+|x+)(\\.([A|a](\\+\\d+)?|[X|x]+)?)?"
#> attr(,"options")
#> attr(,"options")$case_insensitive
#> [1] FALSE
#> 
#> attr(,"options")$comments
#> [1] FALSE
#> 
#> attr(,"options")$dotall
#> [1] FALSE
#> 
#> attr(,"options")$multiline
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "stringr_regex"   "stringr_pattern" "character"      

get_tplyr_regex('format_group')
#> [1] "[^\\s\\d]*\\s*(\\-?\\d+(\\.\\d+)?)\\S*"
#> attr(,"options")
#> attr(,"options")$case_insensitive
#> [1] FALSE
#> 
#> attr(,"options")$comments
#> [1] FALSE
#> 
#> attr(,"options")$dotall
#> [1] FALSE
#> 
#> attr(,"options")$multiline
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "stringr_regex"   "stringr_pattern" "character"      

get_tplyr_regex('number_group')
#> [1] "-?(?:\\d*\\.\\d+|\\d+)"
#> attr(,"options")
#> attr(,"options")$case_insensitive
#> [1] FALSE
#> 
#> attr(,"options")$comments
#> [1] FALSE
#> 
#> attr(,"options")$dotall
#> [1] FALSE
#> 
#> attr(,"options")$multiline
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "stringr_regex"   "stringr_pattern" "character"      
```
