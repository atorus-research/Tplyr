# Reformat strings with leading whitespace for HTML

Reformat strings with leading whitespace for HTML

## Usage

``` r
replace_leading_whitespace(x, tab_width = 4)
```

## Arguments

- x:

  Target string

- tab_width:

  Number of spaces to compensate for tabs

## Value

String with &nbsp; replaced for leading whitespace

## Examples

``` r
x <- c(" Hello there", "  Goodbye Friend ",  "\tNice to meet you",
"  \t What are you up to? \t \t ")
replace_leading_whitespace(x)
#> [1] "&nbsp;Hello there"                                                   
#> [2] "&nbsp;&nbsp;Goodbye Friend "                                         
#> [3] "&nbsp;&nbsp;&nbsp;&nbsp;Nice to meet you"                            
#> [4] "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;What are you up to? \t \t "

replace_leading_whitespace(x, tab=2)
#> [1] "&nbsp;Hello there"                                       
#> [2] "&nbsp;&nbsp;Goodbye Friend "                             
#> [3] "&nbsp;&nbsp;Nice to meet you"                            
#> [4] "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;What are you up to? \t \t "
```
