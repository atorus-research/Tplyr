# Attach column headers to a Tplyr output

When working with 'huxtable' tables, column headers can be controlled as
if they are rows in the data frame. `add_column_headers` eases the
process of introducing these headers.

## Usage

``` r
add_column_headers(.data, s, header_n = NULL)
```

## Arguments

- .data:

  The data.frame/tibble on which the headers shall be attached

- s:

  The text containing the intended header string

- header_n:

  A header_n or generic data.frame to use for binding count values. This
  is required if you are using the token replacement.

## Value

A data.frame with the processed header string elements attached as the
top rows

## Details

Headers are created by providing a single string. Columns are specified
by delimitting each header with a '\|' symbol. Instead of specifying the
destination of each header, `add_column_headers` assumes that you have
organized the columns of your data frame before hand. This means that
after you use
[`Tplyr::build()`](https://atorus-research.github.io/Tplyr/reference/build.md),
if you'd like to reorganize the default column order (which is simply
alphabetical), simply pass the build output to a
[`dplyr::select`](https://dplyr.tidyverse.org/reference/select.html) or
[`dplyr::relocate`](https://dplyr.tidyverse.org/reference/relocate.html)
statement before passing into `add_column_headers`.

Spanning headers are also supported. A spanning header is an overarching
header that sits across multiple columns. Spanning headers are
introduced to `add_column_header` by providing the spanner text (i.e.
the text that you'd like to sit in the top row), and then the spanned
text (the bottom row) within curly brackets ('{}). For example, take the
iris dataset. We have the names:

`"Sepal.Length" "Sepal.Width" "Petal.Length" "Petal.Width" "Species"`

If we wanted to provide a header string for this dataset, with spanners
to help with categorization of the variables, we could provide the
following string:

`"Sepal {Length | Width} | Petal {Length | Width} | Species"`

## Important note

Make sure you are aware of the order of your variables prior to passing
in to `add_column_headers`. The only requirement is that the number of
column match. The rest is up to you.

## Development notes

There are a few features of `add_column_header` that are intended but
not yet supported:

- Nested spanners are not yet supported. Only a spanning row and a
  bottom row can currently be created

- Different delimiters and indicators for a spanned group may be used in
  the future. The current choices were intuitive, but based on feedback
  it could be determined that less common characters may be necessary.

## Token Replacement

This function has support for reading values from the header_n object in
a Tplyr table and adding them in the column headers. Note: The order of
the parameters passed in the token is important. They should be first
the treatment variable then any `cols` variables in the order they were
passed in the table construction.

Use a double asterisk "\*\*" at the begining to start the token and
another double asterisk to close it. You can separate column parameters
in the token with a single underscore. For example,
\*\*group1_flag2_param3\*\* will pull the count from the header_n
binding for group1 in the `treat_var`, flag2 in the first `cols`
argument, and param3 in the second `cols` argument.

You can pass fewer arguments in the token to get the sum of multiple
columns. For example, \*\*group1\*\* would get the sum of the group1
treat_var, and all cols from the header_n.

## Examples

``` r
# Load in pipe
library(magrittr)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
header_string <- "Sepal {Length | Width} | Petal {Length | Width} | Species"

iris2 <- iris %>%
  mutate_all(as.character)

iris2 %>% add_column_headers(header_string)
#> # A tibble: 152 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species  
#>    <chr>        <chr>       <chr>        <chr>       <chr>    
#>  1 Sepal        ""          Petal        ""          ""       
#>  2 Length       "Width"     Length       "Width"     "Species"
#>  3 5.1          "3.5"       1.4          "0.2"       "setosa" 
#>  4 4.9          "3"         1.4          "0.2"       "setosa" 
#>  5 4.7          "3.2"       1.3          "0.2"       "setosa" 
#>  6 4.6          "3.1"       1.5          "0.2"       "setosa" 
#>  7 5            "3.6"       1.4          "0.2"       "setosa" 
#>  8 5.4          "3.9"       1.7          "0.4"       "setosa" 
#>  9 4.6          "3.4"       1.4          "0.3"       "setosa" 
#> 10 5            "3.4"       1.5          "0.2"       "setosa" 
#> # ℹ 142 more rows

# Example with counts
mtcars2 <- mtcars %>%
  mutate_all(as.character)

t <- tplyr_table(mtcars2, vs, cols = am) %>%
  add_layer(
    group_count(cyl)
  )

b_t <- build(t) %>%
  mutate_all(as.character)

count_string <- paste0(" | V N=**0** {auto N=**0_0** | man N=**0_1**} |",
                       " S N=**1** {auto N=**1_0** | man N=**1_1**} | | ")

add_column_headers(b_t, count_string, header_n(t))
#> # A tibble: 5 × 7
#>   row_label1 var1_0_0     var1_0_1 var1_1_0 var1_1_1 ord_layer_index ord_layer_1
#>   <chr>      <chr>        <chr>    <chr>    <chr>    <chr>           <chr>      
#> 1 ""         "V N=18"     ""       "S N=14" ""       ""              ""         
#> 2 ""         "auto N=12"  "man N=… "auto N… "man N=… ""              ""         
#> 3 "4"        " 0 (  0.0%… " 1 ( 1… " 3 ( 4… " 7 (10… "1"             "1"        
#> 4 "6"        " 0 (  0.0%… " 3 ( 5… " 4 ( 5… " 0 (  … "1"             "2"        
#> 5 "8"        "12 (100.0%… " 2 ( 3… " 0 (  … " 0 (  … "1"             "3"        
```
