# Set custom summaries to be performed within a descriptive statistics layer

This function allows a user to define custom summaries to be performed
in a call to
[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html).
A custom summary by the same name as a default summary will override the
default. This allows the user to override the default behavior of
summaries built into 'Tplyr', while also adding new desired summary
functions.

## Usage

``` r
set_custom_summaries(e, ...)
```

## Arguments

- e:

  `desc` layer on which the summaries should be bound

- ...:

  Named parameters containing syntax to be used in a call to
  [`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)

## Value

Binds a variable `custom_summaries` to the specified layer

## Details

When programming the logic of the summary function, use the variable
name `.var` to within your summary functions. This allows you apply the
summary function to each variable when multiple target variables are
declared.

An important, yet not immediately obvious, part of using
`set_custom_summaries` is to understand the link between the named
parameters you set in `set_custom_summaries` and the names called in
[`f_str`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
objects within
[`set_format_strings`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md).
In
[`f_str`](https://atorus-research.github.io/Tplyr/reference/f_str.md),
after you supply the string format you'd like your numbers to take, you
specify the summaries that fill those strings.

When you go to set your format strings, the name you use to declare a
summary in `set_custom_summaries` is the same name that you use in your
[`f_str`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
call. This is necessary because
[`set_format_strings`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md)
needs some means of putting two summaries in the same value, and setting
a row label for the summary being performed.

Review the examples to see this put into practice. Note the relationship
between the name created in `set_custom_summaries` and the name used in
[`set_format_strings`](https://atorus-research.github.io/Tplyr/reference/set_format_strings.md)
within the
[`f_str`](https://atorus-research.github.io/Tplyr/reference/f_str.md)
call

## Examples

``` r
#Load in pipe
library(magrittr)

tplyr_table(iris, Species) %>%
  add_layer(
    group_desc(Sepal.Length, by = "Sepal Length") %>%
      set_custom_summaries(
        geometric_mean = exp(sum(log(.var[.var > 0]),
                                     na.rm=TRUE) / length(.var))
      ) %>%
      set_format_strings(
        'Geometric Mean' = f_str('xx.xx', geometric_mean)
      )
  ) %>%
  build()
#> # A tibble: 1 × 8
#>   row_label1   row_label2     var1_setosa var1_versicolor var1_virginica
#>   <chr>        <chr>          <chr>       <chr>           <chr>         
#> 1 Sepal Length Geometric Mean " 4.99"     " 5.91"         " 6.56"       
#> # ℹ 3 more variables: ord_layer_index <int>, ord_layer_1 <int>,
#> #   ord_layer_2 <int>
```
