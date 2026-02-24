# Trigger the execution of the `tplyr_table`

The functions used to assemble a `tplyr_table` object and each of the
layers do not trigger the processing of any data. Rather, a lazy
execution style is used to allow you to construct your table and then
explicitly state when the data processing should happen. `build`
triggers this event.

## Usage

``` r
build(x, metadata = FALSE)
```

## Arguments

- x:

  A `tplyr_table` object

- metadata:

  Trigger to build metadata. Defaults to FALSE

## Value

An executed `tplyr_table`

## Details

When the `build` command is executed, all of the data processing
commences. Any pre-processing necessary within the table environment
takes place first. Next, each of the layers begins executing. Once the
layers complete executing, the output of each layer is stacked into the
resulting data frame.

Once this process is complete, any post-processing necessary within the
table environment takes place, and the final output can be delivered.
Metadata and traceability information are kept within each of the layer
environments, which allows an investigation into the source of the
resulting datapoints. For example, numeric data from any summaries
performed is maintained and accessible within a layer using
[`get_numeric_data`](https://atorus-research.github.io/Tplyr/reference/get_numeric_data.md).

The \`metadata\` option of build will trigger the construction of
traceability metadata for the constructed data frame. Essentially, for
every "result" that Tplyr produces, Tplyr can also generate the steps
necessary to obtain the source data which produced that result from the
input. For more information, see vignette("metadata").

## See also

tplyr_table, tplyr_layer, add_layer, add_layers, layer_constructors

## Examples

``` r
# Load in Pipe
library(magrittr)

tplyr_table(iris, Species) %>%
  add_layer(
    group_desc(Sepal.Length, by = "Sepal Length")
  ) %>%
  add_layer(
    group_desc(Sepal.Width, by = "Sepal Width")
  ) %>%
  build()
#> # A tibble: 12 × 8
#>    row_label1   row_label2 var1_setosa    var1_versicolor var1_virginica
#>    <chr>        <chr>      <chr>          <chr>           <chr>         
#>  1 Sepal Length n          " 50"          " 50"           " 50"         
#>  2 Sepal Length Mean (SD)  "5.01 (0.352)" "5.94 (0.516)"  "6.59 (0.636)"
#>  3 Sepal Length Median     "5.00"         "5.90"          "6.50"        
#>  4 Sepal Length Q1, Q3     "4.80, 5.20"   "5.60, 6.30"    "6.23, 6.90"  
#>  5 Sepal Length Min, Max   "4.3, 5.8"     "4.9, 7.0"      "4.9, 7.9"    
#>  6 Sepal Length Missing    "  0"          "  0"           "  0"         
#>  7 Sepal Width  n          " 50"          " 50"           " 50"         
#>  8 Sepal Width  Mean (SD)  "3.43 (0.379)" "2.77 (0.314)"  "2.97 (0.322)"
#>  9 Sepal Width  Median     "3.40"         "2.80"          "3.00"        
#> 10 Sepal Width  Q1, Q3     "3.20, 3.68"   "2.52, 3.00"    "2.80, 3.18"  
#> 11 Sepal Width  Min, Max   "2.3, 4.4"     "2.0, 3.4"      "2.2, 3.8"    
#> 12 Sepal Width  Missing    "  0"          "  0"           "  0"         
#> # ℹ 3 more variables: ord_layer_index <int>, ord_layer_1 <int>,
#> #   ord_layer_2 <int>
```
