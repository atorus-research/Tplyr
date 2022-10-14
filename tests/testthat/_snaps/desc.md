# Desc layer clauses with invalid syntax give informative error

    group_desc `where` condition `bad == code` is invalid. Filter error:
    Error in `filter()`:
    ! Problem while computing `..1 = bad == code`.
    Caused by error in `mask$eval_all_filter()`:
    ! object 'bad' not found
    

# Stats as columns properly transposes the built data

    # A tibble: 3 x 7
      row_label1 var1_n var1_sd var2_n var2_sd ord_layer_index ord_layer_1
      <chr>      <chr>  <chr>   <chr>  <chr>             <int>       <int>
    1 3          "15"   " 0.8"  "15"   " 0.3"                1           1
    2 4          "12"   " 0.6"  "12"   " 0.3"                1           2
    3 5          " 5"   " 0.8"  " 5"   " 0.4"                1           3

---

    # A tibble: 3 x 11
      row_label1 var1_n_0 var1_sd_0 var1_n_1 var1_~1 var2_~2 var2_~3 var2_~4 var2_~5
      <chr>      <chr>    <chr>     <chr>    <chr>   <chr>   <chr>   <chr>   <chr>  
    1 3          "15"     " 0.8"    ""       "BLAH"  "15"    " 0.3"  ""      "BLAH" 
    2 4          " 4"     " 0.2"    " 8"     " 0.5"  " 4"    " 0.1"  " 8"    " 0.3" 
    3 5          ""       "BLAH"    " 5"     " 0.8"  ""      "BLAH"  " 5"    " 0.4" 
    # ... with 2 more variables: ord_layer_index <int>, ord_layer_1 <int>, and
    #   abbreviated variable names 1: var1_sd_1, 2: var2_n_0, 3: var2_sd_0,
    #   4: var2_n_1, 5: var2_sd_1

