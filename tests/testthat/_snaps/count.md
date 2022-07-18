# Count layer clauses with invalid syntax give informative error

    group_count `where` condition `bad == code` is invalid. Filter error:
    Error in `filter()`:
    ! Problem while computing `..1 = bad == code`.
    Caused by error in `mask$eval_all_filter()`:
    ! object 'bad' not found
    

# set_denom_where works as expected

    structure(list(row_label1 = c("4", "8"), var1_3 = c(" 1 ( 6.7)", 
    "12 (80.0)"), var1_4 = c(" 8 (66.7)", " 0 ( 0.0)"), var1_5 = c(" 2 (40.0)", 
    " 2 (40.0)"), ord_layer_index = c(1L, 1L), ord_layer_1 = c(1, 
    3)), row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"
    ))

---

    structure(list(row_label1 = c("4", "8"), var1_3 = c(" 1 ( 7.1)", 
    "12 (85.7)"), var1_4 = c(" 8 (200.0)", " 0 ( 0.0)"), var1_5 = c(" 2 (66.7)", 
    " 2 (66.7)"), ord_layer_index = c(1L, 1L), ord_layer_1 = c(1, 
    3)), row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"
    ))

---

    A `denom_where` has been set with a pop_data. The `denom_where` has been ignored.You should use `set_pop_where` instead of `set_denom_where`.
    

---

    structure(list(row_label1 = c("4", "8"), var1_3 = c(" 1 ( 7.7)", 
    "12 (92.3)"), var1_4 = c(" 8 (100.0)", " 0 ( 0.0)"), var1_5 = c(" 2 (50.0)", 
    " 2 (50.0)"), ord_layer_index = c(1L, 1L), ord_layer_1 = c(1, 
    3)), row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"
    ))

# Nested count layers can accept text values in the first variable

    Inner layers must be data driven variables

# keep_levels works as expeceted

    group_count `where` condition `TRUE` is invalid. Filter error:
    Error: level passed to `kept_levels` not found: 10 20 
    

---

    group_count `where` condition `TRUE` is invalid. Filter error:
    Error: level passed to `kept_levels` not found: nothere 
    

# nested count layers handle `set_denoms_by` as expected

    You can not pass the second variable in `vars` as a denominator.

# nested count layers will error out if second variable is bigger than the first

    The number of values of your second variable must be greater than the number of levels in your first variable

# denom and distinct_denom values work as expected

    Code
      build(t2)
    Output
      # A tibble: 3 x 6
        row_label1 var1_3            var1_4         var1_5 ord_layer_index ord_layer_1
        <chr>      <chr>             <chr>          <chr>            <int>       <dbl>
      1 4          "  1   3   1  15" "  2   4   8 ~ "  1 ~               1           1
      2 6          "  1   3   2  15" "  2   4   4 ~ "  1 ~               1           2
      3 8          "  1   3  12  15" "  0   4   0 ~ "  1 ~               1           3

---

    Code
      build(t2)
    Output
      # A tibble: 3 x 6
        row_label1 var1_3            var1_4         var1_5 ord_layer_index ord_layer_1
        <chr>      <chr>             <chr>          <chr>            <int>       <dbl>
      1 4          "  1   3   1  15" "  2   4   8 ~ "  1 ~               1           1
      2 6          "  1   3   2  15" "  2   4   4 ~ "  1 ~               1           2
      3 8          "  1   3  12  15" "  0   4   0 ~ "  1 ~               1           3

# denoms with distinct population data populates as expected

    Code
      tab
    Output
      # A tibble: 1 x 6
        row_label1      var1_Placebo `var1_Xanomeli~` `var1_Xanomeli~` ord_layer_index
        <chr>           <chr>        <chr>            <chr>                      <int>
      1 Any Body System 69 (80.2%)   79 (94.0%)       77 (91.7%)                     1
      # ... with 1 more variable: ord_layer_1 <lgl>

