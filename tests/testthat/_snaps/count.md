# Count layer clauses with invalid syntax give informative error

    group_count `where` condition `bad == code` is invalid. Filter error:
    Error in `filter()`:
    ! Problem while computing `..1 = bad == code`.
    Caused by error in `mask$eval_all_filter()`:
    ! object 'bad' not found
    

# Total rows and missing counts are displayed correctly(0.1.5 Updates)

    structure(list(row_label1 = c("6", "8", "Missing", "Total"), 
        var1_3 = c(" 2 (13.3)", "12 (80.0)", " 1", "   15 [100.0]"
        ), var1_4 = c(" 4 (33.3)", " 0 ( 0.0)", " 8", "   12 [100.0]"
        ), var1_5 = c(" 1 (20.0)", " 2 (40.0)", " 2", "    5 [100.0]"
        ), ord_layer_index = c(1L, 1L, 1L, 1L), ord_layer_1 = c(1, 
        2, 3, 4)), row.names = c(NA, -4L), class = c("tbl_df", "tbl", 
    "data.frame"))

---

    structure(list(row_label1 = c("6", "8", "Missing", "Not Found", 
    "Total"), var1_3 = c(" 2 (13.3)", "12 (80.0)", " 1", " 0", "   15 [100.0]"
    ), var1_4 = c(" 4 (33.3)", " 0 ( 0.0)", " 8", " 0", "   12 [100.0]"
    ), var1_5 = c(" 1 (20.0)", " 2 (40.0)", " 2", " 0", "    5 [100.0]"
    ), ord_layer_index = c(1L, 1L, 1L, 1L, 1L), ord_layer_1 = c(1, 
    2, 3, 4, 5)), row.names = c(NA, -5L), class = c("tbl_df", "tbl", 
    "data.frame"))

---

    structure(list(row_label1 = c("0", "Missing", "Not Found", "Total"
    ), var1_3 = c("15 (100.0)", " 0", " 0", "   15 [100.0]"), var1_4 = c(" 4 (33.3)", 
    " 8", " 0", "   12 [100.0]"), var1_5 = c(" 0 ( 0.0)", " 5", " 0", 
    "    5 [100.0]"), ord_layer_index = c(1L, 1L, 1L, 1L), ord_layer_1 = c(1, 
    5689, 5690, 9999)), row.names = c(NA, -4L), class = c("tbl_df", 
    "tbl", "data.frame"))

---

    structure(list(row_label1 = c("6", "8", "Missing", "Not Found", 
    "Total"), var1_3 = c(" 2 (13.3)", "12 (80.0)", " 1", " 0", "   15 [100.0]"
    ), var1_4 = c(" 4 (33.3)", " 0 ( 0.0)", " 8", " 0", "   12 [100.0]"
    ), var1_5 = c(" 1 (20.0)", " 2 (40.0)", " 2", " 0", "    5 [100.0]"
    ), ord_layer_index = c(1L, 1L, 1L, 1L, 1L), ord_layer_1 = c(4, 
    0, 999, 1000, 9999)), row.names = c(NA, -5L), class = c("tbl_df", 
    "tbl", "data.frame"))

---

    structure(list(row_label1 = c("6", "8", "Missing", "Total"), 
        var1_3 = c(" 2 (13.3)", "12 (80.0)", " 1", "   15 [100.0]"
        ), var1_4 = c(" 4 (33.3)", " 0 ( 0.0)", " 8", "   12 [100.0]"
        ), var1_5 = c(" 1 (20.0)", " 2 (40.0)", " 2", "    5 [100.0]"
        ), ord_layer_index = c(1L, 1L, 1L, 1L), ord_layer_1 = c(1, 
        2, 3, 7862)), row.names = c(NA, -4L), class = c("tbl_df", 
    "tbl", "data.frame"))

---

    structure(list(row_label1 = c("0", "Missing", "Total"), var1_3 = c("15 (100.0)", 
    " 0", "   15 [100.0]"), var1_4 = c(" 4 (33.3)", " 8", "   12 [100.0]"
    ), var1_5 = c(" 0 ( 0.0)", " 5", "    5 [100.0]"), ord_layer_index = c(1L, 
    1L, 1L), ord_layer_1 = c(1, 3, -Inf)), row.names = c(NA, -3L), class = c("tbl_df", 
    "tbl", "data.frame"))

---

    structure(list(row_label1 = c("6", "8", "Missing", "Total"), 
        var1_3 = c(" 2 (13.3)", "12 (80.0)", " 1", "   15 [100.0]"
        ), var1_4 = c(" 4 (33.3)", " 0 ( 0.0)", " 8", "   12 [100.0]"
        ), var1_5 = c(" 1 (20.0)", " 2 (40.0)", " 2", "    5 [100.0]"
        ), ord_layer_index = c(1L, 1L, 1L, 1L), ord_layer_1 = c(4, 
        0, 8, -6795)), row.names = c(NA, -4L), class = c("tbl_df", 
    "tbl", "data.frame"))

---

    structure(list(row_label1 = c("6", "8", "NA", "Total"), var1_3 = c(" 2 (13.3)", 
    "12 (80.0)", " 1 ( 6.7)", "15 (100.0)"), var1_4 = c(" 4 (33.3)", 
    " 0 ( 0.0)", " 8 (66.7)", "12 (100.0)"), var1_5 = c(" 1 (20.0)", 
    " 2 (40.0)", " 2 (40.0)", " 5 (100.0)"), ord_layer_index = c(1L, 
    1L, 1L, 1L), ord_layer_1 = c(1, 2, 3, 3)), row.names = c(NA, 
    -4L), class = c("tbl_df", "tbl", "data.frame"))

---

    structure(list(row_label1 = c("2", "3", "4", "6", "8", "Missing_"
    ), var1_3 = c(" 0 ( 0.0)", " 0 ( 0.0)", " 0 ( 0.0)", " 2 (13.3)", 
    "12 (80.0)", "   1"), var1_4 = c(" 0 ( 0.0)", " 0 ( 0.0)", " 0 ( 0.0)", 
    " 4 (33.3)", " 0 ( 0.0)", "   8"), var1_5 = c(" 0 ( 0.0)", " 0 ( 0.0)", 
    " 0 ( 0.0)", " 1 (20.0)", " 2 (40.0)", "   2"), ord_layer_index = c(1L, 
    1L, 1L, 1L, 1L, 1L), ord_layer_1 = c(1, 2, 3, 4, 5, 6)), row.names = c(NA, 
    -6L), class = c("tbl_df", "tbl", "data.frame"))

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

# Variable names will be coersed into symbols

    The first target variable has been coerced into a symbol. You should pass variable names unquoted.

---

    The second target variable has been coerced into a symbol.You should pass variable names unquoted.

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

# set_numeric_threshold works as expected

    Code
      build(t1)
    Output
      # A tibble: 2 x 6
        row_label1 var1_3      var1_4        var1_5        ord_layer_index ord_layer_1
        <chr>      <chr>       <chr>         <chr>                   <int>       <dbl>
      1 8          12 ( 80.0%) " 0 (  0.0%)" " 2 ( 40.0%)"               1           0
      2 Total      15 (100.0%) "12 (100.0%)" " 5 (100.0%)"               1          12

---

    Code
      build(t2)
    Output
      # A tibble: 3 x 6
        row_label1 var1_3        var1_4        var1_5        ord_layer_index ord_lay~1
        <chr>      <chr>         <chr>         <chr>                   <int>     <dbl>
      1 4          " 1 (  6.7%)" " 8 ( 66.7%)" " 2 ( 40.0%)"               1         8
      2 8          "12 ( 80.0%)" " 0 (  0.0%)" " 2 ( 40.0%)"               1         0
      3 Total      "15 (100.0%)" "12 (100.0%)" " 5 (100.0%)"               1        12
      # ... with abbreviated variable name 1: ord_layer_1

---

    Code
      build(t3)
    Output
      # A tibble: 1 x 6
        row_label1 var1_3      var1_4      var1_5        ord_layer_index ord_layer_1
        <chr>      <chr>       <chr>       <chr>                   <int>       <dbl>
      1 Total      15 (100.0%) 12 (100.0%) " 5 (100.0%)"               1          12

---

    Code
      build(t4)
    Output
      # A tibble: 0 x 2
      # ... with 2 variables: row_label1 <chr>, ord_layer_index <int>

---

    Code
      build(t5)
    Output
      # A tibble: 3 x 6
        row_label1 var1_3        var1_4        var1_5        ord_layer_index ord_lay~1
        <chr>      <chr>         <chr>         <chr>                   <int>     <dbl>
      1 4          " 1 (  6.7%)" " 8 ( 66.7%)" " 2 ( 40.0%)"               1         8
      2 8          "12 ( 80.0%)" " 0 (  0.0%)" " 2 ( 40.0%)"               1         0
      3 Total      "15 (100.0%)" "12 (100.0%)" " 5 (100.0%)"               1        12
      # ... with abbreviated variable name 1: ord_layer_1

---

    Code
      build(t6)
    Output
      # A tibble: 2 x 6
        row_label1 var1_3      var1_4        var1_5        ord_layer_index ord_layer_1
        <chr>      <chr>       <chr>         <chr>                   <int>       <dbl>
      1 8          12 ( 80.0%) " 0 (  0.0%)" " 2 ( 40.0%)"               1           0
      2 Total      15 (100.0%) "12 (100.0%)" " 5 (100.0%)"               1          12

---

    Code
      build(t7)
    Output
      # A tibble: 9 x 8
        row_label1             row_l~1 var1_~2 var1_~3 var1_~4 ord_l~5 ord_l~6 ord_l~7
        <chr>                  <chr>   <chr>   <chr>   <chr>     <int>   <dbl>   <dbl>
      1 GASTROINTESTINAL DISO~ "GASTR~ " 6 ( ~ " 6 ( ~ " 3 ( ~       1       1     Inf
      2 GASTROINTESTINAL DISO~ "   DI~ " 3 ( ~ " 1 ( ~ " 2 ( ~       1       1       1
      3 GENERAL DISORDERS AND~ "GENER~ "11 ( ~ "21 ( ~ "21 ( ~       1       2     Inf
      4 GENERAL DISORDERS AND~ "   AP~ " 4 ( ~ " 7 ( ~ " 5 ( ~       1       2       1
      5 INFECTIONS AND INFEST~ "INFEC~ " 5 ( ~ " 4 ( ~ " 3 ( ~       1       3     Inf
      6 INFECTIONS AND INFEST~ "   UP~ " 4 ( ~ " 1 ( ~ " 1 ( ~       1       3       1
      7 SKIN AND SUBCUTANEOUS~ "SKIN ~ " 7 ( ~ "21 ( ~ "26 ( ~       1       4     Inf
      8 SKIN AND SUBCUTANEOUS~ "   ER~ " 4 ( ~ " 3 ( ~ " 2 ( ~       1       4       1
      9 SKIN AND SUBCUTANEOUS~ "   PR~ " 3 ( ~ " 8 ( ~ " 7 ( ~       1       4       2
      # ... with abbreviated variable names 1: row_label2, 2: var1_Placebo,
      #   3: `var1_Xanomeline High Dose`, 4: `var1_Xanomeline Low Dose`,
      #   5: ord_layer_index, 6: ord_layer_1, 7: ord_layer_2

---

    Code
      build(t8)
    Output
      # A tibble: 9 x 8
        row_label1             row_l~1 var1_~2 var1_~3 var1_~4 ord_l~5 ord_l~6 ord_l~7
        <chr>                  <chr>   <chr>   <chr>   <chr>     <int>   <dbl>   <dbl>
      1 GASTROINTESTINAL DISO~ "GASTR~ " 6 ( ~ " 6 ( ~ " 3 ( ~       1       3     Inf
      2 GASTROINTESTINAL DISO~ "   DI~ " 3 ( ~ " 1 ( ~ " 2 ( ~       1       3       2
      3 GENERAL DISORDERS AND~ "GENER~ "11 ( ~ "21 ( ~ "21 ( ~       1      21     Inf
      4 GENERAL DISORDERS AND~ "   AP~ " 4 ( ~ " 7 ( ~ " 5 ( ~       1      21       5
      5 INFECTIONS AND INFEST~ "INFEC~ " 5 ( ~ " 4 ( ~ " 3 ( ~       1       3     Inf
      6 INFECTIONS AND INFEST~ "   UP~ " 4 ( ~ " 1 ( ~ " 1 ( ~       1       3       1
      7 SKIN AND SUBCUTANEOUS~ "SKIN ~ " 7 ( ~ "21 ( ~ "26 ( ~       1      26     Inf
      8 SKIN AND SUBCUTANEOUS~ "   ER~ " 4 ( ~ " 3 ( ~ " 2 ( ~       1      26       2
      9 SKIN AND SUBCUTANEOUS~ "   PR~ " 3 ( ~ " 8 ( ~ " 7 ( ~       1      26       7
      # ... with abbreviated variable names 1: row_label2, 2: var1_Placebo,
      #   3: `var1_Xanomeline High Dose`, 4: `var1_Xanomeline Low Dose`,
      #   5: ord_layer_index, 6: ord_layer_1, 7: ord_layer_2

# denom and distinct_denom values work as expected

    Code
      build(t1)
    Output
      # A tibble: 5 x 6
        row_label1 var1_3          var1_4          var1_5          ord_layer~1 ord_l~2
        <chr>      <chr>           <chr>           <chr>                 <int>   <dbl>
      1 4          " 1/ 15 ( 6.7)" " 8/ 12 (66.7)" " 2/  5 (40.0)"           1       8
      2 6          " 2/ 15 (13.3)" " 4/ 12 (33.3)" " 1/  5 (20.0)"           1       4
      3 8          "12/ 15 (80.0)" " 0/ 12 ( 0.0)" " 2/  5 (40.0)"           1       0
      4 Missing    " 0"            " 0"            " 0"                      1       0
      5 Total      "   15 [100.0]" "   12 [100.0]" "    5 [100.0]"           1      12
      # ... with abbreviated variable names 1: ord_layer_index, 2: ord_layer_1

---

    Code
      build(t2)
    Output
      # A tibble: 3 x 6
        row_label1 var1_3            var1_4            var1_5          ord_l~1 ord_l~2
        <chr>      <chr>             <chr>             <chr>             <int>   <dbl>
      1 4          "  1   1   1  15" "  2   2   8  12" "  1   1   2  ~       1       1
      2 6          "  1   1   2  15" "  2   2   4  12" "  1   1   1  ~       1       2
      3 8          "  1   1  12  15" "  0   2   0  12" "  1   1   2  ~       1       3
      # ... with abbreviated variable names 1: ord_layer_index, 2: ord_layer_1

# denoms with distinct population data populates as expected

    Code
      tab
    Output
      # A tibble: 1 x 8
        row_label1      var1_Dosed var1_Plac~1 var1_~2 var1_~3 var1_~4 ord_l~5 ord_l~6
        <chr>           <chr>      <chr>       <chr>   <chr>   <chr>     <int> <lgl>  
      1 Any Body System 93 (55.4%) 32 (37.2%)  125 (4~ 43 (51~ 50 (59~       1 NA     
      # ... with abbreviated variable names 1: var1_Placebo, 2: var1_Total,
      #   3: `var1_Xanomeline High Dose`, 4: `var1_Xanomeline Low Dose`,
      #   5: ord_layer_index, 6: ord_layer_1

# nested count layers error out when you try to add a total row

    You can't include total rows in nested counts. Instead, add a seperate layer for total counts.

