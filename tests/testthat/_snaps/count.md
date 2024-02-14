# Count layer clauses with invalid syntax give informative error

    i In index: 1.
    Caused by error in `value[[3L]]()`:
    ! group_count `where` condition `bad == code` is invalid. Filter error:
    Error in `filter()`:
    i In argument: `bad == code`.
    Caused by error:
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

    i In index: 1.
    Caused by error:
    ! Inner layers must be data driven variables

# Variable names will be coersed into symbols

    The first target variable has been coerced into a symbol. You should pass variable names unquoted.

---

    The second target variable has been coerced into a symbol.You should pass variable names unquoted.

# keep_levels works as expeceted

    i In index: 1.
    Caused by error in `value[[3L]]()`:
    ! group_count `where` condition `TRUE` is invalid. Filter error:
    Error: level passed to `kept_levels` not found: 10 20 

---

    i In index: 1.
    Caused by error in `value[[3L]]()`:
    ! group_count `where` condition `TRUE` is invalid. Filter error:
    Error: level passed to `kept_levels` not found: nothere 

# nested count layers handle `set_denoms_by` as expected

    You can not pass the second variable in `vars` as a denominator.

---

    Code
      tplyr_table(mtcars, gear, cols = vs) %>% add_layer(group_count(vars(cyl, grp)) %>%
        set_denoms_by(cyl)) %>% build() %>% as.data.frame()
    Output
        row_label1 row_label2    var1_3_0    var1_3_1    var1_4_0    var1_4_1
      1          4          4  0 (  0.0%)  1 (  9.1%)  0 (  0.0%)  8 ( 72.7%)
      2          4      grp.4  0 (  0.0%)  1 (  9.1%)  0 (  0.0%)  3 ( 27.3%)
      3          4    grp.4.5  0 (  0.0%)  0 (  0.0%)  0 (  0.0%)  5 ( 45.5%)
      4          6          6  0 (  0.0%)  2 ( 28.6%)  2 ( 28.6%)  2 ( 28.6%)
      5          6      grp.6  0 (  0.0%)  0 (  0.0%)  1 ( 14.3%)  1 ( 14.3%)
      6          6    grp.6.5  0 (  0.0%)  2 ( 28.6%)  1 ( 14.3%)  1 ( 14.3%)
      7          8          8 12 ( 85.7%)  0 (  0.0%)  0 (  0.0%)  0 (  0.0%)
      8          8      grp.8  7 ( 50.0%)  0 (  0.0%)  0 (  0.0%)  0 (  0.0%)
      9          8    grp.8.5  5 ( 35.7%)  0 (  0.0%)  0 (  0.0%)  0 (  0.0%)
           var1_5_0    var1_5_1 ord_layer_index ord_layer_1 ord_layer_2
      1  1 (  9.1%)  1 (  9.1%)               1           1         Inf
      2  1 (  9.1%)  0 (  0.0%)               1           1           1
      3  0 (  0.0%)  1 (  9.1%)               1           1           2
      4  1 ( 14.3%)  0 (  0.0%)               1           2         Inf
      5  0 (  0.0%)  0 (  0.0%)               1           2           1
      6  1 ( 14.3%)  0 (  0.0%)               1           2           2
      7  2 ( 14.3%)  0 (  0.0%)               1           3         Inf
      8  2 ( 14.3%)  0 (  0.0%)               1           3           1
      9  0 (  0.0%)  0 (  0.0%)               1           3           2

---

    Code
      tplyr_table(mtcars, gear, cols = vs) %>% add_layer(group_count(vars(cyl, grp))) %>%
        build() %>% as.data.frame()
    Output
        row_label1 row_label2    var1_3_0    var1_3_1    var1_4_0    var1_4_1
      1          4          4  0 (  0.0%)  1 ( 33.3%)  0 (  0.0%)  8 ( 80.0%)
      2          4      grp.4  0 (  0.0%)  1 ( 33.3%)  0 (  0.0%)  3 ( 30.0%)
      3          4    grp.4.5  0 (  0.0%)  0 (  0.0%)  0 (  0.0%)  5 ( 50.0%)
      4          6          6  0 (  0.0%)  2 ( 66.7%)  2 (100.0%)  2 ( 20.0%)
      5          6      grp.6  0 (  0.0%)  0 (  0.0%)  1 ( 50.0%)  1 ( 10.0%)
      6          6    grp.6.5  0 (  0.0%)  2 ( 66.7%)  1 ( 50.0%)  1 ( 10.0%)
      7          8          8 12 (100.0%)  0 (  0.0%)  0 (  0.0%)  0 (  0.0%)
      8          8      grp.8  7 ( 58.3%)  0 (  0.0%)  0 (  0.0%)  0 (  0.0%)
      9          8    grp.8.5  5 ( 41.7%)  0 (  0.0%)  0 (  0.0%)  0 (  0.0%)
           var1_5_0    var1_5_1 ord_layer_index ord_layer_1 ord_layer_2
      1  1 ( 25.0%)  1 (100.0%)               1           1         Inf
      2  1 ( 25.0%)  0 (  0.0%)               1           1           1
      3  0 (  0.0%)  1 (100.0%)               1           1           2
      4  1 ( 25.0%)  0 (  0.0%)               1           2         Inf
      5  0 (  0.0%)  0 (  0.0%)               1           2           1
      6  1 ( 25.0%)  0 (  0.0%)               1           2           2
      7  2 ( 50.0%)  0 (  0.0%)               1           3         Inf
      8  2 ( 50.0%)  0 (  0.0%)               1           3           1
      9  0 (  0.0%)  0 (  0.0%)               1           3           2

# nested count can accept data if second variable is bigger than the first

    Code
      x
    Output
                           row_label1                    row_label2  var1_TRT1
      1 Antiemetics and antinauseants Antiemetics and antinauseants 1 ( 50.0%)
      2 Antiemetics and antinauseants    Promethazine hydrochloride 1 ( 50.0%)
      3                 Psycholeptics                 Psycholeptics 1 ( 50.0%)
      4                 Psycholeptics    Promethazine hydrochloride 1 ( 50.0%)
         var1_TRT2 ord_layer_index ord_layer_1 ord_layer_2
      1 0 (  0.0%)               1           1         Inf
      2 0 (  0.0%)               1           1           1
      3 1 (100.0%)               1           2         Inf
      4 1 (100.0%)               1           2           1

# set_numeric_threshold works as expected

    Code
      as.data.frame(build(t1))
    Output
        row_label1      var1_3      var1_4      var1_5 ord_layer_index ord_layer_1
      1          8 12 ( 80.0%)  0 (  0.0%)  2 ( 40.0%)               1           0
      2      Total 15 (100.0%) 12 (100.0%)  5 (100.0%)               1          12

---

    Code
      as.data.frame(build(t2))
    Output
        row_label1      var1_3      var1_4      var1_5 ord_layer_index ord_layer_1
      1          4  1 (  6.7%)  8 ( 66.7%)  2 ( 40.0%)               1           8
      2          8 12 ( 80.0%)  0 (  0.0%)  2 ( 40.0%)               1           0
      3      Total 15 (100.0%) 12 (100.0%)  5 (100.0%)               1          12

---

    Code
      as.data.frame(build(t3))
    Output
        row_label1      var1_3      var1_4      var1_5 ord_layer_index ord_layer_1
      1      Total 15 (100.0%) 12 (100.0%)  5 (100.0%)               1          12

---

    Code
      as.data.frame(build(t4))
    Output
      [1] row_label1      ord_layer_index
      <0 rows> (or 0-length row.names)

---

    Code
      as.data.frame(build(t5))
    Output
        row_label1      var1_3      var1_4      var1_5 ord_layer_index ord_layer_1
      1          4  1 (  6.7%)  8 ( 66.7%)  2 ( 40.0%)               1           8
      2          8 12 ( 80.0%)  0 (  0.0%)  2 ( 40.0%)               1           0
      3      Total 15 (100.0%) 12 (100.0%)  5 (100.0%)               1          12

---

    Code
      as.data.frame(build(t6))
    Output
        row_label1      var1_3      var1_4      var1_5 ord_layer_index ord_layer_1
      1          8 12 ( 80.0%)  0 (  0.0%)  2 ( 40.0%)               1           0
      2      Total 15 (100.0%) 12 (100.0%)  5 (100.0%)               1          12

---

    Code
      as.data.frame(build(t7))
    Output
                                                  row_label1
      1                           GASTROINTESTINAL DISORDERS
      2                           GASTROINTESTINAL DISORDERS
      3 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
      4 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
      5                          INFECTIONS AND INFESTATIONS
      6                          INFECTIONS AND INFESTATIONS
      7               SKIN AND SUBCUTANEOUS TISSUE DISORDERS
      8               SKIN AND SUBCUTANEOUS TISSUE DISORDERS
      9               SKIN AND SUBCUTANEOUS TISSUE DISORDERS
                                                  row_label2 var1_Placebo
      1                           GASTROINTESTINAL DISORDERS   6 ( 12.8%)
      2                                            DIARRHOEA   3 (  6.4%)
      3 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS  11 ( 23.4%)
      4                            APPLICATION SITE PRURITUS   4 (  8.5%)
      5                          INFECTIONS AND INFESTATIONS   5 ( 10.6%)
      6                    UPPER RESPIRATORY TRACT INFECTION   4 (  8.5%)
      7               SKIN AND SUBCUTANEOUS TISSUE DISORDERS   7 ( 14.9%)
      8                                             ERYTHEMA   4 (  8.5%)
      9                                             PRURITUS   3 (  6.4%)
        var1_Xanomeline High Dose var1_Xanomeline Low Dose ord_layer_index
      1                6 (  7.8%)               3 (  3.9%)               1
      2                1 (  1.3%)               2 (  2.6%)               1
      3               21 ( 27.3%)              21 ( 27.6%)               1
      4                7 (  9.1%)               5 (  6.6%)               1
      5                4 (  5.2%)               3 (  3.9%)               1
      6                1 (  1.3%)               1 (  1.3%)               1
      7               21 ( 27.3%)              26 ( 34.2%)               1
      8                3 (  3.9%)               2 (  2.6%)               1
      9                8 ( 10.4%)               7 (  9.2%)               1
        ord_layer_1 ord_layer_2
      1           1         Inf
      2           1           1
      3           2         Inf
      4           2           1
      5           3         Inf
      6           3           1
      7           4         Inf
      8           4           1
      9           4           2

---

    Code
      as.data.frame(build(t8))
    Output
                                                  row_label1
      1                           GASTROINTESTINAL DISORDERS
      2                           GASTROINTESTINAL DISORDERS
      3 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
      4 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
      5                          INFECTIONS AND INFESTATIONS
      6                          INFECTIONS AND INFESTATIONS
      7               SKIN AND SUBCUTANEOUS TISSUE DISORDERS
      8               SKIN AND SUBCUTANEOUS TISSUE DISORDERS
      9               SKIN AND SUBCUTANEOUS TISSUE DISORDERS
                                                  row_label2 var1_Placebo
      1                           GASTROINTESTINAL DISORDERS   6 ( 12.8%)
      2                                            DIARRHOEA   3 (  6.4%)
      3 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS  11 ( 23.4%)
      4                            APPLICATION SITE PRURITUS   4 (  8.5%)
      5                          INFECTIONS AND INFESTATIONS   5 ( 10.6%)
      6                    UPPER RESPIRATORY TRACT INFECTION   4 (  8.5%)
      7               SKIN AND SUBCUTANEOUS TISSUE DISORDERS   7 ( 14.9%)
      8                                             ERYTHEMA   4 (  8.5%)
      9                                             PRURITUS   3 (  6.4%)
        var1_Xanomeline High Dose var1_Xanomeline Low Dose ord_layer_index
      1                6 (  7.8%)               3 (  3.9%)               1
      2                1 (  1.3%)               2 (  2.6%)               1
      3               21 ( 27.3%)              21 ( 27.6%)               1
      4                7 (  9.1%)               5 (  6.6%)               1
      5                4 (  5.2%)               3 (  3.9%)               1
      6                1 (  1.3%)               1 (  1.3%)               1
      7               21 ( 27.3%)              26 ( 34.2%)               1
      8                3 (  3.9%)               2 (  2.6%)               1
      9                8 ( 10.4%)               7 (  9.2%)               1
        ord_layer_1 ord_layer_2
      1           3         Inf
      2           3           2
      3          21         Inf
      4          21           5
      5           3         Inf
      6           3           1
      7          26         Inf
      8          26           2
      9          26           7

# denom and distinct_denom values work as expected

    Code
      as.data.frame(build(t1))
    Output
        row_label1        var1_3        var1_4        var1_5 ord_layer_index
      1          4  1/ 15 ( 6.7)  8/ 12 (66.7)  2/  5 (40.0)               1
      2          6  2/ 15 (13.3)  4/ 12 (33.3)  1/  5 (20.0)               1
      3          8 12/ 15 (80.0)  0/ 12 ( 0.0)  2/  5 (40.0)               1
      4    Missing             0             0             0               1
      5      Total    15 [100.0]    12 [100.0]     5 [100.0]               1
        ord_layer_1
      1           8
      2           4
      3           0
      4           0
      5          12

---

    Code
      as.data.frame(build(t2))
    Output
        row_label1          var1_3          var1_4          var1_5 ord_layer_index
      1          4   1   1   1  15   2   2   8  12   1   1   2   5               1
      2          6   1   1   2  15   2   2   4  12   1   1   1   5               1
      3          8   1   1  12  15   0   2   0  12   1   1   2   5               1
        ord_layer_1
      1           1
      2           2
      3           3

# denoms with distinct population data populates as expected

    Code
      as.data.frame(tab)
    Output
             row_label1 var1_Dosed var1_Placebo  var1_Total var1_Xanomeline High Dose
      1 Any Body System 93 (55.4%)   32 (37.2%) 125 (49.2%)                43 (51.2%)
        var1_Xanomeline Low Dose ord_layer_index ord_layer_1
      1               50 (59.5%)               1          NA

# nested count layers error out when you try to add a total row

    i In index: 1.
    Caused by error:
    ! You can't include total rows in nested counts. Instead, add a seperate layer for total counts.

# Tables with pop_data can accept a layer level where

    Code
      as.data.frame(build(t))
    Output
                                 row_label1                 var1_Placebo
      1                      ABDOMINAL PAIN   0, [  0] (  0.0%) [  0.0%]
      2                           AGITATION   0, [  0] (  0.0%) [  0.0%]
      3                             ANXIETY   0, [  0] (  0.0%) [  0.0%]
      4         APPLICATION SITE DERMATITIS   1, [  1] (  1.2%) [  2.1%]
      5           APPLICATION SITE ERYTHEMA   0, [  0] (  0.0%) [  0.0%]
      6         APPLICATION SITE IRRITATION   1, [  1] (  1.2%) [  2.1%]
      7               APPLICATION SITE PAIN   0, [  0] (  0.0%) [  0.0%]
      8           APPLICATION SITE PRURITUS   4, [  4] (  4.7%) [  8.5%]
      9           APPLICATION SITE REACTION   1, [  1] (  1.2%) [  2.1%]
      10         APPLICATION SITE URTICARIA   0, [  0] (  0.0%) [  0.0%]
      11          APPLICATION SITE VESICLES   1, [  1] (  1.2%) [  2.1%]
      12            APPLICATION SITE WARMTH   0, [  0] (  0.0%) [  0.0%]
      13                 ATRIAL HYPERTROPHY   1, [  1] (  1.2%) [  2.1%]
      14                            BLISTER   0, [  0] (  0.0%) [  0.0%]
      15          BUNDLE BRANCH BLOCK RIGHT   1, [  1] (  1.2%) [  2.1%]
      16                  BURNING SENSATION   0, [  0] (  0.0%) [  0.0%]
      17         CARDIAC FAILURE CONGESTIVE   1, [  1] (  1.2%) [  2.1%]
      18                             CHILLS   1, [  2] (  1.2%) [  4.3%]
      19           COMPLEX PARTIAL SEIZURES   0, [  0] (  0.0%) [  0.0%]
      20                  CONFUSIONAL STATE   1, [  1] (  1.2%) [  2.1%]
      21                       CONSTIPATION   1, [  1] (  1.2%) [  2.1%]
      22                           CYSTITIS   0, [  0] (  0.0%) [  0.0%]
      23                 DERMATITIS CONTACT   0, [  0] (  0.0%) [  0.0%]
      24                          DIARRHOEA   2, [  2] (  2.3%) [  4.3%]
      25                          DIZZINESS   0, [  0] (  0.0%) [  0.0%]
      26 ELECTROCARDIOGRAM T WAVE INVERSION   1, [  1] (  1.2%) [  2.1%]
      27                          EPISTAXIS   0, [  0] (  0.0%) [  0.0%]
      28                           ERYTHEMA   3, [  4] (  3.5%) [  8.5%]
      29                            FATIGUE   0, [  0] (  0.0%) [  0.0%]
      30              HALLUCINATION, VISUAL   0, [  0] (  0.0%) [  0.0%]
      31               HEART RATE INCREASED   1, [  1] (  1.2%) [  2.1%]
      32               HEART RATE IRREGULAR   1, [  1] (  1.2%) [  2.1%]
      33                      HYPERHIDROSIS   0, [  0] (  0.0%) [  0.0%]
      34                      HYPONATRAEMIA   1, [  1] (  1.2%) [  2.1%]
      35                        HYPOTENSION   0, [  0] (  0.0%) [  0.0%]
      36                 INCREASED APPETITE   1, [  1] (  1.2%) [  2.1%]
      37                       INFLAMMATION   0, [  0] (  0.0%) [  0.0%]
      38                       IRRITABILITY   1, [  1] (  1.2%) [  2.1%]
      39                            MALAISE   0, [  0] (  0.0%) [  0.0%]
      40                            MYALGIA   0, [  0] (  0.0%) [  0.0%]
      41              MYOCARDIAL INFARCTION   0, [  0] (  0.0%) [  0.0%]
      42                             NAUSEA   1, [  1] (  1.2%) [  2.1%]
      43                  OEDEMA PERIPHERAL   1, [  1] (  1.2%) [  2.1%]
      44                           PRURITUS   3, [  3] (  3.5%) [  6.4%]
      45               PRURITUS GENERALISED   0, [  0] (  0.0%) [  0.0%]
      46                               RASH   0, [  0] (  0.0%) [  0.0%]
      47                RASH MACULO-PAPULAR   0, [  0] (  0.0%) [  0.0%]
      48                      RASH PRURITIC   0, [  0] (  0.0%) [  0.0%]
      49                  SINUS BRADYCARDIA   0, [  0] (  0.0%) [  0.0%]
      50                   SKIN EXFOLIATION   0, [  0] (  0.0%) [  0.0%]
      51                    SKIN IRRITATION   0, [  0] (  0.0%) [  0.0%]
      52     SUPRAVENTRICULAR EXTRASYSTOLES   1, [  1] (  1.2%) [  2.1%]
      53                            SYNCOPE   0, [  0] (  0.0%) [  0.0%]
      54                        TACHYCARDIA   1, [  1] (  1.2%) [  2.1%]
      55         TRANSIENT ISCHAEMIC ATTACK   0, [  0] (  0.0%) [  0.0%]
      56  UPPER RESPIRATORY TRACT INFECTION   1, [  1] (  1.2%) [  2.1%]
      57                          URTICARIA   0, [  0] (  0.0%) [  0.0%]
      58                           VOMITING   0, [  0] (  0.0%) [  0.0%]
      59                              WOUND   0, [  0] (  0.0%) [  0.0%]
            var1_Xanomeline High Dose     var1_Xanomeline Low Dose ord_layer_index
      1    0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      2    0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      3    0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      4    3, [  3] (  3.6%) [  3.9%]   2, [  2] (  2.4%) [  2.6%]               1
      5    3, [  3] (  3.6%) [  3.9%]   4, [  4] (  4.8%) [  5.3%]               1
      6    3, [  4] (  3.6%) [  5.2%]   2, [  2] (  2.4%) [  2.6%]               1
      7    1, [  1] (  1.2%) [  1.3%]   0, [  0] (  0.0%) [  0.0%]               1
      8    6, [  7] (  7.1%) [  9.1%]   4, [  4] (  4.8%) [  5.3%]               1
      9    1, [  1] (  1.2%) [  1.3%]   0, [  0] (  0.0%) [  0.0%]               1
      10   0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      11   1, [  1] (  1.2%) [  1.3%]   0, [  0] (  0.0%) [  0.0%]               1
      12   0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      13   0, [  0] (  0.0%) [  0.0%]   0, [  0] (  0.0%) [  0.0%]               1
      14   0, [  0] (  0.0%) [  0.0%]   1, [  2] (  1.2%) [  2.6%]               1
      15   0, [  0] (  0.0%) [  0.0%]   0, [  0] (  0.0%) [  0.0%]               1
      16   1, [  1] (  1.2%) [  1.3%]   0, [  0] (  0.0%) [  0.0%]               1
      17   0, [  0] (  0.0%) [  0.0%]   0, [  0] (  0.0%) [  0.0%]               1
      18   0, [  0] (  0.0%) [  0.0%]   0, [  0] (  0.0%) [  0.0%]               1
      19   0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      20   0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      21   0, [  0] (  0.0%) [  0.0%]   0, [  0] (  0.0%) [  0.0%]               1
      22   1, [  1] (  1.2%) [  1.3%]   0, [  0] (  0.0%) [  0.0%]               1
      23   0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      24   1, [  1] (  1.2%) [  1.3%]   2, [  2] (  2.4%) [  2.6%]               1
      25   1, [  1] (  1.2%) [  1.3%]   3, [  4] (  3.6%) [  5.3%]               1
      26   0, [  0] (  0.0%) [  0.0%]   0, [  0] (  0.0%) [  0.0%]               1
      27   1, [  1] (  1.2%) [  1.3%]   0, [  0] (  0.0%) [  0.0%]               1
      28   3, [  3] (  3.6%) [  3.9%]   1, [  1] (  1.2%) [  1.3%]               1
      29   0, [  0] (  0.0%) [  0.0%]   2, [  2] (  2.4%) [  2.6%]               1
      30   1, [  1] (  1.2%) [  1.3%]   0, [  0] (  0.0%) [  0.0%]               1
      31   0, [  0] (  0.0%) [  0.0%]   0, [  0] (  0.0%) [  0.0%]               1
      32   0, [  0] (  0.0%) [  0.0%]   0, [  0] (  0.0%) [  0.0%]               1
      33   2, [  2] (  2.4%) [  2.6%]   1, [  1] (  1.2%) [  1.3%]               1
      34   0, [  0] (  0.0%) [  0.0%]   0, [  0] (  0.0%) [  0.0%]               1
      35   0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      36   1, [  1] (  1.2%) [  1.3%]   0, [  0] (  0.0%) [  0.0%]               1
      37   0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      38   0, [  0] (  0.0%) [  0.0%]   0, [  0] (  0.0%) [  0.0%]               1
      39   0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      40   1, [  1] (  1.2%) [  1.3%]   0, [  0] (  0.0%) [  0.0%]               1
      41   1, [  1] (  1.2%) [  1.3%]   2, [  2] (  2.4%) [  2.6%]               1
      42   2, [  2] (  2.4%) [  2.6%]   0, [  0] (  0.0%) [  0.0%]               1
      43   0, [  0] (  0.0%) [  0.0%]   0, [  0] (  0.0%) [  0.0%]               1
      44   8, [  8] (  9.5%) [ 10.4%]   6, [  6] (  7.1%) [  7.9%]               1
      45   0, [  0] (  0.0%) [  0.0%]   1, [  2] (  1.2%) [  2.6%]               1
      46   2, [  2] (  2.4%) [  2.6%]   3, [  4] (  3.6%) [  5.3%]               1
      47   1, [  2] (  1.2%) [  2.6%]   0, [  0] (  0.0%) [  0.0%]               1
      48   1, [  1] (  1.2%) [  1.3%]   1, [  1] (  1.2%) [  1.3%]               1
      49   1, [  1] (  1.2%) [  1.3%]   0, [  0] (  0.0%) [  0.0%]               1
      50   0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      51   1, [  1] (  1.2%) [  1.3%]   3, [  3] (  3.6%) [  3.9%]               1
      52   0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      53   1, [  1] (  1.2%) [  1.3%]   2, [  2] (  2.4%) [  2.6%]               1
      54   0, [  0] (  0.0%) [  0.0%]   0, [  0] (  0.0%) [  0.0%]               1
      55   0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
      56   0, [  0] (  0.0%) [  0.0%]   0, [  0] (  0.0%) [  0.0%]               1
      57   1, [  2] (  1.2%) [  2.6%]   1, [  2] (  1.2%) [  2.6%]               1
      58   2, [  2] (  2.4%) [  2.6%]   0, [  0] (  0.0%) [  0.0%]               1
      59   0, [  0] (  0.0%) [  0.0%]   1, [  1] (  1.2%) [  1.3%]               1
         ord_layer_1
      1            1
      2            2
      3            3
      4            4
      5            5
      6            6
      7            7
      8            8
      9            9
      10          10
      11          11
      12          12
      13          15
      14          17
      15          19
      16          20
      17          21
      18          23
      19          24
      20          25
      21          26
      22          30
      23          32
      24          33
      25          34
      26          35
      27          36
      28          37
      29          40
      30          42
      31          44
      32          45
      33          47
      34          49
      35          50
      36          51
      37          52
      38          54
      39          55
      40          56
      41          57
      42          60
      43          63
      44          65
      45          66
      46          67
      47          68
      48          69
      49          72
      50          73
      51          74
      52          76
      53          78
      54          79
      55          80
      56          82
      57          84
      58          87
      59          88

# Regression test to make sure cols produce correct denom

    Code
      t
    Output
                              row_label1        var1_0_F        var1_0_M
      1 Subjects with at least one event  19 (35.8) [53]  13 (39.4) [33]
              var1_54_F       var1_54_M       var1_81_F       var1_81_M
      1  27 (54.0) [50]  23 (67.6) [34]  17 (42.5) [40]  26 (59.1) [44]

# Error checking for add_missing_subjects_row()

    Argument `fmt` does not inherit "f_str". Classes: character

---

    Argument `sort_value` does not inherit "numeric". Classes: character

---

    Argument `e` does not inherit "count_layer". Classes: tplyr_layer, desc_layer, environment

---

    Argument `missing_subjects_row_label` must be character. Instead a class of "numeric" was passed.

---

    length(missing_subjects_row_label) not equal to 1

---

    Argument `e` does not inherit "count_layer". Classes: tplyr_layer, desc_layer, environment

