# Count layer clauses with invalid syntax give informative error

    group_count `where` condition `bad == code` is invalid. Filter error:
    Error in `h()`:
    ! Problem with `filter()` input `..1`.
    i Input `..1` is `bad == code`.
    x object 'bad' not found
    

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

# nested count layers can accecpt text values in the first variable

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

