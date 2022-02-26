# Missing by variables are handled as specified in precision data

    The precision data provided is missing by variable cases:
               vs
    Datsun 710  1

---

    The precision data provided is missing by variable cases:
               vs
    Datsun 710  1

---

    'arg' should be one of "error", "auto"

---

    # A tibble: 12 x 8
       row_label1 row_label2 var1_3    var1_4   var1_5   ord_layer_index ord_layer_1
       <chr>      <chr>      <chr>     <chr>    <chr>              <int>       <dbl>
     1 0          n          " 12"     "  2"    "  4"                  1           1
     2 0          Mean (SD)  "4.10 (0~ "2.75 (~ "2.91 (~               1           1
     3 0          Median     "3.81"    "2.75"   "2.97"                 1           1
     4 0          Q1, Q3     "3.56, 4~ "2.68, ~ "2.61, ~               1           1
     5 0          Min, Max   "3.4, 5.~ "2.6, 2~ "2.1, 3~               1           1
     6 0          Missing    "  0"     "  0"    "  0"                  1           1
     7 1          n          "  3"     " 10"    "  1"                  1           2
     8 1          Mean (SD)  "3.0467 ~ "2.5905~ "1.5130~               1           2
     9 1          Median     "3.2150"  "2.5500" "1.5130"               1           2
    10 1          Q1, Q3     "2.8400,~ "2.0012~ "1.5130~               1           2
    11 1          Min, Max   "2.465, ~ "1.615,~ "1.513,~               1           2
    12 1          Missing    "  0"     "  0"    "  0"                  1           2
    # ... with 1 more variable: ord_layer_2 <int>

# Data validation for external precision data works effectively

    Precision dataset must include the variables max_int and max_dec

---

    Precision dataset must include the variables max_int and max_dec

---

    max_int and max_dec in precision dataset must be valid integer values

---

    max_int and max_dec in precision dataset must be valid integer values

---

    By variable types mismatch between precision dataset and target data

