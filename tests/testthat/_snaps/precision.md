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
       row_label1 row_label2 var1_3            var1_4 var1_5 ord_l~1 ord_l~2 ord_l~3
       <chr>      <chr>      <chr>             <chr>  <chr>    <int>   <dbl>   <int>
     1 0          n          " 12"             "  2"  "  4"        1       1       1
     2 0          Mean (SD)  "4.10 (0.768)"    "2.75~ "2.91~       1       1       2
     3 0          Median     "3.81"            "2.75" "2.97"       1       1       3
     4 0          Q1, Q3     "3.56, 4.36"      "2.68~ "2.61~       1       1       4
     5 0          Min, Max   "3.4, 5.4"        "2.6,~ "2.1,~       1       1       5
     6 0          Missing    "  0"             "  0"  "  0"        1       1       6
     7 1          n          "  3"             " 10"  "  1"        1       2       1
     8 1          Mean (SD)  "3.0467 (0.51842~ "2.59~ "1.51~       1       2       2
     9 1          Median     "3.2150"          "2.55~ "1.51~       1       2       3
    10 1          Q1, Q3     "2.8400, 3.3375"  "2.00~ "1.51~       1       2       4
    11 1          Min, Max   "2.465, 3.460"    "1.61~ "1.51~       1       2       5
    12 1          Missing    "  0"             "  0"  "  0"        1       2       6
    # ... with abbreviated variable names 1: ord_layer_index, 2: ord_layer_1,
    #   3: ord_layer_2

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

# Partially provided decimal precision caps populate correctly

    # A tibble: 3 x 3
      var1_Placebo     `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose`
      <chr>            <chr>                       <chr>                     
    1 322.2 ( 65.0)    298.8 ( 55.5)               287.1 ( 76.8)             
    2 322.223 (64.969) 298.849 (55.543)            287.149 (76.822)          
    3 322.2 (65.0)     298.8 (55.5)                287.1 (76.8)              

