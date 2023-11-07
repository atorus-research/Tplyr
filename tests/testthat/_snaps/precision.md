# Missing by variables are handled as specified in precision data

    i In index: 1.
    Caused by error:
    ! The precision data provided is missing by variable cases:
               vs
    Datsun 710  1

---

    i In index: 1.
    Caused by error:
    ! The precision data provided is missing by variable cases:
               vs
    Datsun 710  1

---

    'arg' should be one of "error", "auto"

---

    Code
      t <- tplyr_table(mtcars, gear)
      l <- group_desc(t, wt, by = vs) %>% set_precision_data(prec2, default = "auto")
      t <- add_layers(t, l)
      as.data.frame(build(t))
    Message
      Unhandled precision cases were found - calculating precision based on source data
    Output
         row_label1 row_label2           var1_3           var1_4           var1_5
      1           0          n               12                2                4
      2           0  Mean (SD)     4.10 (0.768)     2.75 (0.180)     2.91 (0.610)
      3           0     Median             3.81             2.75             2.97
      4           0     Q1, Q3       3.56, 4.36       2.68, 2.81       2.61, 3.27
      5           0   Min, Max         3.4, 5.4         2.6, 2.9         2.1, 3.6
      6           0    Missing                0                0                0
      7           1          n                3               10                1
      8           1  Mean (SD) 3.0467 (0.51842) 2.5905 (0.69357) 1.5130 (       )
      9           1     Median           3.2150           2.5500           1.5130
      10          1     Q1, Q3   2.8400, 3.3375   2.0012, 3.1800   1.5130, 1.5130
      11          1   Min, Max     2.465, 3.460     1.615, 3.440     1.513, 1.513
      12          1    Missing                0                0                0
         ord_layer_index ord_layer_1 ord_layer_2
      1                1           1           1
      2                1           1           2
      3                1           1           3
      4                1           1           4
      5                1           1           5
      6                1           1           6
      7                1           2           1
      8                1           2           2
      9                1           2           3
      10               1           2           4
      11               1           2           5
      12               1           2           6

# Data validation for external precision data works effectively

    Precision dataset must include the variables max_int and max_dec

---

    Precision dataset must include the variables max_int and max_dec

---

    max_int and max_dec in precision dataset must be valid integer values

---

    max_int and max_dec in precision dataset must be valid integer values

---

    i In index: 1.
    Caused by error:
    ! By variable types mismatch between precision dataset and target data

# Partially provided decimal precision caps populate correctly

    Code
      as.data.frame(d %>% select(starts_with("var1")))
    Output
            var1_Placebo var1_Xanomeline High Dose var1_Xanomeline Low Dose
      1    322.2 ( 65.0)             298.8 ( 55.5)            287.1 ( 76.8)
      2 322.223 (64.969)          298.849 (55.543)         287.149 (76.822)
      3     322.2 (65.0)              298.8 (55.5)             287.1 (76.8)

