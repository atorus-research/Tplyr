# `add_risk_diff` can't be applied to a non-count layer

    Risk difference can only be applied to a count layer.

# Improper parameter entry is handled correctly

    Comparisons provided must be two-element character vectors

---

    Comparisons provided must be two-element character vectors

---

    All arguments provided via `args` must be valid arguments of `prop.test`

# Invalid name to format string call errors properly

    Invalid format names supplied. Count layers only accept the following format names: n_counts, riskdiff

# Error generates when duplicating riskdiff comparison values

    Comparison {4, 4} has duplicated values. Comparisons must not be duplicates

# Missing counts don't cause error in comparisons

    Code
      head(as.data.frame(build(t)))
    Condition
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
      Warning in `prop.test()`:
      Chi-squared approximation may be incorrect
    Output
                                    row_label1                             row_label2
      1 SKIN AND SUBCUTANEOUS TISSUE DISORDERS SKIN AND SUBCUTANEOUS TISSUE DISORDERS
      2 SKIN AND SUBCUTANEOUS TISSUE DISORDERS                               ALOPECIA
      3 SKIN AND SUBCUTANEOUS TISSUE DISORDERS                                BLISTER
      4 SKIN AND SUBCUTANEOUS TISSUE DISORDERS                             COLD SWEAT
      5 SKIN AND SUBCUTANEOUS TISSUE DISORDERS                      DERMATITIS ATOPIC
      6 SKIN AND SUBCUTANEOUS TISSUE DISORDERS                     DERMATITIS CONTACT
        var1_Placebo_F var1_Placebo_M var1_Xanomeline High Dose_F
      1    13 ( 24.5%)     8 ( 24.2%)                  0 (  0.0%)
      2     1 (  1.9%)     0 (  0.0%)                  0 (  0.0%)
      3     0 (  0.0%)     0 (  0.0%)                  0 (  0.0%)
      4     0 (  0.0%)     1 (  3.0%)                  0 (  0.0%)
      5     0 (  0.0%)     1 (  3.0%)                  0 (  0.0%)
      6     0 (  0.0%)     0 (  0.0%)                  0 (  0.0%)
        var1_Xanomeline High Dose_M var1_Xanomeline Low Dose_F
      1                  0 (  0.0%)                24 ( 48.0%)
      2                  0 (  0.0%)                 0 (  0.0%)
      3                  0 (  0.0%)                 2 (  4.0%)
      4                  0 (  0.0%)                 0 (  0.0%)
      5                  0 (  0.0%)                 0 (  0.0%)
      6                  0 (  0.0%)                 0 (  0.0%)
        var1_Xanomeline Low Dose_M ord_layer_index
      1                18 ( 52.9%)               1
      2                 0 (  0.0%)               1
      3                 3 (  8.8%)               1
      4                 0 (  0.0%)               1
      5                 0 (  0.0%)               1
      6                 1 (  2.9%)               1
        rdiff_Xanomeline High Dose_Placebo_F rdiff_Xanomeline High Dose_Placebo_M
      1              -0.245 (-0.383, -0.108)              -0.242 (-0.415, -0.070)
      2              -0.019 (-0.074,  0.037)               0.000 ( 0.000,  0.000)
      3               0.000 ( 0.000,  0.000)               0.000 ( 0.000,  0.000)
      4               0.000 ( 0.000,  0.000)              -0.030 (-0.115,  0.055)
      5               0.000 ( 0.000,  0.000)              -0.030 (-0.115,  0.055)
      6               0.000 ( 0.000,  0.000)               0.000 ( 0.000,  0.000)
        ord_layer_1 ord_layer_2
      1           1         Inf
      2           1           1
      3           1           2
      4           1           3
      5           1           4
      6           1           5

