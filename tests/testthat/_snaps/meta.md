# Metadata creation errors generate properly

    meta must be a tplyr_meta object

---

    meta must be a tplyr_meta object

---

    Filters must be provided as a list of calls

---

    Filters must be provided as a list of calls

---

    Names must be provided as a list of names

---

    Names must be provided as a list of names

# Metadata extraction and extension error properly

    t must be a tplyr_table object

---

    t does not contain a metadata dataframe. Make sure the tplyr_table was built with `build(metadata=TRUE)`

---

    The provided metadata dataset must have a column named row_id

---

    row_id values in the provided metadata dataset are duplicates of row_id values in the Tplyr metadata. All row_id values must be unique. FALSE

# Metadata extraction and extension work properly

    Code
      as.data.frame(get_metadata(t))
    Output
        row_id row_label1                                   var1_3
      1   d1_1          n ~gear, ~wt, gear == c("3"), ~TRUE, ~TRUE
      2   d2_1  Mean (SD) ~gear, ~wt, gear == c("3"), ~TRUE, ~TRUE
      3   d3_1     Median ~gear, ~wt, gear == c("3"), ~TRUE, ~TRUE
      4   d4_1     Q1, Q3 ~gear, ~wt, gear == c("3"), ~TRUE, ~TRUE
      5   d5_1   Min, Max ~gear, ~wt, gear == c("3"), ~TRUE, ~TRUE
      6   d6_1    Missing ~gear, ~wt, gear == c("3"), ~TRUE, ~TRUE
      7   x1_1       <NA>                                     NULL
                                          var1_4
      1 ~gear, ~wt, gear == c("4"), ~TRUE, ~TRUE
      2 ~gear, ~wt, gear == c("4"), ~TRUE, ~TRUE
      3 ~gear, ~wt, gear == c("4"), ~TRUE, ~TRUE
      4 ~gear, ~wt, gear == c("4"), ~TRUE, ~TRUE
      5 ~gear, ~wt, gear == c("4"), ~TRUE, ~TRUE
      6 ~gear, ~wt, gear == c("4"), ~TRUE, ~TRUE
      7                                     NULL
                                          var1_5
      1 ~gear, ~wt, gear == c("5"), ~TRUE, ~TRUE
      2 ~gear, ~wt, gear == c("5"), ~TRUE, ~TRUE
      3 ~gear, ~wt, gear == c("5"), ~TRUE, ~TRUE
      4 ~gear, ~wt, gear == c("5"), ~TRUE, ~TRUE
      5 ~gear, ~wt, gear == c("5"), ~TRUE, ~TRUE
      6 ~gear, ~wt, gear == c("5"), ~TRUE, ~TRUE
      7                                     NULL

# Metadata print method is accurate

    Code
      print(x)
    Output
      tplyr_meta: 3 names, 4 filters
      Names:
          a, b, c 
      Filters:
          a == 1, b == 2, c == 3, x == "a" 

