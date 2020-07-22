# Count Layers

# This is used for nesting counts
mtcars$grp <- rep(c("A", "B", "C", "D"), each = 8)
t1 <- tplyr_table(mtcars, gear)
t2 <- tplyr_table(mtcars, gear)
t3 <- tplyr_table(mtcars, gear)
t4 <- tplyr_table(mtcars, gear)
t5 <- tplyr_table(mtcars, gear)
t6 <- tplyr_table(mtcars, gear)
t7 <- tplyr_table(mtcars, gear)
t8 <- tplyr_table(mtcars, gear)
t9 <- tplyr_table(mtcars, gear)
t10 <- tplyr_table(mtcars, gear)

c1 <- group_count(t1, cyl)
c2 <- group_count(t2, cyl, by = am)
c3 <- group_count(t3, cyl, by = vars(am, vs))
c4 <- group_count(t4, cyl, by = vars(am, vs)) %>%
  set_format_strings(f_str("xxx", n))
c5 <- group_count(t5, cyl, by = vars(am, vs)) %>%
  add_total_row()
c6 <- group_count(t6, "cyl") %>%
  set_distinct_by(cyl)
c7 <- group_count(t7, vars(grp, cyl))
c8 <- group_count(t8, cyl) %>%
  set_format_strings(f_str("xx (xx.x%) [xx]", n, pct, distinct)) %>%
  set_distinct_by(am)
c9 <- group_count(t9, vars(grp,cyl)) %>%
  set_indentation("")
c10 <- group_count(t10, cyl) %>%
  set_count_row_prefix("abc")

t1 <- add_layers(t1, c1)
t2 <- add_layers(t2, c2)
t3 <- add_layers(t3, c3)
t4 <- add_layers(t4, c4)
t5 <- add_layers(t5, c5)
t6 <- add_layers(t6, c6)
t7 <- add_layers(t7, c7)
t8 <- add_layers(t8, c8)
t9 <- add_layers(t9, c9)
t10 <- add_layers(t10, c10)


test_that("Count layers are built as expected", {
  # Before the build there should only be 7 things in the layer
  # sort_vars, by, sort, where, target_var, and (sub)layers
  expect_length(c1, 7)
  expect_length(c2, 7)
  expect_length(c3, 7)
  # c4 has format_strings
  expect_length(c4, 8)
  # c5 has include_total_row
  expect_length(c5, 8)
  # c6 has distinct_by
  expect_length(c6, 7)
  expect_length(c7, 6)
  expect_length(c8, 8)
  expect_length(c9, 7)
  expect_length(c10, 7)

  expect_equal(c1$by, quos())
  expect_equal(c2$by, quos(am))
  expect_equal(c3$by, quos(am, vs))
  expect_equal(c4$by, quos(am, vs))
  expect_equal(c5$by, quos(am, vs))
  expect_equal(c6$by, quos())
  expect_equal(c7$by, quos())
  expect_equal(c8$by, quos())
  expect_equal(c9$by, quos())
  expect_equal(c10$by, quos())

  expect_equal(c1$target_var, quos(cyl))
  expect_equal(c2$target_var, quos(cyl))
  expect_equal(c3$target_var, quos(cyl))
  expect_equal(c4$target_var, quos(cyl))
  expect_equal(c5$target_var, quos(cyl))
  expect_equal(c6$target_var, quos("cyl"))
  expect_equal(c7$target_var, quos(grp, cyl))
  expect_equal(c8$target_var, quos(cyl))
  expect_equal(c9$target_var, quos(grp, cyl))
  expect_equal(c10$target_var, quos(cyl))

  expect_equal(c4$format_strings$n_counts, f_str("xxx", n))
  expect_equal(c5$include_total_row, TRUE)
  expect_equal(c6$distinct_by, quo(cyl))
  expect_equal(c8$distinct_by, quo(am))
  expect_equal(c9$indentation, "")
  expect_equal(c10$count_row_prefix, "abc")
})


test_that("Count layers are summarized without errors and warnings", {
  expect_silent(build(t1))
  expect_silent(build(t2))
  expect_silent(build(t3))
  expect_silent(build(t4))
  expect_silent(build(t5))
  expect_silent(build(t6))
  expect_silent(build(t7))
  expect_silent(build(t8))
  expect_silent(build(t9))
  expect_silent(build(t10))
})

test_that("Count layers are processed as expected", {

  # After the build there should be 17 things in the layer
  # [1] "numeric_data"      "sort_vars"         "include_total_row" "id_col_expr"
  # [5] "max_length"        "format_strings"    "summary_stat"      "by"
  # [9] "by_expr"           "sort"              "i"                 "where"
  # [13] "target_var"        "formatted_data"    "count_row_prefix"  "n_width"
  # [17] "replaced_string"   "layers"
  expect_length(c1, 17)
  expect_length(c2, 17)
  expect_length(c3, 17)
  # c4 doesn't have i
  expect_length(c4, 16)
  expect_length(c5, 18)
  # c6 will also have distinct_by so 15
  expect_length(c6, 19)
  expect_length(c7, 14)
  expect_length(c8, 18)
  expect_length(c9, 15)
  expect_length(c10, 17)

  expect_equal(dim(c1$numeric_data), c(9, 4))
  expect_equal(dim(c2$numeric_data), c(18, 5))
  expect_equal(dim(c3$numeric_data), c(36, 6))
  expect_equal(dim(c4$numeric_data), c(36, 6))
  expect_equal(dim(c5$numeric_data), c(39, 6))
  expect_equal(dim(c6$numeric_data), c(3, 7))
  expect_equal(dim(c7$numeric_data), c(45, 5))
  expect_equal(dim(c8$numeric_data), c(9, 6))
  expect_equal(dim(c9$numeric_data), c(45, 5))
  expect_equal(dim(c10$numeric_data), c(9, 4))

  expect_type(c1$numeric_data$n, "double")
  expect_type(c2$numeric_data$n, "double")
  expect_type(c3$numeric_data$n, "double")
  expect_type(c4$numeric_data$n, "double")
  expect_type(c5$numeric_data$n, "double")
  expect_type(c6$numeric_data$n, "double")
  expect_type(c7$numeric_data$n, "double")
  expect_type(c8$numeric_data$n, "double")
  expect_type(c9$numeric_data$n, "double")
  expect_type(c10$numeric_data$n, "double")

  expect_equal(dim(c1$formatted_data), c(3, 4))
  expect_equal(dim(c2$formatted_data), c(6, 5))
  expect_equal(dim(c3$formatted_data), c(12, 6))
  expect_equal(dim(c4$formatted_data), c(12, 6))
  expect_equal(dim(c5$formatted_data), c(13, 6))
  expect_equal(dim(c6$formatted_data), c(1, 4))
  expect_equal(dim(c7$formatted_data), c(15, 4))
  expect_equal(dim(c8$formatted_data), c(3, 4))
  expect_equal(dim(c9$formatted_data), c(15, 4))
  expect_equal(dim(c10$formatted_data), c(3, 4))

  expect_true(all(nchar(unlist(c1$formatted_data[, 2:4])) == 11))
  expect_true(all(nchar(unlist(c2$formatted_data[, 3:5])) == 11))
  expect_true(all(nchar(unlist(c3$formatted_data[, 4:6])) == 11))
  expect_true(all(nchar(unlist(c4$formatted_data[, 5:6])) == 3))
  expect_true(all(nchar(unlist(c5$formatted_data[, 4:6])) == 11))
  expect_true(all(nchar(unlist(c6$formatted_data[, 3:4])) == 11))
  expect_true(all(nchar(unlist(c7$formatted_data[, 2:4])) == 10))

  # Check all start with abc
  expect_true(all(map_chr(unlist(c10$formatted_data[, 1]), str_sub, 1, 3) == "abc"))


})

test_that("An informative warning/error message is raised appropriate", {
  #Issue 443 - Specifing a f_str with less than the n padding raises warning.
  expect_warning({t <- tplyr_table(mtcars, gear) %>%
    add_total_group() %>%
    add_layer(
      group_count(cyl)%>%
        set_format_strings(f_str("x (xxx%)",n,pct))
    ) %>%
    build()}, "Some Informative Warning Message")
})

