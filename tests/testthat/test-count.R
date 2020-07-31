# Count Layers

# This is used for nesting counts
mtcars$grp <- paste0("grp.", mtcars$cyl + sample(c(0, 0.5), 32, replace = TRUE))
mtcars$amn <- unclass(as.factor(mtcars$am))
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
t11 <- tplyr_table(mtcars, gear)
t12 <- tplyr_table(mtcars, gear)

c1 <- group_count(t1, cyl)
# Add in by
c2 <- group_count(t2, cyl, by = am)
# Add in multiple bys
c3 <- group_count(t3, cyl, by = vars(am, vs))
# Multiple bys and different f_str
c4 <- group_count(t4, cyl, by = vars(am, vs)) %>%
  set_format_strings(f_str("xxx", n))
# Multiple bys and total row
c5 <- group_count(t5, cyl, by = vars(am, vs)) %>%
  add_total_row()
# Add distinct by
c6 <- group_count(t6, "cyl") %>%
  set_distinct_by(cyl)
# Multiple target_vars
c7 <- group_count(t7, vars(cyl, grp))
# Distinct count and Event count
c8 <- group_count(t8, cyl) %>%
  set_format_strings(f_str("xx (xx.x%) [xx]", n, pct, distinct)) %>%
  set_distinct_by(am)
# Change indentaion
c9 <- group_count(t9, vars(cyl, grp)) %>%
  set_indentation("")
# Change row prefix
c10 <- group_count(t10, cyl) %>%
  set_count_row_prefix("abc")
# Change ordering cols
c11 <- group_count(t11, cyl) %>%
  set_ordering_cols(5)
# Change numeric extraction value
c12 <- group_count(t12, cyl) %>%
  set_format_strings(f_str("xx (xx.x%) [xx]", n, pct, distinct)) %>%
  set_result_order_var(distinct_n)



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
t11 <- add_layers(t11, c11)
t12 <- add_layers(t12, c12)

test_that("Count layers are built as expected", {
  expect_setequal(names(c1), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers"))
  expect_setequal(names(c2), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers"))
  expect_setequal(names(c3), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers"))
  expect_setequal(names(c4), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers",
                               "format_strings"))
  expect_setequal(names(c5), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers",
                               "include_total_row"))
  expect_setequal(names(c6), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers",
                               "distinct_by"))
  expect_setequal(names(c7), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers"))
  expect_setequal(names(c8), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers",
                               "distinct_by", "format_strings"))
  expect_setequal(names(c9), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers",
                               "indentation"))
  expect_setequal(names(c10), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers",
                               "count_row_prefix"))
  expect_setequal(names(c11), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers",
                               "ordering_cols"))
  expect_setequal(names(c12), c("by", "stats", "precision_on", "where",
                               "target_var", "precision_by", "layers",
                               "format_strings", "result_order_var"))

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
  expect_equal(c7$target_var, quos(cyl, grp))
  expect_equal(c8$target_var, quos(cyl))
  expect_equal(c9$target_var, quos(cyl, grp))
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

  expect_equal(dim(c1$numeric_data), c(9, 4))
  expect_equal(dim(c2$numeric_data), c(18, 5))
  expect_equal(dim(c3$numeric_data), c(36, 6))
  expect_equal(dim(c4$numeric_data), c(36, 6))
  expect_equal(dim(c5$numeric_data), c(39, 6))
  expect_equal(dim(c6$numeric_data), c(3, 7))
  expect_equal(dim(c7$numeric_data), c(27, 5))
  expect_equal(dim(c8$numeric_data), c(9, 6))
  expect_equal(dim(c9$numeric_data), c(27, 5))
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

  expect_equal(dim(c1$formatted_data), c(3, 5))
  expect_equal(dim(c2$formatted_data), c(6, 7))
  expect_equal(dim(c3$formatted_data), c(12, 9))
  expect_equal(dim(c4$formatted_data), c(12, 9))
  expect_equal(dim(c5$formatted_data), c(13, 9))
  expect_equal(dim(c6$formatted_data), c(1, 5))
  expect_equal(dim(c7$formatted_data), c(9, 6))
  expect_equal(dim(c8$formatted_data), c(3, 5))
  expect_equal(dim(c9$formatted_data), c(9, 6))
  expect_equal(dim(c10$formatted_data), c(3, 5))

  expect_true(all(nchar(unlist(c1$formatted_data[, 2:4])) == 11))
  expect_true(all(nchar(unlist(c2$formatted_data[, 3:5])) == 11))
  expect_true(all(nchar(unlist(c3$formatted_data[, 4:6])) == 11))
  expect_true(all(nchar(unlist(c4$formatted_data[, 5:6])) == 3))
  expect_true(all(nchar(unlist(c5$formatted_data[, 4:6])) == 11))
  expect_true(all(nchar(unlist(c6$formatted_data[, 3:4])) == 11))
  expect_true(all(nchar(unlist(c7$formatted_data[, 2:4])) == 11))

  # Check all start with abc
  expect_true(all(map_chr(unlist(c10$formatted_data[, 1]), str_sub, 1, 3) == "abc"))


})

test_that("An informative warning/error message is raised appropriate", {
  #Issue 443 - Specifing a f_str with less than the n padding raises warning.
  expect_warning({t <- tplyr_table(mtcars, gear) %>%
    add_total_group() %>%
    add_layer(
      group_count(cyl) %>%
        set_format_strings(f_str("x (xxx%)",n,pct))
    ) %>%
    build()}, "Some Informative Warning Message")
})

