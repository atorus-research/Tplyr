# Count Layers

# This is used for nesting counts
mtcars$grp <- paste0("grp.", mtcars$cyl + sample(c(0, 0.5), 32, replace = TRUE))
mtcars$amn <- unclass(as.factor(mtcars$am))
mtcars <- mutate_all(mtcars, as.character)
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
t13 <- tplyr_table(mtcars, gear)
t14 <- tplyr_table(mtcars, gear)
t15 <- tplyr_table(mtcars, gear)
t16 <- tplyr_table(mtcars, gear) %>%
  add_total_group()

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
c6 <- group_count(t6, cyl) %>%
  set_distinct_by(cyl)
# Multiple target_vars
c7 <- group_count(t7, vars(cyl, grp))
# Distinct count and Event count
c8 <- group_count(t8, cyl) %>%
  set_format_strings(f_str("xx (xx.x%) [xx]", n, pct, distinct)) %>%
  set_distinct_by(am)
# Change indentation
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
  set_result_order_var(distinct_n) %>%
  set_distinct_by(am)
c13 <- group_count(t13, vars(cyl, grp), by = "Test")
c14 <- group_count(t14, vars(cyl, grp)) %>%
  set_outer_sort_position("asc")
c15 <- group_count(t15, cyl) %>%
  set_distinct_by(vars(am, vs))
c16 <- group_count(t16, cyl) %>%
  set_distinct_by(vars(am,vs))

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
t13 <- add_layers(t13, c13)
t14 <- add_layers(t14, c14)
t15 <- add_layers(t15, c15)
t16 <- add_layers(t16, c16)

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
                               "format_strings", "result_order_var", "distinct_by"))

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
  expect_equal(c6$target_var, quos(cyl))
  expect_equal(c7$target_var, quos(cyl, grp))
  expect_equal(c8$target_var, quos(cyl))
  expect_equal(c9$target_var, quos(cyl, grp))
  expect_equal(c10$target_var, quos(cyl))

  expect_equal(c4$format_strings$n_counts, f_str("xxx", n))
  expect_equal(c5$include_total_row, TRUE)
  expect_equal(c6$distinct_by, quos(cyl))
  expect_equal(c8$distinct_by, quos(am))
  expect_equal(c9$indentation, "")
  expect_equal(c10$count_row_prefix, "abc")
  expect_equal(c15$distinct_by, quos(am, vs))
})

test_that("Count layers are summarized without errors and warnings", {
  expect_silent(build(t1))
  expect_silent(build(t2))
  expect_silent(build(t3))
  expect_silent(build(t4))
  expect_silent(build(t5))
  expect_silent(build(t6))
  # Just building this due to some weird error. It produces no output in an
  # expect output, but produces output in an expect silent. Any warnings raised
  # will raise when this is rebuilt for consistancy.
  build(t7)
  expect_silent(build(t8))
  expect_silent(build(t9))
  expect_silent(build(t10))
  expect_silent(build(t11))
  expect_silent(build(t12))
  expect_silent(build(t13))
  expect_silent(build(t14))
  expect_silent(build(t15))
  expect_silent(build(t16))
})

test_that("Count layers are processed as expected", {

  expect_equal(dim(c1$numeric_data), c(9, 4))
  expect_equal(dim(c2$numeric_data), c(18, 5))
  expect_equal(dim(c3$numeric_data), c(36, 6))
  expect_equal(dim(c4$numeric_data), c(36, 6))
  expect_equal(dim(c5$numeric_data), c(39, 6))
  expect_equal(dim(c6$numeric_data), c(9, 6))
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
  expect_equal(dim(c6$formatted_data), c(3, 5))
  expect_equal(dim(c7$formatted_data), c(9, 7))
  expect_equal(dim(c8$formatted_data), c(3, 5))
  expect_equal(dim(c9$formatted_data), c(9, 7))
  expect_equal(dim(c10$formatted_data), c(3, 5))

  expect_true(all(nchar(unlist(c1$formatted_data[, 2:4])) == 11))
  expect_true(all(nchar(unlist(c2$formatted_data[, 3:5])) == 11))
  expect_true(all(nchar(unlist(c3$formatted_data[, 4:6])) == 11))
  expect_true(all(nchar(unlist(c4$formatted_data[, 5:6])) == 3))
  expect_true(all(nchar(unlist(c5$formatted_data[, 4:6])) == 11))
  expect_true(all(nchar(unlist(c6$formatted_data[, 3:4])) == 11))
  expect_true(all(nchar(unlist(c7$formatted_data[, 3:5])) == 11))

  # Check all start with abc
  expect_true(all(map_chr(unlist(c10$formatted_data[, 1]), str_sub, 1, 3) == "abc"))

  # Check denoms can be properly formed when there are custom groups and
  # distinct variables
  expect_true(all(t16$layers[[1]]$numeric_data$distinct_total != 0))

})

test_that("nested count layers can be rebuilt without changes", {
  tmp1 <- build(t7)
  tmp2 <- build(t7)

  expect_equal(tmp1, tmp2)

})

test_that("missing counts can be displayed as expected", {
  mtcars[mtcars$cyl == 6, "cyl"] <- NA
  t1 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xx ", n))
    ) %>%
    build()
  expect_equal(t1[3, 1], tibble(row_label1 = "Missing"))
  expect_equal(t1[3, 2:4], tibble(var1_3 = " 2 ", var1_4 = " 4 ", var1_5 = " 1 "))

  mtcars[is.na(mtcars$cyl), "cyl"] <- "Not here"
  t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xx ", n), string = "Not here")
    ) %>%
    build()
  expect_equal(t2[3, 1], tibble(row_label1 = "Missing"))
  expect_equal(t2[3, 2:4], tibble(var1_3 = " 2 ", var1_4 = " 4 ", var1_5 = " 1 "))

  mtcars[mtcars$cyl == "Not here", "cyl"] <- "Unknown"
  t3 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xx ", n), string = c(UNK = "Unknown"))
    ) %>%
    build()
  expect_equal(t3[3, 1], tibble(row_label1 = "UNK"))
  expect_equal(t3[3, 2:4], tibble(var1_3 = " 2 ", var1_4 = " 4 ", var1_5 = " 1 "))

  mtcars[mtcars$cyl == 8, "cyl"] <- NA
  t4 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xx ", n), string = c(Missing = "NA")) %>%
        set_denom_ignore("Unknown", NA)
    ) %>%
    build()
  expect_equal(t4$row_label1, c("4", "Unknown", "Missing"))
  expect_equal(t4$var1_3, c(" 1 (100.0%)", " 2 (200.0%)", "12 "))
  expect_equal(t4$var1_4, c(" 8 (100.0%)", " 4 ( 50.0%)", " 0 "))
  expect_equal(t4$var1_5, c(" 2 (100.0%)", " 1 ( 50.0%)", " 2 "))
  expect_equal(t4$ord_layer_index, c(1L, 1L, 1L))
  # Added unname for compatibility between tibble versions
  expect_equal(unname(t4$ord_layer_1), c(1, 2, 3))

})

test_that("Count layer clauses with invalid syntax give informative error", {
  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(am, where=bad == code)
    )

  expect_error(build(t), "group_count `where` condition `bad == code` is invalid.")
})


test_that("Nested count layers can be built with text by variables", {
  expect_equal(dim(c13$numeric_data), c(27, 6))
  expect_equal(dim(c13$formatted_data), c(9, 9))

  expect_equal(c13$formatted_data$ord_layer_2, rep(2, 9))
})

test_that("set_outer_sort_position works as expected", {
  expect_equal(c14$formatted_data$ord_layer_2, rep(c(-Inf, 1, 2), 3))
})
