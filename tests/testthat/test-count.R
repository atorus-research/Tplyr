# Count Layers
# set seed to get consistant sample
set.seed(1)
# This is used for nesting counts
mtcars2 <- mtcars
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
t17 <- tplyr_table(mtcars, gear)
t18 <- tplyr_table(mtcars, gear)
t19 <- tplyr_table(mtcars, gear)
t20 <- tplyr_table(mtcars, gear)

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
  add_total_row() %>%
  set_denoms_by(gear)
# Add distinct by
c6 <- group_count(t6, cyl) %>%
  set_distinct_by(cyl)
# Multiple target_vars
c7 <- group_count(t7, vars(cyl, grp))
# Distinct count and Event count
c8 <- group_count(t8, cyl) %>%
  set_format_strings(f_str("xx (xx.x%) [xx]", n, pct, distinct_n)) %>%
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
  set_format_strings(f_str("xx (xx.x%) [xx]", n, pct, distinct_n)) %>%
  set_result_order_var(distinct_n) %>%
  set_distinct_by(am)
c13 <- group_count(t13, vars(cyl, grp), by = "Test")
c14 <- group_count(t14, vars(cyl, grp)) %>%
  set_outer_sort_position("asc")
c15 <- group_count(t15, cyl) %>%
  set_distinct_by(vars(am, vs))
c16 <- group_count(t16, cyl) %>%
  set_distinct_by(vars(am,vs))
#Check for warning with by, total row and no denom_by
c17 <- group_count(t17, cyl, by = vars(am, vs)) %>%
  add_total_row()
# Warning shouldn't raise here because they are both strings
c18 <- group_count(t18, cyl, by = vars("am", "vs")) %>%
  add_total_row()
c19 <- group_count(t19, cyl, by = am) %>%
  set_denoms_by(am) %>%
  add_total_row()
c20 <- group_count(t20, cyl) %>%
  set_missing_count(f_str("xx", n), Missing = "4", denom_ignore = TRUE) %>%
  add_total_row()


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
t17 <- add_layers(t17, c17)
t18 <- add_layers(t18, c18)
t19 <- add_layers(t19, c19)
t20 <- add_layers(t20, c20)

test_that("Count layers are built as expected", {

  expect_equal(unname(map_chr(c1$by, as_name)), character())
  expect_equal(unname(map_chr(c2$by, as_name)), "am")
  expect_equal(unname(map_chr(c3$by, as_name)), c("am", "vs"))
  expect_equal(unname(map_chr(c4$by, as_name)), c("am", "vs"))
  expect_equal(unname(map_chr(c5$by, as_name)), c("am", "vs"))
  expect_equal(unname(map_chr(c6$by, as_name)), character())
  expect_equal(unname(map_chr(c7$by, as_name)), character())
  expect_equal(unname(map_chr(c8$by, as_name)), character())
  expect_equal(unname(map_chr(c9$by, as_name)), character())
  expect_equal(unname(map_chr(c10$by, as_name)), character())

  expect_equal(unname(map_chr(c1$target_var, as_name)), "cyl")
  expect_equal(unname(map_chr(c2$target_var, as_name)), "cyl")
  expect_equal(unname(map_chr(c3$target_var, as_name)), "cyl")
  expect_equal(unname(map_chr(c4$target_var, as_name)), "cyl")
  expect_equal(unname(map_chr(c5$target_var, as_name)), "cyl")
  expect_equal(unname(map_chr(c6$target_var, as_name)), "cyl")
  expect_equal(unname(map_chr(c7$target_var, as_name)), c("cyl", "grp"))
  expect_equal(unname(map_chr(c8$target_var, as_name)), "cyl")
  expect_equal(unname(map_chr(c9$target_var, as_name)), c("cyl", "grp"))
  expect_equal(unname(map_chr(c10$target_var, as_name)), "cyl")

  expect_equal(c4$format_strings$n_counts, f_str("xxx", n))
  expect_equal(c5$include_total_row, TRUE)
  expect_equal(unname(map_chr(c6$distinct_by, as_name)), "cyl")
  expect_equal(unname(map_chr(c8$distinct_by, as_name)), "am")
  expect_equal(c9$indentation, "")
  expect_equal(c10$count_row_prefix, "abc")
  expect_equal(unname(map_chr(c15$distinct_by, as_name)), c("am", "vs"))
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
  expect_warning(build(t17), "A total row was added in addition")
  expect_silent(build(t18))
  expect_silent(build(t19))
  expect_warning(build(t20), "Your total row is ignoring certain values.")
})

test_that("Count layers are processed as expected", {

  expect_equal(dim(c1$numeric_data), c(9, 6))
  expect_equal(dim(c2$numeric_data), c(18, 7))
  expect_equal(dim(c3$numeric_data), c(36, 8))
  expect_equal(dim(c4$numeric_data), c(36, 8))
  expect_equal(dim(c5$numeric_data), c(39, 8))
  expect_equal(dim(c6$numeric_data), c(9, 6))
  expect_equal(dim(c7$numeric_data), c(27, 7))
  expect_equal(dim(c8$numeric_data), c(9, 6))
  expect_equal(dim(c9$numeric_data), c(27, 7))
  expect_equal(dim(c10$numeric_data), c(9, 6))

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

  # Check denoms_by calculates pcts correctly
  expect_equal(t19$layers[[1]]$numeric_data$total,
               c(19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L,
                 13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L, 13L))
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
        set_missing_count(f_str("xx ", n), Missing = NA)
    ) %>%
    build()
  expect_equal(t1[3, 1], tibble(row_label1 = "Missing"))
  expect_equal(t1[3, 2:4], tibble(var1_3 = " 2 ", var1_4 = " 4 ", var1_5 = " 1 "))

  mtcars[is.na(mtcars$cyl), "cyl"] <- "Not here"
  t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xx ", n), Missing = "Not here")
    ) %>%
    build()
  expect_equal(t2[3, 1], tibble(row_label1 = "Missing"))
  expect_equal(t2[3, 2:4], tibble(var1_3 = " 2 ", var1_4 = " 4 ", var1_5 = " 1 "))

  mtcars[mtcars$cyl == "Not here", "cyl"] <- "Unknown"
  t3 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xx ", n), UNK = "Unknown")
    ) %>%
    build()
  expect_equal(t3[3, 1], tibble(row_label1 = "UNK"))
  expect_equal(t3[3, 2:4], tibble(var1_3 = " 2 ", var1_4 = " 4 ", var1_5 = " 1 "))

  mtcars[mtcars$cyl == 8, "cyl"] <- NA
  t4 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xx ", n), denom_ignore = TRUE, Missing = NA, Unknown = "Unknown")
    ) %>%
    build() %>%
    arrange(ord_layer_1)
  expect_equal(t4$row_label1, c("4", "Unknown", "Missing"))
  expect_equal(t4$var1_3, c(" 1 (100.0%)", " 2 ", "12 "))
  expect_equal(t4$var1_4, c(" 8 (100.0%)", " 4 ", " 0 "))
  expect_equal(t4$var1_5, c(" 2 (100.0%)", " 1 ", " 2 "))
  expect_equal(t4$ord_layer_index, c(1L, 1L, 1L))
  # Added unname for compatibility between tibble versions
  expect_equal(unname(t4$ord_layer_1), c(1, 2, 3))

  t5 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        add_total_row(f_str("xx (xx.x%)", n, pct), count_missings = FALSE) %>%
        set_missing_count(f_str("xx", n), Missing = c("Unknown", NA))
    ) %>%
    build()

  expect_equal(t5$var1_3, c(" 1 (  6.7%)", "14", " 1 ( 6.7%)"))
})

test_that("Count layer clauses with invalid syntax give informative error", {
  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(am, where = bad == code)
    )

  expect_snapshot_error(build(t))
})


test_that("Nested count layers can be built with text by variables", {
  expect_equal(c13$formatted_data$ord_layer_2, rep(2, 9))
})

test_that("set_outer_sort_position works as expected", {
  expect_equal(c14$formatted_data$ord_layer_2, rep(c(-Inf, 1, 2), 3))
})

test_that("Total rows and missing counts are displayed correctly(0.1.5 Updates)", {
  mtcars2$cyl2 <- mtcars2$cyl + 10
  mtcars2[mtcars2$cyl == "4", "cyl"] <- NA
  mtcars2$grp <- paste0("grp.", mtcars2$cyl + rep(c(0, 0.5), 16))
  mtcars2$amN <- unclass(as.factor(mtcars2$am))
  mtcars2[mtcars2$am == 1, "am"] <- NA

  t1 <- tplyr_table(mtcars2, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xx", n), Missing = NA) %>%
        add_total_row(f_str("xxxxx [xx.x]", n, pct)) %>%
        set_format_strings(f_str("xx (xx.x)", n, pct))
    ) %>%
    build()
  # Missing Count + Total Row
  t2 <- tplyr_table(mtcars2, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xx", n), Missing = NA, `Not Found` = NaN) %>%
        add_total_row(f_str("xxxxx [xx.x]", n, pct)) %>%
        set_format_strings(f_str("xx (xx.x)", n, pct))
    ) %>%
    build()
  # Missing Counts + Total Row
  t3 <- tplyr_table(mtcars2, gear) %>%
    add_layer(
      group_count(am) %>%
        set_missing_count(f_str("xx", n), sort_value = 5689, Missing = NA, `Not Found` = NaN) %>%
        add_total_row(f_str("xxxxx [xx.x]", n, pct), sort_value = 9999, count_missings = TRUE) %>%
        set_order_count_method("byvarn") %>%
        set_format_strings(f_str("xx (xx.x)", n, pct))
    ) %>%
    build()
  # Missing Counts + Total Row + byvarn
  t4 <- tplyr_table(mtcars2, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xx", n), sort_value = 999, Missing = NA, `Not Found` = NaN) %>%
        add_total_row(f_str("xxxxx [xx.x]", n, pct), sort_value = 9999) %>%
        set_order_count_method("bycount") %>%
        set_format_strings(f_str("xx (xx.x)", n, pct))
    ) %>%
    build()
  # Missing Counts + Total Row + bycount
  t5 <- tplyr_table(mtcars2, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xx", n), Missing = NA) %>%
        add_total_row(f_str("xxxxx [xx.x]", n, pct), sort_value = 7862) %>%
        set_format_strings(f_str("xx (xx.x)", n, pct))
    ) %>%
    build()
  # Missing COunts + Total Row(bottom)
  t6 <- tplyr_table(mtcars2, gear) %>%
    add_layer(
      group_count(am) %>%
        set_missing_count(f_str("xx", n), Missing = NA) %>%
        set_order_count_method("byvarn") %>%
        add_total_row(f_str("xxxxx [xx.x]", n, pct), sort_value = -Inf, count_missings = TRUE) %>%
        set_format_strings(f_str("xx (xx.x)", n, pct))
    ) %>%
    build()
  # Missing COunts + Total Row(bottom) + byVarn
  t7 <- tplyr_table(mtcars2, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xx", n), Missing = NA) %>%
        set_order_count_method("bycount") %>%
      add_total_row(f_str("xxxxx [xx.x]", n, pct), sort_value = -6795, count_missings = TRUE) %>%
        set_format_strings(f_str("xx (xx.x)", n, pct))
    ) %>%
    # Suppressing warnring for pct in total
    build()
  # Missing COunts + Total Row(bottom) + by count
  t8 <- tplyr_table(mtcars2, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_distinct_by(am) %>%
        add_total_row() %>%
        set_format_strings(f_str("xx (xx.x)", n, pct))
    ) %>%
    build()
  mtcars3 <- mtcars2
  mtcars3$cyl <- factor(mtcars3$cyl, c(2,3,4,6,8))
  t9 <- tplyr_table(mtcars3, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xxxx", n), Missing_ = NA) %>%
        set_format_strings(f_str("xx (xx.x)", n, pct))
    ) %>%
    build()


  expect_snapshot_output(dput(t1))
  expect_snapshot_output(dput(t2))
  expect_snapshot_output(dput(t3))
  expect_snapshot_output(dput(t4))
  expect_snapshot_output(dput(t5))
  expect_snapshot_output(dput(t6))
  expect_snapshot_output(dput(t7))
  expect_snapshot_output(dput(t8))
  expect_snapshot_output(dput(t9))
})

test_that("set_denom_where works as expected", {
  # Just make a different object for pop_data. Just used to check for warning
  pop_mtcars <- mtcars
  pop_mtcars$grp <- seq_along(mtcars$am)
  t10 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl, where = cyl != 6) %>%
        set_denom_where(TRUE) %>%
        set_format_strings(f_str("xx (xx.x)", n, pct))
    ) %>%
    build()
  expect_snapshot_output(dput(t10))
  t11 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl, where = cyl != 6) %>%
        set_denom_where(cyl != 4) %>%
        set_format_strings(f_str("xx (xx.x)", n, pct))
    ) %>%
    build()
  expect_snapshot_output(dput(t11))

  t12 <- tplyr_table(mtcars, gear) %>%
    set_pop_data(pop_mtcars) %>%
    add_layer(
      group_count(cyl) %>%
        set_denom_where(cyl != 6) %>%
        set_distinct_by(am)
    )
  expect_snapshot_warning(build(t12))

  t13 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl, where = cyl != 6) %>%
        set_distinct_by(am) %>%
        set_format_strings(f_str("xx (xx.x)", distinct_n, distinct_pct)) %>%
        set_format_strings(f_str("xx (xx.x)", n, pct))
    ) %>%
    build()

  expect_snapshot_output(dput(t13))
})

test_that("set_denom_where errors for incompatible object type", {
  t1 <- tplyr_table(mtcars, gear)

  # Modify the object type to make it incompatible
  class(t1) <- "environment"

  # Function errors
  t1 <- set_denom_where(t1, mpg != 21) %>%
    expect_error("Object type should be")
})

test_that("missing counts can be set without a format and it inherits the layer format", {
  t1 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(Missing = "4")
    ) %>%
    build()
  expect_equal(t1$row_label1, c("Missing", "6", "8"))
  expect_equal(t1$var1_3, c(" 1 (  6.7%)", " 2 ( 13.3%)", "12 ( 80.0%)"))

  t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(Missing = "4") %>%
        set_format_strings(f_str("xxx  [xx.x%]", n, pct))
    ) %>%
    build()
  expect_equal(t2$row_label1, c("Missing", "6", "8"))
  expect_equal(t2$var1_3, c("  1  [ 6.7%]", "  2  [13.3%]", " 12  [80.0%]"))
})

test_that("distinct is changed to distinct_n with a warning", {

  expect_warning({
    t <- tplyr_table(mtcars, gear) %>%
      add_layer(
        group_count(cyl) %>%
          set_distinct_by(am) %>%
          set_format_strings(f_str("xx", distinct))
      )
  }, "The use of 'distinct' in count f_strs is discouraged. It was replaced with 'distinct_n' for consistancy.")

  expect_equal(t$layers[[1]]$format_strings$n_counts$vars[[1]], "distinct_n")

})

test_that("Nested count layers can accept text values in the first variable", {
  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(vars("All Cyl", cyl))
    )

  expect_silent(build(t))

  expect_equal(t$layers[[1]]$formatted_data$row_label1,
               c("All Cyl", "All Cyl", "All Cyl", "All Cyl"))
  expect_equal(t$layers[[1]]$formatted_data$row_label2,
               c("All Cyl", "   4", "   6", "   8"))
  expect_equal(t$layers[[1]]$formatted_data$var1_3,
               c("15 (100.0%)", " 1 (  6.7%)", " 2 ( 13.3%)", "12 ( 80.0%)"))
  expect_equal(t$layers[[1]]$formatted_data$var1_4,
               c("12 (100.0%)", " 8 ( 66.7%)", " 4 ( 33.3%)", " 0 (  0.0%)"))
  expect_equal(t$layers[[1]]$formatted_data$var1_5,
               c(" 5 (100.0%)", " 2 ( 40.0%)", " 1 ( 20.0%)", " 2 ( 40.0%)"))

  t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(vars(cyl, "Txt"))
    )
  expect_snapshot_error(build(t2))

  mtcars$cyl <- factor(as.character(mtcars$cyl), c("4", "6", "8", "25"))
  t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(vars("all cyl", cyl))
    ) %>%
    build()

  expect_equal(t2$var1_3,
               c("15 (100.0%)", " 1 (  6.7%)", " 2 ( 13.3%)", "12 ( 80.0%)",
                 " 0 (  0.0%)"))
})

test_that("Variable names will be coersed into symbols", {
  t1 <- tplyr_table(mtcars2, gear) %>%
    add_layer(
      group_count("cyl")
    )
  expect_snapshot_warning(build(t1))

  t2 <- tplyr_table(mtcars2, gear) %>%
    add_layer(
      group_count(vars("all cyl", "cyl"))
    )
  expect_snapshot_warning(build(t2))
})

test_that("nested count layers can be built with character value in first position and risk difference", {
  suppressWarnings({
    t1 <- tplyr_table(mtcars, gear) %>%
      add_layer(
        group_count(vars("all_cyl", cyl)) %>%
          add_risk_diff(
            c("4", "5"),
            c("3", "5")
            )
      ) %>%
      build()
  })


  expect_equal(t1$rdiff_4_5, c(" 0.000 ( 0.000,  0.000)",
                               " 0.267 (-0.380,  0.914)",
                               " 0.133 (-0.441,  0.707)",
                               "-0.400 (-0.971,  0.171)"))
})

test_that("keep_levels works as expeceted", {
  t1 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        keep_levels("4", "6") %>%
        set_format_strings(f_str("xxx (xxx%)", n, pct))
    ) %>%
    build()
  t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(vars("all cyl", cyl)) %>%
        keep_levels("8") %>%
        set_format_strings(f_str("xxx (xxx%)", n, pct))
    ) %>%
    build()

  expect_equal(t1$var1_3, c("  1 (  7%)", "  2 ( 13%)"))
  expect_equal(dim(t1), c(2, 6))
  expect_equal(t2$var1_3, c(" 12 ( 80%)", " 12 ( 80%)"))
  expect_equal(dim(t2), c(2, 8))

  expect_snapshot_error({
    t3 <- tplyr_table(mtcars, gear) %>%
      add_layer(
        group_count(cyl) %>%
          keep_levels("10", "20")
      ) %>%
      build()
  })

  mtcars$grp <- paste0("grp.", as.numeric(mtcars$cyl) + rep(c(0, 0.5), 16))
  t4 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(vars(cyl, grp)) %>%
        keep_levels("nothere")
    )
  expect_snapshot_error(build(t4))
})

test_that("nested count layers can be built with restrictive where logic", {
  mtcars <- mtcars2
  mtcars$grp <- paste0("grp.", mtcars$cyl + sample(c(0, 0.5), 32, replace = TRUE))

  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(vars(cyl, grp), where = grp == "grp.8.5") %>%
        set_nest_count(TRUE) %>%
        set_order_count_method('bycount') %>%
        set_ordering_cols("3")
    ) %>%
    build()

  expect_equal(dim(t), c(2, 7))

})

test_that("nested count layers handle `set_denoms_by` as expected", {
  mtcars <- mtcars2
  mtcars$grp <- paste0("grp.", mtcars$cyl + rep(c(0, 0.5), 16))

  expect_snapshot_error({
    t1 <- tplyr_table(mtcars, gear) %>%
      add_layer(
        group_count(vars(cyl,grp)) %>%
          set_denoms_by(grp)
      )
  })

  t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(vars(cyl,grp)) %>%
        set_denoms_by(cyl)
    ) %>%
    build()

  expect_equal(t2$var1_3,
               c(" 1 (  9.1%)", " 1 (  9.1%)", " 0 (  0.0%)", " 2 ( 28.6%)",
                 " 0 (  0.0%)", " 2 ( 28.6%)", "12 ( 85.7%)", " 7 ( 50.0%)",
                 " 5 ( 35.7%)"))

  t3 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(vars(cyl,grp)) %>%
        set_denoms_by(cyl, gear)
    ) %>%
    build()

  expect_equal(t3$var1_3,
               c(" 1 (100.0%)", " 1 (100.0%)", " 0 (  0.0%)", " 2 (100.0%)",
                 " 0 (  0.0%)", " 2 (100.0%)", "12 (100.0%)", " 7 ( 58.3%)",
                 " 5 ( 41.7%)"))

  # Tests added to capture #136
  expect_snapshot(
    # Results have been manually verified
    # Denom for cyl == 4 is 11
    tplyr_table(mtcars, gear, cols=vs) %>%
      add_layer(
        group_count(vars(cyl,grp)) %>%
          set_denoms_by(cyl)
      ) %>%
      build() %>%
      as.data.frame()
  )

  expect_snapshot(
    # Results have been manually verified
    # Denom for gear == 3, vs = 0 is 12
    tplyr_table(mtcars, gear, cols=vs) %>%
      add_layer(
        group_count(vars(cyl,grp))
      ) %>%
      build() %>%
      as.data.frame()
  )

})

test_that("test IBM rounding option", {
  row_num <- seq(1:2000)
  trta = ifelse(row_num <= 1000, "Placebo", "ThisDrg")
  gender = ifelse(between(row_num, 1, 485), "F",
                  ifelse(between(row_num, 1001, 1525), "F", "M"))
  adsl <- tibble(trta, gender)

  tabl <- tplyr_table(adsl, trta) %>%
    add_total_group(group_name = "Total") %>%
    add_layer(
      group_count(gender, by = "Gender")   %>%
        set_format_strings(f_str("xxx (xxx%)", n, pct))
    ) %>%
    build()

  expect_equal(tabl$var1_Placebo, c("485 ( 48%)", "515 ( 52%)"))

  options(tplyr.IBMRounding = TRUE)

  tabl2 <- tplyr_table(adsl, trta) %>%
    add_total_group(group_name = "Total") %>%
    add_layer(
      group_count(gender, by = "Gender")  %>%
        set_format_strings(f_str("xxx (xxx%)", n, pct))
    )

  expect_warning({tabl2 <- build(tabl2)}, "You have enabled IBM Rounding.")

  expect_equal(tabl2$var1_Placebo, c("485 ( 49%)", "515 ( 52%)"))

  options(tplyr.IBMRounding = FALSE)
})

test_that("test specific rounding proplem #124", {
  vec <- c(2.64, -3.20, -2.88, 2.95)
  mvec <- mean(vec)

  options(tplyr.IBMRounding = TRUE)

  rounded <- ut_round(mvec, 3)

  expect_equal(rounded, -0.123)

  options(tplyr.IBMRounding = FALSE)
})

test_that("nested count can accept data if second variable is bigger than the first", {
  test_adcm <- data.frame(
    SUBJID = c("1", "2", "3"),
    ATC2 = c("Antiemetics and antinauseants", "Psycholeptics", "Psycholeptics"),
    CMDECOD = c("Promethazine hydrochloride", "Promethazine hydrochloride", "Promethazine hydrochloride"),
    TRT101A = c("TRT1", "TRT2", "TRT1")
  )

  x <- test_adcm %>%
    tplyr_table(TRT101A) %>%
    add_layer(
      group_count(vars(ATC2, CMDECOD))
    ) %>%
    build() %>%
    as.data.frame()

  expect_snapshot(x)
})

test_that("Posix columns don't cause the build to error out.", {
#

  load(test_path("adae.Rdata"))
  load(test_path("adsl.Rdata"))

  adsl <- adsl %>%
    mutate(fake_dttm = as.POSIXct("2019-01-01 10:10:10"), origin = "1970-01-01") %>%
    rename(TRTA = TRT01A)

  tp_obj <- tplyr_table(adae, TRTA) %>%
    set_pop_data(adsl) %>%
    add_layer(
      group_count('Number of subjects with any event') %>%
        set_distinct_by(USUBJID) %>%
        set_denoms_by(TRTA)
    )

  expect_silent(build(tp_obj))
})

test_that("set_numeric_threshold works as expected", {

  t1 <- mtcars %>%
    tplyr_table(gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_numeric_threshold(10, "n") %>%
        add_total_row() %>%
        set_order_count_method("bycount")
    )

  expect_snapshot(as.data.frame(build(t1)))

  t2 <- mtcars %>%
    tplyr_table(gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_numeric_threshold(5.1, "n") %>%
        add_total_row() %>%
        set_order_count_method("bycount")
    )

  expect_snapshot(as.data.frame(build(t2)))

  t3 <- mtcars %>%
    tplyr_table(gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_numeric_threshold(13, "n") %>%
        add_total_row() %>%
        set_order_count_method("bycount")
    )

  expect_snapshot(as.data.frame(build(t3)))

  t4 <- mtcars %>%
    tplyr_table(gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_numeric_threshold(16, "n") %>%
        add_total_row() %>%
       set_order_count_method("bycount")
    )

  expect_snapshot(as.data.frame(build(t4)))

  t5 <- mtcars %>%
    tplyr_table(gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_numeric_threshold(0.5, "pct") %>%
        add_total_row() %>%
        set_order_count_method("bycount")
    )

  expect_snapshot(as.data.frame(build(t5)))

  t6 <- mtcars %>%
    tplyr_table(gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_numeric_threshold(4, "n", "3") %>%
        add_total_row() %>%
        set_order_count_method("bycount")
    )

  expect_snapshot(as.data.frame(build(t6)))

  load(test_path("adae.Rdata"))

  t7 <- adae %>%
    tplyr_table(TRTA) %>%
    add_layer(
      group_count(vars(AEBODSYS, AEDECOD)) %>%
        set_numeric_threshold(3, "n", "Placebo")
    )

  expect_snapshot(as.data.frame(build(t7)))

  t8 <- adae %>%
    tplyr_table(TRTA) %>%
    add_layer(
      group_count(vars(AEBODSYS, AEDECOD)) %>%
        set_numeric_threshold(3, "n", "Placebo") %>%
        set_order_count_method("bycount")
    )

  expect_snapshot(as.data.frame(build(t8)))
})

test_that("denom and distinct_denom values work as expected", {


  t1 <- tplyr_table(mtcars2, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_missing_count(f_str("xx", n), Missing = NA) %>%
        add_total_row(f_str("xxxxx [xx.x]", n, pct)) %>%
        set_format_strings(f_str("xx/xxx (xx.x)", n, total, pct)) %>%
        set_order_count_method("bycount")
    )

  expect_snapshot(as.data.frame(build(t1)))

  t2 <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl) %>%
        set_distinct_by(am) %>%
        set_format_strings(f_str("xxx xxx xxx xxx", distinct_n, distinct_total, n, total))
    )

  expect_snapshot(as.data.frame(build(t2)))
})

test_that("denoms with distinct population data populates as expected", {
  load(test_path("adae.Rdata"))
  load(test_path("adsl.Rdata"))

  tab <- tplyr_table(adae, TRTA) %>%
    set_pop_data(adsl) %>%
    set_pop_treat_var(TRT01A) %>%
    add_total_group %>%
    add_treat_grps(Dosed = c("Xanomeline High Dose", "Xanomeline Low Dose")) %>%
    add_layer(
      group_count("Any Body System") %>%
        set_distinct_by(USUBJID) %>%
        set_format_strings(f_str("xx (xx.x%)", distinct_n, distinct_pct))
    ) %>%
    build()

  expect_snapshot(as.data.frame(tab))
})

test_that("nested count layers error out when you try to add a total row", {

  # GH issue 92
  tab <- tplyr_table(mtcars, am) %>%
    add_layer(
      group_count(vars(cyl, grp)) %>%
        add_total_row()
    )

    expect_snapshot_error(build(tab))

  # The weird use of mtcars2 makes us have to overwrite this again
  mtcars <- mtcars2
})

test_that("Tables with pop_data can accept a layer level where", {

  load(test_path('adsl.Rdata'))
  load(test_path('adae.Rdata'))
  t <- tplyr_table(adae, TRTA) %>%
    set_pop_data(adsl) %>%
    set_pop_treat_var(TRT01A) %>%
    set_pop_where(TRUE) %>%
    add_layer(
      group_count(AEDECOD, where = AEREL != "NONE") %>%
        set_distinct_by(USUBJID) %>%
        set_format_strings(f_str("xxx, [xxx] (xxx.x%) [xxx.x%]", distinct_n, n, distinct_pct, pct))
    )

  expect_snapshot(as.data.frame(build(t)))

})

test_that("Regression test to make sure cols produce correct denom", {
  load(test_path('adsl.Rdata'))
  load(test_path('adae.Rdata'))
  t <- tplyr_table(adae,TRTAN, cols=SEX) %>%
    set_pop_data(adsl) %>%
    set_pop_treat_var(TRT01AN) %>%
    add_layer(
      group_count("Subjects with at least one event") %>%
        set_distinct_by(USUBJID) %>%
        set_format_strings(f_str("xxx (xx.x) [xx]", distinct_n, distinct_pct, distinct_total))
    ) %>%
    build() %>%
    select(-starts_with('ord')) %>%
    as.data.frame()

  expect_snapshot(t)
})

test_that("Error checking for add_missing_subjects_row()", {
  expect_snapshot_error(
    tplyr_table(tplyr_adae, TRTA) %>%
      add_layer(
        group_count(AEDECOD) %>%
          add_missing_subjects_row("blah")
      )
  )

  expect_snapshot_error(
    tplyr_table(tplyr_adae, TRTA) %>%
      add_layer(
        group_count(AEDECOD) %>%
          add_missing_subjects_row(f_str("xx", distinct_n), sort_value = "x")
      )
  )

  expect_error({
    tplyr_table(tplyr_adae, TRTA) %>%
      add_layer(
        group_shift(vars(AEBODSYS, AEDECOD)) %>%
          add_missing_subjects_row(f_str("xx", distinct_n))
      )
    }, "`add_missing_subjects_row` for shift layers"
  )

  expect_snapshot_error(
    tplyr_table(tplyr_adae, TRTA) %>%
      add_layer(
        group_desc(RACEN) %>%
          add_missing_subjects_row(f_str("xx", distinct_n))
      )
  )

  ## ----

  expect_snapshot_error(
    tplyr_table(tplyr_adae, TRTA) %>%
      add_layer(
        group_count(AEDECOD) %>%
          set_missing_subjects_row_label(3)
      )
  )

  expect_snapshot_error(
    tplyr_table(tplyr_adae, TRTA) %>%
      add_layer(
        group_count(AEDECOD) %>%
          set_missing_subjects_row_label(c("x", "y"))
      )
  )

  expect_error({
    tplyr_table(tplyr_adae, TRTA) %>%
      add_layer(
        group_shift(vars(AEBODSYS, AEDECOD)) %>%
          set_missing_subjects_row_label("x")
      )}, "`set_missing_subjects_row_label` for shift layers"
  )

  expect_snapshot_error(
    tplyr_table(tplyr_adae, TRTA) %>%
      add_layer(
        group_desc(RACEN) %>%
          set_missing_subjects_row_label("x")
      )
  )

})

test_that("Missing subjects row calculates correctly", {
  x <- tplyr_table(tplyr_adlb, TRTA, cols=SEX) %>%
    set_pop_data(tplyr_adsl) %>%
    set_pop_treat_var(TRT01A) %>%
    add_layer(
      group_count(ANRIND, by = vars(PARAM, AVISIT)) %>%
        set_distinct_by(USUBJID) %>%
        add_missing_subjects_row(f_str("xx", distinct_n))
    ) %>%
    build()

  # Check 1
  in_res1 <- x %>%
    filter(row_label3 == "Missing", row_label1 == "Blood Urea Nitrogen (mmol/L)", row_label2 == "Week 12") %>%
    pull(var1_Placebo_F) %>%
    as.numeric()

  pop1 <- tplyr_adsl %>%
    filter(TRT01A == "Placebo", SEX == "F") %>%
    nrow()

  dat1 <- tplyr_adlb %>%
    filter(PARAM == "Blood Urea Nitrogen (mmol/L)", AVISIT == "Week 12", TRTA == "Placebo", SEX == "F") %>%
    distinct(USUBJID) %>%
    nrow()

  expect_equal(pop1-dat1, in_res1)

  # Check 2
  in_res2 <- x %>%
    filter(row_label3 == "Missing", row_label1 == "Gamma Glutamyl Transferase (U/L)", row_label2 == "Week 24") %>%
    pull(`var1_Xanomeline Low Dose_M`) %>%
    as.numeric()

  pop2 <- tplyr_adsl %>%
    filter(TRT01A == "Xanomeline Low Dose", SEX == "M") %>%
    nrow()

  dat2 <- tplyr_adlb %>%
    filter(PARAM == "Gamma Glutamyl Transferase (U/L)", AVISIT == "Week 24", TRTA == "Xanomeline Low Dose", SEX == "M") %>%
    distinct(USUBJID) %>%
    nrow()

  expect_equal(pop2-dat2, in_res2)

})

test_that("Missing counts on nested count layers function correctly", {
  x <- tplyr_table(tplyr_adae, TRTA) %>%
    set_pop_data(tplyr_adsl) %>%
    set_pop_treat_var(TRT01A) %>%
    add_layer(
      group_count(vars(AEBODSYS, AEDECOD)) %>%
        set_distinct_by(USUBJID) %>%
        add_missing_subjects_row(f_str("xx (XX.x%)", distinct_n, distinct_pct), sort_value = Inf)
    ) %>%
    build()

  expect_equal(nrow(x %>% filter(row_label2 == "   Missing")), 1)
  expect_equal(tail(x, 1)$ord_layer_2, Inf)

  # Verify that bycount works for missing values and sort value is assigned correctly
  x <- tplyr_table(tplyr_adae, TRTA) %>%
    set_pop_data(tplyr_adsl) %>%
    set_pop_treat_var(TRT01A) %>%
    add_layer(
      group_count(vars(AEBODSYS, AEDECOD)) %>%
        set_distinct_by(USUBJID) %>%
        set_order_count_method("bycount") %>%
        set_ordering_cols("Xanomeline High Dose") %>%
        set_result_order_var(distinct_n) %>%
        add_missing_subjects_row(f_str("xx (XX.x%)", distinct_n, distinct_pct), sort_value = 99999)
    ) %>%
    build()

  expect_equal(tail(x, 1)$ord_layer_2, 99999)

  # Also test that label reassignment flows
  # The warning here is intentional
  expect_snapshot_warning({
    x <- tplyr_table(tplyr_adsl, TRT01A) %>%
      add_layer(
        group_count(vars(SEX, RACE)) %>%
          set_order_count_method(c("byfactor", "byvarn")) %>%
          add_missing_subjects_row(f_str("xx (XX.x%)", distinct_n, distinct_pct), sort_value = 99999) %>%
          set_missing_subjects_row_label("New label")
      ) %>%
      build()
  })

  expect_equal(filter(x, row_label2 == "   New label")$ord_layer_2, c(99999, 99999))
})
