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

t1 <- add_layers(t1, c1)
t2 <- add_layers(t2, c2)
t3 <- add_layers(t3, c3)
t4 <- add_layers(t4, c4)
t5 <- add_layers(t5, c5)
t6 <- add_layers(t6, c6)
t7 <- add_layers(t7, c7)


test_that("Count layers are summarized without errors and warnings", {
  expect_silent(build(t1))
  expect_silent(build(t2))
  expect_silent(build(t3))
  expect_silent(build(t4))
  expect_silent(build(t5))
  expect_silent(build(t6))
  expect_silent(build(t7))
})

test_that("Count layers are built as expected", {
  # Before the build there should only be 7 things in the layer
  # sort_vars, by, sort, where, target_var, and (sub)layers
  expect_length(c1, 6)
  expect_length(c2, 6)
  expect_length(c3, 6)
  # c4 has format_strings
  expect_length(c4, 7)
  # c5 has include_total_row
  expect_length(c5, 7)
  # c6 has distinct_by
  expect_length(c6, 7)
  expect_length(c7, 6)

  expect_equal(c1$by, quos())
  expect_equal(c2$by, quos(am))
  expect_equal(c3$by, quos(am, vs))
  expect_equal(c4$by, quos(am, vs))
  expect_equal(c5$by, quos(am, vs))
  expect_equal(c6$by, quos())
  expect_equal(c7$by, quos())

  expect_equal(c1$target_var, quos(cyl))
  expect_equal(c2$target_var, quos(cyl))
  expect_equal(c3$target_var, quos(cyl))
  expect_equal(c4$target_var, quos(cyl))
  expect_equal(c5$target_var, quos(cyl))
  expect_equal(c6$target_var, quos("cyl"))
  expect_equal(c7$target_var, quos(grp, cyl))

  expect_equal(c4$format_strings, f_str("xxx", n))
  expect_equal(c5$include_total_row, FALSE)
  expect_equal(c6$distinct_by, quo(cyl))
})

test_that("Count layers are processed as expected", {
  build(t1)
  build(t2)
  build(t3)
  build(t4)
  build(t5)
  build(t6)
  #build(t7)

  # After the build there should be 14 things in the layer
  expect_length(c1, 14)
  expect_length(c2, 14)
  expect_length(c3, 14)
  expect_length(c4, 14)
  expect_length(c5, 14)
  # c6 will also have distinct_by so 15
  expect_length(c6, 15)
  #expect_length(c7, 14)

  expect_equal(dim(c1$numeric_data), c(12, 4))
  expect_equal(dim(c2$numeric_data), c(21, 5))
  expect_equal(dim(c3$numeric_data), c(39, 6))
  expect_equal(dim(c4$numeric_data), c(39, 6))
  expect_equal(dim(c5$numeric_data), c(36, 6))
  expect_equal(dim(c6$numeric_data), c(6, 4))
  #expect_equal(dim(c7$numeric_data), c(0, 0))

  expect_equal(dim(c1$formatted_data), c(4, 4))
  expect_equal(dim(c2$formatted_data), c(7, 5))
  expect_equal(dim(c3$formatted_data), c(13, 6))
  expect_equal(dim(c4$formatted_data), c(13, 6))
  expect_equal(dim(c5$formatted_data), c(12, 6))
  expect_equal(dim(c6$formatted_data), c(2, 4))
  #expect_equal(dim(c6$formatted_data), c(4, 4))

  expect_true(all(nchar(unlist(c1$formatted_data[, 2:4])) == 11))
  expect_true(all(nchar(unlist(c4$formatted_data[, 5:6])) == 3))
  expect_true(all(nchar(unlist(c6$formatted_data[, 3:4])) == 10))
})
