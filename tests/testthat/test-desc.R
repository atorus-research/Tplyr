# Desc Layer

t1 <- tplyr_table(mtcars, gear)
t2 <- tplyr_table(mtcars, gear)
t3 <- tplyr_table(mtcars, gear)
t4 <- tplyr_table(mtcars, gear)
t5 <- tplyr_table(mtcars, gear)

d1 <- group_desc(t1, mpg)
d2 <- group_desc(t2, mpg, by = am)
d3 <- group_desc(t3, mpg, by = vars(am, vs))
d4 <- group_desc(t4, mpg) %>%
  set_custom_summaries(mean_squared = mean(.var, na.rm=TRUE)**2) %>%
  set_format_strings(
    "Mean Squared" = f_str("xx.xx", mean_squared)
  )
d5 <- group_desc(t5, vars(mpg, wt))

t1 <- add_layers(t1, d1)
t2 <- add_layers(t2, d2)
t3 <- add_layers(t3, d3)
t4 <- add_layers(t4, d4)
t5 <- add_layers(t5, d5)

test_that("group_desc are built as expected", {
  expect_length(d1, 6)
  expect_length(d2, 6)
  expect_length(d3, 6)
  # The non-default summaries are here
  expect_length(d4, 13)
  expect_length(d5, 6)
})

test_that("Group_desc can be created without warnings and errors", {
  expect_silent(build(t1))
  expect_silent(build(t2))
  expect_silent(build(t3))
  expect_silent(build(t4))
  expect_silent(build(t5))
})

test_that("group_desc are processed as expected", {

  expect_length(d1, 14)
  expect_length(d2, 14)
  expect_length(d3, 14)
  expect_length(d4, 15)
  expect_length(d5, 14)

  expect_equal(dim(d1$numeric_data), c(27, 4))
  expect_equal(dim(d2$numeric_data), c(36, 5))
  expect_equal(dim(d3$numeric_data), c(63, 6))
  expect_equal(dim(d4$numeric_data), c(3, 4))
  expect_equal(dim(d5$numeric_data), c(54, 4))

  expect_type(d1$numeric_data$value, "double")
  expect_type(d2$numeric_data$value, "double")
  expect_type(d3$numeric_data$value, "double")
  expect_type(d4$numeric_data$value, "double")
  expect_type(d5$numeric_data$value, "double")

  expect_equal(dim(d1$formatted_data), c(6, 4))
  expect_equal(dim(d2$formatted_data), c(12, 5))
  expect_equal(dim(d3$formatted_data), c(24, 6))
  expect_equal(dim(d4$formatted_data), c(1, 4))
  expect_equal(dim(d5$formatted_data), c(6, 7))

  expect_true(!any(is.na(unlist(d1$formatted_data[, 2:4]))))
  expect_true(!any(is.na(unlist(d2$formatted_data[, 2:4]))))
  expect_true(!any(is.na(unlist(d3$formatted_data[, 4:6]))))
  expect_true(!any(is.na(unlist(d4$formatted_data[, 2:4]))))
  expect_true(!any(is.na(unlist(d5$formatted_data[, 2:7]))))

})

