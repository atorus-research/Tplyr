##### header tests #####
test_that("header binding attaches headers properly", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)

  expect_error(tplyr_header(tab), "object 'headers' not found")

  tplyr_header(tab) <- c("a", "b")
  expect_equal(tplyr_header(tab), c("a", "b"))

  tab <- set_tplyr_header(tab, c("a", "b", "c"))
  expect_equal(tplyr_header(tab), c("a", "b", "c"))
})

test_that("header binding throws expected errors", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)

  expect_error(tplyr_header(tab) <- c(1, 2), "'headers' argument passed")
  expect_error(tplyr_header(tab) <- list("a", "b"), "'headers' argument passed")
  expect_error(tplyr_header(tab) <- NULL, "'headers' argument passed")
  expect_error(tplyr_header(tab) <- NA, "'headers' argument passed")
  expect_silent(tplyr_header(tab) <- NA_character_)
  expect_silent(tplyr_header(tab) <- c("a", "b"))
})

##### pop_data tests #####
test_that("pop_data binding attaches pop_data properly", {
  df <- data.frame(a = 1:10, b = 11:20)
  tab <- tplyr_table(df, a)

  expect_reference(tplyr_pop_data(tab), df)

  tplyr_pop_data(tab) <- iris
  expect_reference(tplyr_pop_data(tab), iris)

  tab <- set_tplyr_pop_data(tab , mtcars)
  expect_reference(tplyr_pop_data(tab), mtcars)
})

test_that("pop_data binding throws expected errors", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)

  expect_error(tplyr_pop_data(tab) <- "a", "'pop_data' argument passed")
  expect_error(tplyr_pop_data(tab) <- iris3, "'pop_data' argument passed")
  expect_error(tplyr_pop_data(tab) <- NA, "'pop_data' argument passed")
  expect_error(tplyr_pop_data(tab) <- NULL, "'pop_data' argument passed")
  expect_silent(tplyr_pop_data(tab) <- iris)
})

##### treat_var tests #####
test_that("treat_var binding attaches treat_var properly", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)

  expect_equal(tplyr_treat_var(tab), quo(a))

  tab <- set_tplyr_treat_var(tab, b)
  expect_equal(tplyr_treat_var(tab), quo(b))
})

test_that("treat_var throws errors as expected", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)

  expect_error(tplyr_treat_var(tab) <- quo(c), "Cannot find treat_var")
  expect_error(tplyr_treat_var(tab) <- "A", "Cannot find treat_var")
  expect_error(set_tplyr_treat_var(tab), "A treat_var argument must be")
  expect_silent(set_tplyr_treat_var(tab, b))
})
