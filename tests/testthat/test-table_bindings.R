##### header tests #####
test_that("header binding attaches headers properly", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)

  expect_error(header(tab), "object 'headers' not found")

  header(tab) <- c("a", "b")
  expect_equal(header(tab), c("a", "b"))

  tab <- set_header(tab, c("a", "b", "c"))
  expect_equal(header(tab), c("a", "b", "c"))
})

test_that("header binding throws expected errors", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)

  expect_error(header(tab) <- c(1, 2), "'headers' argument passed")
  expect_error(header(tab) <- list("a", "b"), "'headers' argument passed")
  expect_error(header(tab) <- NULL, "'headers' argument passed")
  expect_error(header(tab) <- NA, "'headers' argument passed")
  expect_silent(header(tab) <- NA_character_)
  expect_silent(header(tab) <- c("a", "b"))
})

##### header_n tests #####
test_that("header_n binding attaches header_n properly", {
  tab <- tplyr_table(iris, Species)

  expect_equal(header_n(tab), c(setosa = 50, versicolor = 50, virginica = 50))

  header_n(tab) <- c(group1 = 75, group2 = 75)
  expect_equal(header_n(tab), c(group1 = 75, group2 = 75))

  set_header_n(tab, c(total = 150))
  expect_equal(header_n(tab), c(total = 150))
})

test_that("header_n binding throws errors as expected", {
  tab <- tplyr_table(iris, Species)

  expect_error(header_n(tab) <- c(1,2,3), "header_n argument must be named")
  expect_error(set_header_n(tab, c(a = "1", b = "2")), "header_n argument must be numeric")
  expect_silent(header_n(tab) <- c(a = NA_integer_))
})

##### pop_data tests #####
test_that("pop_data binding attaches pop_data properly", {
  df <- data.frame(a = 1:10, b = 11:20)
  tab <- tplyr_table(df, a)

  expect_reference(pop_data(tab), df)

  pop_data(tab) <- iris
  expect_reference(pop_data(tab), iris)

  tab <- set_pop_data(tab , mtcars)
  expect_reference(pop_data(tab), mtcars)
})

test_that("pop_data binding throws expected errors", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)

  expect_error(pop_data(tab) <- "a", "'pop_data' argument passed")
  expect_error(pop_data(tab) <- iris3, "'pop_data' argument passed")
  expect_error(pop_data(tab) <- NA, "'pop_data' argument passed")
  expect_error(pop_data(tab) <- NULL, "'pop_data' argument passed")
  expect_silent(pop_data(tab) <- iris)
})

##### treat_var tests #####
test_that("treat_var binding attaches treat_var properly", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)

  expect_equal(treat_var(tab), quo(a))

  tab <- set_treat_var(tab, b)
  expect_equal(treat_var(tab), quo(b))
})

test_that("treat_var throws errors as expected", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)

  expect_error(set_treat_var(tab, c),"treat_var column not found in target dataset")
  expect_error(set_treat_var(tab, A), "treat_var column not found in target dataset")
  expect_error(set_treat_var(tab), "A treat_var argument must be supplied")
  expect_silent(set_treat_var(tab, b))
})
