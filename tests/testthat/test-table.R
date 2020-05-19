test_that("tplyr_table returns an empty envrionment of class 'tplyr_table' when passed no arguemnts", {
  st <- tplyr_table()
  expect_true(is.environment(st))
})

### Errors ###
test_that("tplyr_table throws error when passed a bad table argument", {
  expect_error(tplyr_table(matrix(), c("a", "b", "c"), c(a = "1", b = "2", c = "3")), class = "tplyr_table_constructor_error")
  expect_silent(tplyr_table(data.frame(), c("a", "b", "c"), c(a = "1", b = "2", c = "3")))
})

test_that("tplyr_table throws error when passed a bad header argument", {
  expect_error(tplyr_table(data.frame(), list("a", "b", "c"), c(a = "1", b = "2", c = "3")), class = "tplyr_table_constructor_error")
  expect_error(tplyr_table(data.frame(), c(1, 2, 3), c(a = "1", b = "2", c = "3")), class = "tplyr_table_constructor_error")
  expect_silent(tplyr_table(data.frame(), c("a", "b", "c"), c(a = "1", b = "2", c = "3")))
})

test_that("tplyr_table throws error when passed a bad header_n argument", {
  expect_error(tplyr_table(data.frame(), c("a", "b", "c"), c("1", "2", "3")), class = "tplyr_table_constructor_error")
  expect_error(tplyr_table(data.frame(), c("a", "b", "c"), c(a = 1, b = 2, c = 3)))
  expect_error(tplyr_table(data.frame(), c("a", "b", "c"), c(b = "1", c = "2", d = "3")), class = "tplyr_table_constructor_error")
  expect_error(tplyr_table(data.frame(), c("a", "b", "c"), c(a = "1", b = "2", c = "3", d = "4")), class = "tplyr_table_constructor_error")
  expect_silent(tplyr_table(data.frame(), c("a", "b", "c"), c(a = "1", b = "2", c = "3")))
})
