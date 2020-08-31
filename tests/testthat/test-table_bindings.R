


##### pop_data tests #####
test_that("pop_data binding attaches pop_data properly", {
  df <- data.frame(a = 1:10, b = 11:20)
  tab <- tplyr_table(df, a)

  # Changed to equivalent due to attribute change in constructor.
  expect_equivalent(pop_data(tab), df)

  pop_data(tab) <- iris
  expect_equivalent(pop_data(tab), iris)

  tab <- set_pop_data(tab , mtcars)
  expect_equivalent(pop_data(tab), mtcars)
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

##### pop_treat_var #####
test_that("pop_treat_var binding attaches pop_treat_var properly", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a) %>%
    set_pop_data(data.frame(d = 21:30))

  expect_equal(pop_treat_var(tab), quo(a))

  set_pop_treat_var(tab, d)
  expect_equal(pop_treat_var(tab), quo(d))
})

test_that("pop_treat_var throws errors as expected", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a) %>%
    set_pop_data(data.frame(d = 21:30))

  expect_error(set_pop_treat_var(tab, a), "pop_treat_var passed to tplyr_table is not a column of pop_data")
  expect_error(set_pop_treat_var(tab, A), "pop_treat_var passed to tplyr_table is not a column of pop_data")
  expect_error(set_pop_treat_var(tab), "pop_treat_var passed to tplyr_table is not a column of pop_data")
  expect_silent(set_pop_treat_var(tab, d))
})

##### treat_grps #####
test_that("treat_grps binding attaches properly", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)

  expect_equal(treat_grps(tab), list())

  add_treat_grps(tab, "Total" = c("Placebo", "Low", "High"))
  expect_equal(treat_grps(tab), list(Total = c("Placebo", "Low", "High")))

  add_treat_grps(tab, "Treated" = c("Low", "High"))
  expect_equal(treat_grps(tab), list(Total = c("Placebo", "Low", "High"),
                                     Treated = c("Low", "High")))

})
