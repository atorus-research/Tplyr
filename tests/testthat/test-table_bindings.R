


##### pop_data tests #####
test_that("pop_data binding attaches pop_data properly", {
  df <- data.frame(a = 1:10, b = 11:20)
  tab <- tplyr_table(df, a)

  # Changed to equivalent due to attribute change in constructor.
  expect_equal(pop_data(tab), df, ignore_attr = TRUE)

  pop_data(tab) <- iris
  expect_equal(pop_data(tab), iris, ignore_attr = TRUE)

  tab <- set_pop_data(tab , mtcars)
  expect_equal(pop_data(tab), mtcars, ignore_attr = TRUE)
})

test_that("pop_data binding throws expected errors", {
  tab <- tplyr_table(data.frame(a = 1:10, b = 11:20), a)

  expect_snapshot_error(pop_data(tab) <- "a")
  expect_snapshot_error(pop_data(tab) <- iris3)
  expect_snapshot_error(pop_data(tab) <- NA)
  expect_snapshot_error(pop_data(tab) <- NULL)
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

  expect_snapshot_error(set_treat_var(tab, c))
  expect_snapshot_error(set_treat_var(tab, A))
  expect_snapshot_error(set_treat_var(tab))
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

  expect_snapshot_error(set_pop_treat_var(tab, a))
  expect_snapshot_error(set_pop_treat_var(tab, A))
  expect_snapshot_error(set_pop_treat_var(tab))
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

##### header_n tests #####
test_that("set_header_n binds header_n data properly", {
  tab <- tplyr_table(mtcars, gear)
  hn <- data.frame(gear = c(3, 4, 5), n = c(15L, 12L, 5L))

  tab <- set_header_n(tab, hn)
  expect_equal(header_n(tab), hn)

  # Assignment operator
  hn2 <- data.frame(gear = c(3, 4, 5), n = c(10L, 8L, 3L))
  header_n(tab) <- hn2
  expect_equal(header_n(tab), hn2)
})

test_that("set_header_n validates inputs", {
  tab <- tplyr_table(mtcars, gear)

  expect_error(set_header_n(tab, "not a data frame"))
  expect_error(set_header_n(tab, data.frame(a = 1)))
  expect_error(set_header_n(tab, data.frame(n = "a")))
})

##### where binding tests #####
test_that("get_where and set_where work on tplyr_table", {
  tab <- tplyr_table(mtcars, gear)

  tab <- set_where(tab, cyl == 4)
  w <- get_where(tab)
  expect_true(is_quosure(w))
})

test_that("set_pop_where and get_pop_where work", {
  tab <- tplyr_table(mtcars, gear)

  tab <- set_pop_where(tab, cyl > 4)
  w <- get_pop_where(tab)
  expect_true(is_quosure(w))
})

##### table format default tests #####
test_that("desc layer format defaults can be set and retrieved", {
  tab <- tplyr_table(mtcars, gear)

  tab <- set_desc_layer_formats(tab,
    TEST = f_str("xx.x", mean)
  )
  fmts <- get_desc_layer_formats(tab)
  expect_true(is.list(fmts))
  expect_true("TEST" %in% names(fmts))
})

test_that("count layer format defaults can be set and retrieved", {
  tab <- tplyr_table(mtcars, gear)

  tab <- set_count_layer_formats(tab,
    n_counts = f_str("xx (xx.x%)", n, pct)
  )
  fmts <- get_count_layer_formats(tab)
  expect_true(is.list(fmts))
})

test_that("shift layer format defaults can be set and retrieved", {
  tab <- tplyr_table(mtcars, gear)

  tab <- set_shift_layer_formats(tab,
    f_str("xx", n)
  )
  fmts <- get_shift_layer_formats(tab)
  expect_true(is.list(fmts))
})
