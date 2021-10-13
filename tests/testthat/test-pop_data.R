##### add_total_group #####

test_that("add_treat_grps errors function properly", {

  t <- tplyr_table(mtcars, gear)

  # Must be named
  expect_error(add_treat_grps(t, c("blah", "bloh")), "Treatment group arguments must have names")

  # Must attach to tplyr_table
  l <- group_count(t, am)
  expect_error(add_treat_grps(l, "one" = c(1,2,3)), msg = "Treatment groups can only be added to `tplyr_table` objects")
})

test_that("add_total_group errors function properly", {
  t <- tplyr_table(mtcars, gear)
  expect_error(add_total_group(t, 1), msg = "Argument `group_name` in function `add_total_group` must be character")
})

test_that("add_total_group adds treat_grps bindings properly", {
  tab <- tplyr_table(iris, Species)

  expect_equal(treat_grps(tab), list())
  add_total_group(tab)
  expect_equal(treat_grps(tab), list(Total = c("setosa", "versicolor", "virginica")))

})

# Multiple calls continually append
test_that("add_treat_grps and add_total_grps properly append existing groups", {
  t <- tplyr_table(mtcars, gear) %>%
    add_treat_grps(a = c("3", "4"), b = c("1", "2")) %>%
    add_total_group()

  expect_equal(treat_grps(t), list(a = c("3", "4"), b=c("1", "2"), Total=c("4", "3", "5")))

})

test_that("default header_n is built properly", {
  t <- tplyr_table(mtcars, gear) %>%
    add_total_group() %>%
    set_distinct_by(cyl) %>%
    add_layer(
      group_count(vs)
    )
  t_b <- build(t)

  expect_equal(header_n(t), tibble(gear = factor(c(3, 4, 5, "Total")), n = c(3, 2, 3, 3)))
})
