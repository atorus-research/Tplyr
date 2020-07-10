##### add_total_group #####
test_that("add_total_group adds treat_grps bindings properly", {
  tab <- tplyr_table(iris, Species)

  expect_equal(treat_grps(tab), list())
  add_total_group(tab)
  expect_equal(treat_grps(tab), list(Total = c("setosa", "versicolor", "virginica")))

})
