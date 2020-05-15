test_that("safety_table returns an empty envrionment of class 'safety_table' when passed no arguemnts", {
  st <- safety_table()
  expect_true(is.environment(st))
})
