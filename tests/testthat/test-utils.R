context('utils.R')

## modify_nested_call ----
test_that("Call must be quoted", {
  expect_error(Tplyr:::modify_nested_call(mean(c(1,2,3))), "`call` must be a quoted call")
  c <- quo(tplyr_table(treat_var = Species))
  expect_silent(Tplyr:::modify_nested_call(c))
})

test_that("With no additional parameters, a call returns unchanged", {
  c <- quo(tplyr_table(treat_var = Species))
  r <- Tplyr:::modify_nested_call(c)
  expect_equal(c, r)
})

test_that("By default, only `Tplyr` exported functions are allowed", {
  # Non-tplyr function
  c <- quo(mean(c(1,2,3)))
  expect_error(Tplyr:::modify_nested_call(c), "Functions called within `add_layer` must be part of `Tplyr`")

  # Non-exported Tplyr function
  # c <- quo(Tplyr:::modify_nested_call(quo(x %>% y)))
  # expect_silent(Tplyr:::modify_nested_call(c))

  # Exported Tplyr function
  c <- quo(tplyr_table(treat_var = Species))
  expect_silent(Tplyr:::modify_nested_call(c))
})

test_that("Setting `allowable_calls` to null allows calls from any namespace", {
  # Non-tplyr function
  c <- quo(mean(c(1,2,3)))
  expect_silent(Tplyr:::modify_nested_call(c, allowable_calls = NULL))

  # Non-exported Tplyr function
  c <- quo(Tplyr:::modify_nested_call(quo(x %>% y)))
  expect_silent(Tplyr:::modify_nested_call(c, allowable_calls = NULL))

  # Exported Tplyr function
  c <- quo(var() %>% tplyr_table(treat_var = Species) %>% print())
  expect_silent(Tplyr:::modify_nested_call(c, allowable_calls = NULL))
})

test_that("Calls are modified and evaluate - no piping", {
  c <- quo(mean(c(1,2,3, NA)))
  r <- Tplyr:::modify_nested_call(c, na.rm=TRUE, allowable_calls = NULL)
  expect_true(is.na(eval(quo_get_expr(c))))
  expect_equal(eval(quo_get_expr(r)), 2)
})

test_that("Calls are modified and evaluate - piping", {
  c <- quo(mean(c(1,2,3, NA)) %>% print())
  r <- Tplyr:::modify_nested_call(c, na.rm=TRUE, allowable_calls = NULL)

  expect_output(eval(quo_get_expr(c)), "NA")
  expect_output(eval(quo_get_expr(r)), "2")
})

test_that("Multiple pipes are processed appropriately", {
  c <- quo(c(1,2,3) %>% mean() %>% print())
  r <- Tplyr:::modify_nested_call(c, a = NA, allowable_calls = NULL)

  expect_output(eval(quo_get_expr(c)), "2")
  expect_output(eval(quo_get_expr(r)), "NA")
})



