

t <- tplyr_table(mtcars, gear)

test_that("this_denom can be called after a group_by and gives totals", {
  df <- mtcars %>%
    group_by(gear) %>%
    do(this_denom(., header_n(t))) %>%
    ungroup() %>%
    select(total)

  expect_equal(df, tibble(total = rep(c(15, 12, 5), c(15, 12, 5))))

  df2 <- mtcars %>%
    group_by(gear, vs) %>%
    do(this_denom(., header_n(t))) %>%
    ungroup() %>%
    select(total)

  expect_equal(df2, tibble(total = rep(c(15, 12, 5), c(15, 12, 5))))
})

test_that("this_denom raises error when bad groups are passed", {
  expect_error({
    mtcars %>%
      # Not grouped by gear
      group_by(cyl) %>%
      do(this_denom(., header_n(t)))
  }, "All columns in the call must be in separate groups in the table.")
})
