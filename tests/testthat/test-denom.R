

t <- tplyr_table(mtcars, gear)

test_that("this_denom can be called after a group_by and gives totals", {
  df <- mtcars %>%
    group_by(gear) %>%
    do(this_denom(., header_n(t))) %>%
    ungroup()

  expect_equal(df, tibble(gear  = c(3, 4, 5),
                          total = c(15, 12, 5)))

  df2 <- mtcars %>%
    group_by(gear, vs) %>%
    do(this_denom(., header_n(t))) %>%
    ungroup()

  expect_equal(df2, tibble(gear  = c(3, 3, 4, 4, 5, 5),
                           vs    = c(0, 1, 0, 1, 0, 1),
                           total = c(15, 15, 12, 12, 5, 5)))
})

test_that("this_denom raises error when bad groups are passed", {
  expect_error({
    mtcars %>%
      # Not grouped by gear
      group_by(cyl) %>%
      do(this_denom(., header_n(t)))
  }, "All columns in the call must be in separate groups in the table.")
})
