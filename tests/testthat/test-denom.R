

t <- tplyr_table(mtcars, gear)
# Add header_n before build
header_n <- tibble(
  gear = c(3, 4, 5),
  n = c(15, 12, 5)
)
treat_var <- quo(gear)

test_that("this_denom can be called after a group_by and gives totals", {

  df <- mtcars %>%
    group_by(gear) %>%
    do(this_denom(., header_n, treat_var)) %>%
    ungroup() %>%
    select(total)

  expect_equal(df, tibble(total = rep(c(15, 12, 5), c(15, 12, 5))))

  df2 <- mtcars %>%
    group_by(gear, vs) %>%
    do(this_denom(., header_n, treat_var)) %>%
    ungroup() %>%
    select(total)

  expect_equal(df2, tibble(total = rep(c(15, 12, 5), c(15, 12, 5))))
})
