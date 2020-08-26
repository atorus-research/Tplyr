

mtcars$cyl2 <- mtcars$cyl + 10
t1 <- tplyr_table(mtcars, gear)
t2 <- tplyr_table(mtcars, gear)
t3 <- tplyr_table(mtcars, gear)

s1 <- group_shift(t1, vars(row = cyl, column = cyl2)) %>%
  set_format_strings(f_str("a", n))
s2 <- group_shift(t2, vars(row = cyl, column = cyl2)) %>%
  set_format_strings(f_str("a (xx.xx%)", n, pct))
s3 <- group_shift(t3, vars(row = cyl, column = cyl2)) %>%
  set_format_strings(f_str("a (xx.xx%)", n, pct)) %>%
  set_denoms_by(cyl)

t1 <- add_layers(t1, s1)
t2 <- add_layers(t2, s2)
t3 <- add_layers(t3, s3)

test_that("group_shift layers can be built without warnings/errors", {
  expect_silent(build(t1))
  expect_silent(build(t2))
  expect_silent(build(t3))
})

test_that("group_shift outputs the expected numeric data", {
  expect_equal(dim(s1$numeric_data), c(27, 4))
  expect_equal(s1$numeric_data$n, c(1, 0, 0, 0, 2, 0, 0, 0, 12, 8, 0, 0, 0, 4,
                                    0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 2))

  expect_equal(dim(s2$numeric_data), c(27, 5))
  expect_equal(s2$numeric_data$n, c(1, 0, 0, 0, 2, 0, 0, 0, 12, 8, 0, 0, 0, 4,
                                    0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 2))
  expect_equal(s2$numeric_data$total, c(15, 15, 15, 15, 15, 15, 15, 15, 15, 12, 12, 12,
                                        12, 12, 12, 12, 12, 12, 5, 5, 5, 5, 5, 5, 5, 5,
                                        5))

  expect_equal(dim(s3$numeric_data), c(27, 5))
  expect_equal(s3$numeric_data$n, c(1, 0, 0, 8, 0, 0, 2, 0, 0, 0, 2, 0, 0, 4, 0, 0, 1, 0, 0, 0,
                                    12, 0, 0, 0, 0, 0, 2))
  expect_equal(s3$numeric_data$total, c(11, 11, 11, 11, 11, 11, 11, 11, 11, 7,
                                      7, 7, 7, 7, 7, 7, 7, 7, 14, 14, 14, 14,
                                      14, 14, 14, 14, 14))

})

test_that("group_shift outputs the expected formatted data", {
  expect_equal(dim(s1$formatted_data), c(3, 11))
  expect_equal(dim(s2$formatted_data), c(3, 11))
  expect_equal(dim(s3$formatted_data), c(3, 11))


})

test_that("Shift layer clauses with invalid syntax give informative error", {
  t <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_shift(vars(row=vs, column=am), where=bad == code)
    )

  expect_error(build(t), "group_shift `where` condition `bad == code` is invalid.")
})
