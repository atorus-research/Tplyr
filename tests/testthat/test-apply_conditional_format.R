test_string1 <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)", "12", "Howdy yall")
test_string2 <- c(" 0 ( 0.0%)", " 8 ( 9.3%)", "78 (90.7%) [ 5]", "12", "Howdy yall")

test_that("Test input validation and warning generation", {
  expect_error(
    apply_conditional_format(c(1), 2, x == 0, "(<1%)", full_string=TRUE),
     "Paramter `string`"
  )

  expect_error(
    apply_conditional_format(test_string1, "bad", x == 0, "(<1%)", full_string=TRUE),
    "Paramter `format_group`"
  )

  expect_error(
    apply_conditional_format(test_string1, 1.1, x == 0, "(<1%)", full_string=TRUE),
    "Paramter `format_group`"
  )

  expect_error(
    apply_conditional_format(test_string1, 2, "bad", "(<1%)", full_string=TRUE),
    "Condition must be"
  )

  expect_error(
    apply_conditional_format(test_string1, 2, x == 0, 1, full_string=TRUE),
    "Paramter `replacement"
  )

  expect_error(
    apply_conditional_format(test_string1, 2, x == 0, "(<1%)", full_string="TRUE"),
    "Paramter `full_string`"
  )

  expect_warning(
    apply_conditional_format(test_string1, 2, x == 0, "---------------"),
    "Replacement string length"
  )
})

test_that("Conditional formatting is correctly applied", {
  expect_equal(
    apply_conditional_format(test_string1, 2, x==0, " 0        ", full_string=TRUE),
    c(" 0        ", " 8  (9.3%)", "78 (90.7%)", "12", "Howdy yall")
  )

  expect_equal(
    apply_conditional_format(test_string1, 2, x==0, "(<1%)"),
    c(" 0   (<1%)", " 8  (9.3%)", "78 (90.7%)", "12", "Howdy yall")
  )

  expect_equal(
    apply_conditional_format(test_string2, 2, x==0, "( <1%)"),
    c(" 0  ( <1%)", " 8 ( 9.3%)", "78 (90.7%) [ 5]", "12", "Howdy yall")
  )

  expect_equal(
    apply_conditional_format(test_string2, 3, x==5, "--"),
    c(" 0 ( 0.0%)", " 8 ( 9.3%)", "78 (90.7%)   --", "12", "Howdy yall")
  )

  expect_equal(
    apply_conditional_format(test_string1, 1, x > 7 & x < 13, "--"),
    c(" 0  (0.0%)", "--  (9.3%)", "78 (90.7%)", "--", "Howdy yall")
  )
})



