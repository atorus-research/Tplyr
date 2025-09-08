string <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)", "-1 (-0.56, -.75) -523%")

test_that("String extractor errors generate properly", {
  expect_error(
    str_extract_fmt_group(c(1), 1),
    "Parameter `string`"
  )

  expect_error(
    str_extract_fmt_group(string, "hi"),
    "Parameter `format_group`"
  )

  expect_error(
    str_extract_num(c(1), 1),
    "Parameter `string`"
  )

  expect_error(
    str_extract_num(string, "hi"),
    "Parameter `format_group`"
  )
})

test_that("Format groups can be extracted", {
  expect_equal(
    str_extract_fmt_group(string, 1),
    c(' 0', ' 8', '78', "-1")
  )

  expect_equal(
    str_extract_fmt_group(string, 2),
    c("(0.0%)", "(9.3%)", "(90.7%)", "(-0.56,")
  )

  expect_equal(
    str_extract_fmt_group(string, 3),
    c(rep(NA_character_, 3), "-.75)")
  )
})

test_that("Numbers from format groups can be extracted", {
  expect_equal(
    str_extract_num(string, 1),
    c(0, 8, 78, -1)
  )

  expect_equal(
    str_extract_num(string, 2),
    c(0.0, 9.3, 90.7, -.56)
  )

  expect_equal(
    str_extract_num(string, 3),
    c(rep(NA_real_, 3), -.75)
  )

  expect_equal(
    str_extract_num(string, 4),
    c(rep(NA_real_, 3), -523)
  )

})
