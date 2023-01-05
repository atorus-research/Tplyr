test_that("Proper regular expressions are returned", {
  expect_equal(
    as.character(get_tplyr_regex('format_string')),
    "(a(\\+\\d+)?|(\\S+)A(\\+\\d+)?|(\\S+)X+|x+)(\\.([A|a](\\+\\d+)?|[X|x]+)?)?"
  )

  expect_equal(
    as.character(get_tplyr_regex('format_group')),
    "[^\\s\\d]*\\s*(\\-?\\d+(\\.\\d+)?)\\S*"
  )
})
