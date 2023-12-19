test_that("Test replacement of leading whitespace", {
  x <- c("Hello there", "  Goodbye Friend ",  "\tNice to meet you", "  \t What are you up to? \t \t ")

  expect_equal(
    replace_leading_whitespace(x),
    c("Hello there", "&nbsp;&nbsp;Goodbye Friend ",
      "&nbsp;&nbsp;&nbsp;&nbsp;Nice to meet you", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;What are you up to? \t \t ")
  )

  expect_equal(
    replace_leading_whitespace(x, tab=2),
    c("Hello there", "&nbsp;&nbsp;Goodbye Friend ",
      "&nbsp;&nbsp;Nice to meet you", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;What are you up to? \t \t ")
  )
})
