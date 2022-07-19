test_that("str_indent_wrap errors properly", {
  expect_error(str_indent_wrap(1), regexp = 'x must be a character vector')
})

test_that("str_indent_wrap wraps text properly", {
  text1 <- c("RENAL AND URINARY DISORDERS", "   NEPHROLITHIASIS")
  text2 <- c("RENAL AND URINARY DISORDERS", "\tNEPHROLITHIASIS")
  text3 <- c("RENAL AND URINARY DISORDERS", "\t\tNEPHROLITHIASIS")

  expect_equal(
    str_indent_wrap(text1, width=8),
    c("RENAL\nAND\nURINARY\nDISORDE-\nRS", "   NEPHROL-\n   ITHIASI-\n   S")
  )

  expect_equal(
    str_indent_wrap(text2, width=9, tab_width=4),
    c("RENAL AND\nURINARY\nDISORDER-\nS","    NEPHROLI-\n    THIASIS")
  )

  expect_equal(
    str_indent_wrap(text3, width=9, tab_width=2),
    c("RENAL AND\nURINARY\nDISORDER-\nS","    NEPHROLI-\n    THIASIS")
  )
})
