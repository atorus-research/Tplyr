dat <- tibble::tribble(
  ~row_label1, ~row_label2, ~row_label3, ~row_label4, ~var1,
  "A",         "C",         "G",         "M",         1L,
  "A",         "C",         "G",         "N",         2L,
  "A",         "C",         "H",         "O",         3L,
  "A",         "D",         "H",         "P",         4L,
  "A",         "D",         "I",         "Q",         5L,
  "A",         "D",         "I",         "R",         6L,
  "B",         "E",         "J",         "S",         7L,
  "B",         "E",         "J",         "T",         8L,
  "B",         "E",         "K",         "U",         9L,
  "B",         "F",         "K",         "V",         10L,
  "B",         "F",         "L",         "W",         11L
)


test_that("Errors generate as expected", {
  expect_error(collapse_row_labels(1, blah, blah), "x must be a data frame")
  expect_error(
    collapse_row_labels(dat, row_label1, row_label2, indent = 1),
    "indent must be a character string"
  )
  expect_error(
    collapse_row_labels(dat, row_label1, missing_col),
    "Columns provided to dots are missing from x."
  )
  expect_error(
    collapse_row_labels(dat, row_label1, "row_label2"),
    "Columns provided to dots must be provided as unquoted symbols."
  )

  expect_error(
    collapse_row_labels(dat, row_label1, row_label2, target_col = "RL"),
    "target_col must be provided as an unquoted symbol."
  )
  expect_error(
    collapse_row_labels(dat, row_label1),
    "Must have two or more columns to collapse"
  )
})

test_that("Row labels collapse appropriately", {

  x <- collapse_row_labels(dat, row_label1, row_label2, row_label3, row_label4)

  expect_equal(
    x$row_label[1:6],
    c("A", "  C", "    G", "      M", "      N", "A")
  )

  x <- collapse_row_labels(dat, row_label1, row_label2, row_label3, row_label4, indent = " ")
  expect_equal(
    x$row_label[1:6],
    c("A", " C", "  G", "   M", "   N", "A")
  )

  x <- collapse_row_labels(dat, row_label1, row_label2, row_label3)
  expect_equal(names(x), c("row_label", "row_label4", "var1"))
  expect_equal(
    x$row_label[1:6],
    c("A", "  C", "    G", "    G", "    H", "A")
  )
  expect_equal(
    x$row_label4[1:6],
    c("", "", "M", "N", "O", "")
  )

  x <- collapse_row_labels(dat, row_label1, row_label2, row_label3, target_col = rl)
  expect_equal(names(x), c("rl", "row_label4", "var1"))
})
