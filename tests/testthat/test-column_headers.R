context("column_headers.R")

# Need a simple data frame to test with
iris2 <- iris %>%
  mutate_all(as.character)

test_that("All columns must be character", {
  expect_error(add_column_headers(iris, "header_text"), "When binding headers")
})

test_that("Nested headers are not allowed", {
  header_string = "TEXT | TEXT {TEXT {TEXT} TEXT } | TEXT"
  expect_error(add_column_headers(iris2, header_string), "Nested spanning headers")
})

test_that("Header strings must have the same number of columns as the data frame", {
  # Create different header strings
  good_no_spanner <- "Sepal Length | Sepal Width | Petal Length | Petal Width | Species"
  less_no_spanner <- "Sepal Length | Sepal Width | Petal Length | Petal Width"
  more_no_spanner <- "Sepal Length | Sepal Width | Petal Length | Petal Width | Species | More"
  good_spanner    <- "Sepal {Length | Width} | Petal {Length | Width} | Species"
  less_spanner    <- "Sepal {Length | Width} | Petal {Length | Width}"
  more_spanner    <- "Sepal {Length | Width} | Petal {Length | Width} | Species | More"
  nested_less     <- "Sepal {Length | Width} | Petal {Length} | Species"
  nested_more     <- "Sepal {Length | Width} | Petal {Length | Width | More} | Species"

  # Test the results
  expect_silent(add_column_headers(iris2, good_no_spanner))
  expect_silent(add_column_headers(iris2, good_spanner))
  err <- "Number of columns provided"
  expect_error(add_column_headers(iris2, less_no_spanner), err)
  expect_error(add_column_headers(iris2, more_no_spanner), err)
  expect_error(add_column_headers(iris2, less_spanner), "Malformed column header string")
  expect_error(add_column_headers(iris2, more_spanner), err)
  expect_error(add_column_headers(iris2, nested_less), err)
  expect_error(add_column_headers(iris2, nested_more), err)

})

test_that("Unmatched spanner brackers", {
  header_string = "TEXT | TEXT {TEXT {TEXT} TEXT  | TEXT"
  expect_error(add_column_headers(iris2, header_string), "Unmatched brackets for spanning headers")
})

test_that("Spanning headers produce correctly", {

  # Header text with no spanner
  header_text_no_spanner <- "Sepal Length | Sepal Width | Petal Length | Petal Width | Species"
  # manually construct the expected data frame for the headers
  header_df_no_spanner <- data.frame(
    Sepal.Length = c("Sepal Length"),
    Sepal.Width = c("Sepal Width"),
    Petal.Length = c("Petal Length"),
    Petal.Width = c("Petal Width"),
    Species = c("Species"),
    stringsAsFactors = FALSE)

  # Header text with a spanner
  header_text_with_spanner <- "Sepal {Length | Width} | Petal {Length | Width} | Species"
  # manually construct the expected data frame for the headers
  header_df_with_spanner <- data.frame(
    Sepal.Length = c("Sepal", "Length"),
    Sepal.Width = c("", "Width"),
    Petal.Length = c("Petal", "Length"),
    Petal.Width = c("", "Width"),
    Species = c("", "Species"),
    stringsAsFactors = FALSE)

  # Assume that all columns should match
  expect_true(all(add_column_headers(iris2, header_text_no_spanner) == bind_rows(header_df_no_spanner, iris2)))
  expect_true(all(add_column_headers(iris2, header_text_with_spanner) == bind_rows(header_df_with_spanner, iris2)))

})

