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
  expect_error(add_column_headers(iris2, less_spanner), err)
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

test_that("add_column_headers throws an error when you use a token and don't pass header_n", {
  expect_error({
    mtcars2 <- mtcars %>%
      mutate_all(as.character)

    t <- tplyr_table(mtcars2, am) %>%
      add_layer(
        group_count(cyl)
      )

    b_t <- build(t) %>% mutate_all(as.character)

    count_string <- "Rows | am0 **0** | am1 **1**"

    add_column_headers(b_t, count_string)
  }, "You must pass a header_n if you are using replacement tokens")
})

test_that("add_column_headers returns the expected result when tokens are passed", {
  mtcars2 <- mtcars %>%
    mutate_all(as.character)

  t <- tplyr_table(mtcars2, vs, cols = am) %>%
    add_layer(
      group_count(cyl)
    )

  b_t <- build(t) %>%
    select(starts_with("row_label1") | starts_with("var1_"))

  count_string <- "Rows | V N=**0** {auto N=**0_0** | man N=**0_1**} | S N=**1** {auto N=**1_0** | man N=**1_1**}"

  tab <- add_column_headers(b_t, count_string, header_n(t))
  # The strucutre was visually checked
  expect_equal(tab, structure(list(row_label1 = c("", "Rows", "4", "6", "8"),
                                   var1_0_0 = c("V N=18", "auto N=12", " 0 (  0.0%)", " 0 (  0.0%)", "12 (100.0%)"),
                                   var1_0_1 = c("", "man N=6", " 1 ( 16.7%)", " 3 ( 50.0%)", " 2 ( 33.3%)"),
                                   var1_1_0 = c("S N=14", "auto N=7", " 3 ( 42.9%)", " 4 ( 57.1%)", " 0 (  0.0%)"),
                                   var1_1_1 = c("", "man N=7", " 7 (100.0%)", " 0 (  0.0%)", " 0 (  0.0%)")),
                              row.names = c(NA, -5L),
                              class = c("tbl_df", "tbl", "data.frame")))
})

