# Tests for process_formatting.desc_layer()
# These tests verify the Extract-Process-Bind refactoring

library(testthat)
library(dplyr)
library(tidyr)

# Test data setup
test_data <- tibble::tibble(
  gear = factor(c(3, 3, 3, 4, 4, 4, 5, 5, 5)),
  mpg = c(21.4, 21.5, 18.1, 24.4, 22.8, 32.4, 30.4, 26.0, 15.8),
  wt = c(3.2, 3.1, 3.5, 2.8, 3.0, 2.2, 1.9, 2.1, 3.8),
  am = factor(c(0, 0, 0, 1, 1, 1, 1, 1, 0))
)

test_that("process_formatting.desc_layer formats output correctly", {
  # Create a desc layer and process summaries
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg) %>%
        set_format_strings(
          "n" = f_str("xx", n),
          "Mean (SD)" = f_str("xx.x (xx.xx)", mean, sd)
        )
    )
  
  # Build the table to trigger processing
  result <- build(t)
  
  # Verify the output has expected structure
  expect_true(any(grepl("row_label", names(result))))
  expect_true(any(grepl("var1_", names(result))))
  
  # Verify formatting was applied (should have formatted strings)
  expect_true(all(sapply(result[, grepl("var1_", names(result))], is.character)))
  
  # Verify we have the expected number of rows (one per statistic)
  expect_equal(nrow(result), 2)
})

test_that("process_formatting.desc_layer handles multiple target variables", {
  # Create a desc layer with multiple target variables
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(vars(mpg, wt)) %>%
        set_format_strings(
          "n" = f_str("xx", n),
          "Mean" = f_str("xx.x", mean)
        )
    )
  
  result <- build(t)
  
  # Should have columns for both variables
  expect_true(any(grepl("var1_", names(result))))
  expect_true(any(grepl("var2_", names(result))))
  
  # Should have 2 rows (one per statistic)
  expect_equal(nrow(result), 2)
})

test_that("process_formatting.desc_layer handles by variables", {
  # Create a desc layer with by variable
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg, by = am) %>%
        set_format_strings(
          "n" = f_str("xx", n),
          "Mean" = f_str("xx.x", mean)
        )
    )
  
  result <- build(t)
  
  # Should have row_label columns (row_label1 for statistic, row_label2 for by variable)
  expect_true(any(grepl("row_label", names(result))))
  
  # Should have multiple rows (one per statistic per am level)
  expect_true(nrow(result) >= 2)
})

test_that("process_formatting.desc_layer handles stats_as_columns", {
  # Create a desc layer with stats as columns
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg) %>%
        set_format_strings(
          "n" = f_str("xx", n),
          "Mean" = f_str("xx.x", mean)
        ) %>%
        set_stats_as_columns()
    )
  
  result <- build(t)
  
  # Should have row_label1 column (contains treatment groups)
  expect_true("row_label1" %in% names(result))
  
  # Should have columns for each statistic
  expect_true(any(grepl("var1_n", names(result))))
  expect_true(any(grepl("var1_Mean", names(result))))
})

test_that("process_formatting.desc_layer does not pollute layer environment", {
  # Create a table and build it
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg) %>%
        set_format_strings(
          "n" = f_str("xx", n),
          "Mean (SD)" = f_str("xx.x (xx.xx)", mean, sd)
        )
    )
  
  # Build the table (this processes summaries and formatting)
  result <- build(t)
  
  # Get the layer after processing
  layer <- t$layers[[1]]
  
  # Verify temporary variables are NOT in the layer environment
  expect_false(exists("form_sums", envir = layer, inherits = FALSE))
  expect_false(exists("i", envir = layer, inherits = FALSE))
  expect_false(exists("current_trans_sum", envir = layer, inherits = FALSE))
  expect_false(exists("prec", envir = layer, inherits = FALSE))
  
  # Verify expected results ARE in the layer environment
  expect_true(exists("formatted_data", envir = layer, inherits = FALSE))
})

test_that("process_formatting.desc_layer handles precision data correctly", {
  # Create a desc layer with auto precision
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg, by = am) %>%
        set_format_strings(
          "Mean (SD)" = f_str("a.a+1 (a.a+2)", mean, sd)
        )
    )
  
  result <- build(t)
  
  # Should complete without error and produce formatted output
  expect_true(nrow(result) > 0)
  expect_true(any(grepl("var1_", names(result))))
  
  # Verify formatting was applied
  expect_true(all(sapply(result[, grepl("var1_", names(result))], is.character)))
})

test_that("process_formatting.desc_layer handles cols parameter", {
  # Create a desc layer with cols
  t <- tplyr_table(test_data, gear, cols = am) %>%
    add_layer(
      group_desc(mpg) %>%
        set_format_strings(
          "n" = f_str("xx", n),
          "Mean" = f_str("xx.x", mean)
        )
    )
  
  result <- build(t)
  
  # Should have columns for each treatment group and col combination
  expect_true(any(grepl("var1_.*_0", names(result))))
  expect_true(any(grepl("var1_.*_1", names(result))))
})

test_that("process_formatting.desc_layer preserves existing functionality", {
  # This is a regression test using a more complex example
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(vars(mpg, wt), by = am) %>%
        set_format_strings(
          "n" = f_str("xx", n),
          "Mean (SD)" = f_str("xx.x (xx.xx)", mean, sd),
          "Median" = f_str("xx.x", median),
          "Min, Max" = f_str("xx.x, xx.x", min, max)
        )
    )
  
  # Should build without error
  expect_silent(result <- build(t))
  
  # Should have expected structure
  expect_true(any(grepl("row_label", names(result))))
  expect_true(any(grepl("var1_", names(result))))
  expect_true(any(grepl("var2_", names(result))))
  
  # Should have multiple rows
  expect_true(nrow(result) > 0)
})
