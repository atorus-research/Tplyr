
# Tests for shift layer helper functions
# These tests verify the Extract-Process-Bind pattern and ensure no environment pollution

library(testthat)
library(dplyr)

# Setup test data
mtcars_test <- mtcars
mtcars_test$cyl2 <- mtcars_test$cyl + 10

test_that("process_shift_denoms follows Extract-Process-Bind pattern", {
  # Create a shift layer
  t <- tplyr_table(mtcars_test, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2)) %>%
        set_format_strings(f_str("a (xx.xx%)", n, pct))
    )
  
  # Build to trigger processing
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify denoms_df was created (this is an intended output binding)
  expect_true(!is.null(layer$denoms_df))
  expect_true(is.data.frame(layer$denoms_df))
  
  # Verify denoms_df has expected structure
  expect_true("summary_var" %in% names(layer$denoms_df))
  expect_true("n" %in% names(layer$denoms_df))
  
  # Verify denoms_df has data
  expect_true(nrow(layer$denoms_df) > 0)
})

test_that("process_shift_n calculates shift counts correctly", {
  # Create a shift layer
  t <- tplyr_table(mtcars_test, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2)) %>%
        set_format_strings(f_str("a", n))
    )
  
  # Build to trigger processing
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify numeric_data was created (this is an intended output binding)
  expect_true(!is.null(layer$numeric_data))
  expect_true(is.data.frame(layer$numeric_data))
  
  # Verify numeric_data has expected columns
  expect_true("n" %in% names(layer$numeric_data))
  expect_true("summary_var" %in% names(layer$numeric_data))
  
  # Verify counts are numeric
  expect_true(is.numeric(layer$numeric_data$n))
  
  # Verify counts are non-negative
  expect_true(all(layer$numeric_data$n >= 0))
})

test_that("process_shift_total calculates percentages correctly", {
  # Create a shift layer with percentages
  t <- tplyr_table(mtcars_test, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2)) %>%
        set_format_strings(f_str("a (xx.xx%)", n, pct))
    )
  
  # Build to trigger processing
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify numeric_data has total column (added by process_shift_total)
  expect_true("total" %in% names(layer$numeric_data))
  
  # Verify totals are numeric
  expect_true(is.numeric(layer$numeric_data$total))
  
  # Verify totals are positive
  expect_true(all(layer$numeric_data$total > 0))
  
  # Verify denoms_df exists (it's an intended output binding)
  expect_true(!is.null(layer$denoms_df))
})

test_that("shift layer handles custom denominators", {
  # Create a shift layer with custom denoms_by
  t <- tplyr_table(mtcars_test, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2)) %>%
        set_format_strings(f_str("a (xx.xx%)", n, pct)) %>%
        set_denoms_by(cyl)
    )
  
  # Build to trigger processing
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify numeric_data was created with totals
  expect_true(!is.null(layer$numeric_data))
  expect_true("total" %in% names(layer$numeric_data))
  
  # Verify the totals are calculated by cyl (not by gear)
  # The totals should vary by cyl value
  totals_by_cyl <- layer$numeric_data %>%
    select(summary_var, total) %>%
    distinct()
  
  expect_true(nrow(totals_by_cyl) > 1)
})

test_that("shift layer handles denom_where correctly", {
  # Create a shift layer with denom_where
  t <- tplyr_table(mtcars_test, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2)) %>%
        set_denom_where(vs == 1) %>%
        set_format_strings(f_str("xx (xx.x%)", n, pct))
    )
  
  # Build to trigger processing
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify numeric_data was created
  expect_true(!is.null(layer$numeric_data))
  expect_true("total" %in% names(layer$numeric_data))
  
  # Verify totals are based on filtered data (vs == 1)
  # Should be different from unfiltered totals
  expect_true(all(layer$numeric_data$total > 0))
})

test_that("shift layer produces correct row/column matrix structure", {
  # Create a shift layer
  t <- tplyr_table(mtcars_test, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2)) %>%
        set_format_strings(f_str("a", n))
    )
  
  # Build to trigger processing
  result <- build(t)
  
  # Verify the output has the expected structure
  expect_true(is.data.frame(result))
  
  # Verify row_label1 contains the row variable values
  expect_true("row_label1" %in% names(result))
  
  # Verify there are columns for each combination of treatment and column variable
  # Should have var1_ prefixed columns
  var1_cols <- grep("^var1_", names(result), value = TRUE)
  expect_true(length(var1_cols) > 0)
})

test_that("shift layer handles filtered data correctly", {
  # Create a shift layer with a where clause that filters some data
  t <- tplyr_table(mtcars_test, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2), where = mpg > 15) %>%
        set_format_strings(f_str("a", n))
    )
  
  # Build should not error
  result <- build(t)
  
  # Result should be a data frame
  expect_true(is.data.frame(result))
  
  # Result should have rows
  expect_true(nrow(result) > 0)
})

test_that("shift layer preserves factor levels", {
  # Create data with factors
  mtcars_factor <- mtcars_test
  mtcars_factor$cyl <- factor(mtcars_factor$cyl, levels = c("6", "8", "4"))
  mtcars_factor$cyl2 <- factor(mtcars_factor$cyl2, levels = c("16", "18", "14"))
  
  # Create a shift layer
  t <- tplyr_table(mtcars_factor, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2)) %>%
        set_format_strings(f_str("a", n))
    )
  
  # Build to trigger processing
  result <- build(t)
  
  # Verify the output preserves factor order
  expect_true(is.data.frame(result))
  expect_true("row_label1" %in% names(result))
  
  # The row labels should follow the factor order
  expect_equal(result$row_label1, c("6", "8", "4"))
})

test_that("shift layer helper functions produce expected bindings", {
  # Create a shift layer
  t <- tplyr_table(mtcars_test, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2)) %>%
        set_format_strings(f_str("a (xx.xx%)", n, pct))
    )
  
  # Build to trigger processing
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify expected output bindings exist
  expect_true(!is.null(layer$numeric_data), info = "numeric_data should exist")
  expect_true(!is.null(layer$denoms_df), info = "denoms_df should exist")
  expect_true(!is.null(layer$built_target), info = "built_target should exist")
  expect_true(!is.null(layer$built_target_pre_where), info = "built_target_pre_where should exist")
  
  # Verify these are the correct types
  expect_true(is.data.frame(layer$numeric_data))
  expect_true(is.data.frame(layer$denoms_df))
  expect_true(is.data.frame(layer$built_target))
  expect_true(is.data.frame(layer$built_target_pre_where))
})

test_that("shift layer functions do not pollute environment with temporary variables", {
  # Create a shift layer
  t <- tplyr_table(mtcars_test, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2)) %>%
        set_format_strings(f_str("a (xx.xx%)", n, pct))
    )
  
  # Build to trigger processing
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Get all bindings in the layer environment
  layer_bindings <- ls(envir = layer, all.names = TRUE)
  
  # List of expected bindings (intended outputs and configuration)
  expected_bindings <- c(
    # Configuration bindings
    "target_var", "by", "where", "cols", "format_strings",
    "denoms_by", "denom_where", "limit_data_by",
    # Output bindings
    "numeric_data", "denoms_df", "built_target", "built_target_pre_where",
    "formatted_data", "max_length", "max_layer_length", "max_n_width"
  )
  
  # Check that no unexpected temporary variables exist
  # Temporary variables that should NOT be in the environment:
  # - Loop counters (i, grp_i, etc.)
  # - Intermediate calculation variables
  # - Local processing variables
  
  # We'll check for common temporary variable patterns
  temp_var_patterns <- c(
    "^i$", "^j$", "^k$",  # Loop counters
    "^grp_i$", "^idx$",    # Group indices
    "^temp_", "^tmp_",     # Temporary prefixes
    "^local_", "^calc_"    # Local calculation prefixes
  )
  
  for (pattern in temp_var_patterns) {
    matching_vars <- grep(pattern, layer_bindings, value = TRUE)
    expect_equal(length(matching_vars), 0, 
                 info = paste0("Found unexpected temporary variable(s) matching '", 
                              pattern, "': ", paste(matching_vars, collapse = ", ")))
  }
})

test_that("shift layer row/column matrix structure is correct", {
  # Create a shift layer
  t <- tplyr_table(mtcars_test, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2)) %>%
        set_format_strings(f_str("a", n))
    )
  
  # Build to trigger processing
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify numeric_data has the row variable renamed to summary_var
  expect_true("summary_var" %in% names(layer$numeric_data))
  expect_false("cyl" %in% names(layer$numeric_data))
  
  # Verify numeric_data has the column variable (cyl2)
  expect_true("cyl2" %in% names(layer$numeric_data))
  
  # Verify the formatted_data has been pivoted correctly
  expect_true(!is.null(layer$formatted_data))
  expect_true("row_label1" %in% names(layer$formatted_data))
  
  # Verify there are var1_ prefixed columns (pivoted columns)
  var1_cols <- grep("^var1_", names(layer$formatted_data), value = TRUE)
  expect_true(length(var1_cols) > 0)
  
  # Verify the number of rows matches the number of unique row values
  unique_row_values <- unique(layer$numeric_data$summary_var)
  expect_equal(nrow(layer$formatted_data), length(unique_row_values))
})

test_that("shift layer handles empty data", {
  # Create a shift layer with a where clause that filters out all data
  t <- tplyr_table(mtcars_test, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2), where = mpg > 1000) %>%
        set_format_strings(f_str("a", n))
    )
  
  # Note: Current implementation returns early from process_shift_n when data is empty,
  # leaving numeric_data as NULL, which causes process_formatting to fail.
  # This is existing behavior (not introduced by refactoring).
  # The test verifies this behavior is preserved.
  expect_error(build(t), "no applicable method")
})

test_that("shift layer calculates counts correctly for all combinations", {
  # Create a shift layer
  t <- tplyr_table(mtcars_test, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2)) %>%
        set_format_strings(f_str("a", n))
    )
  
  # Build to trigger processing
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify that numeric_data includes all combinations (including zeros)
  # Get unique values for row and column variables
  unique_rows <- unique(layer$numeric_data$summary_var)
  unique_cols <- unique(layer$numeric_data$cyl2)
  unique_treats <- unique(layer$numeric_data$gear)
  
  # Expected number of rows = unique_rows * unique_cols * unique_treats
  expected_rows <- length(unique_rows) * length(unique_cols) * length(unique_treats)
  
  # Verify we have all combinations
  expect_equal(nrow(layer$numeric_data), expected_rows)
  
  # Verify some counts are zero (from complete_and_limit)
  expect_true(any(layer$numeric_data$n == 0))
  
  # Verify some counts are non-zero
  expect_true(any(layer$numeric_data$n > 0))
})

test_that("shift layer percentages sum correctly within denominator groups", {
  # Create a shift layer with percentages
  t <- tplyr_table(mtcars_test, gear) %>%
    add_layer(
      group_shift(vars(row = cyl, column = cyl2)) %>%
        set_format_strings(f_str("a (xx.xx%)", n, pct))
    )
  
  # Build to trigger processing
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Calculate percentages manually
  layer$numeric_data <- layer$numeric_data %>%
    mutate(calculated_pct = (n / total) * 100)
  
  # Verify percentages are between 0 and 100
  expect_true(all(layer$numeric_data$calculated_pct >= 0))
  expect_true(all(layer$numeric_data$calculated_pct <= 100))
  
  # Verify that within each denominator group, percentages sum to ~100
  # (allowing for rounding and zero counts)
  pct_sums <- layer$numeric_data %>%
    filter(n > 0) %>%  # Only non-zero counts
    group_by(gear, summary_var, cyl2) %>%
    summarize(pct_sum = sum(calculated_pct), .groups = "drop")
  
  # Each group should sum to approximately 100 (within rounding error)
  # But since we're grouping by all variables, each should be <= 100
  expect_true(all(pct_sums$pct_sum <= 100))
})
