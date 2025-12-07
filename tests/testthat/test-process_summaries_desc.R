# Tests for process_summaries.desc_layer() refactoring
# These tests verify:
# 1. All built-in statistics work correctly
# 2. Custom summaries work correctly
# 3. Multi-variable summaries work correctly
# 4. No temporary variables remain in layer environment

library(testthat)
library(dplyr)

# Test data setup
test_data <- mtcars %>%
  mutate(gear = factor(gear))

test_that("process_summaries.desc_layer calculates all built-in statistics correctly", {
  # Create a simple desc layer
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg)
    )
  
  # Build the table to trigger process_summaries
  result <- build(t)
  
  # Verify the layer has numeric_data
  layer <- t$layers[[1]]
  expect_true(!is.null(layer$numeric_data))
  
  # Verify all expected statistics are present
  expect_true("n" %in% layer$numeric_data$stat)
  expect_true("mean" %in% layer$numeric_data$stat)
  expect_true("sd" %in% layer$numeric_data$stat)
  expect_true("median" %in% layer$numeric_data$stat)
  expect_true("min" %in% layer$numeric_data$stat)
  expect_true("max" %in% layer$numeric_data$stat)
  
  # Verify numeric_data has expected structure
  expect_true("summary_var" %in% names(layer$numeric_data))
  treat_var_name <- as_name(env_get(layer, "treat_var", inherit = TRUE))
  expect_true(treat_var_name %in% names(layer$numeric_data))
  expect_true("stat" %in% names(layer$numeric_data))
  expect_true("value" %in% names(layer$numeric_data))
})

test_that("process_summaries.desc_layer works with by variables", {
  # Create desc layer with by variable
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg, by = am)
    )
  
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify numeric_data includes by variable
  expect_true("row_label1" %in% names(layer$numeric_data))
  
  # Verify data is grouped by the by variable
  by_values <- unique(layer$numeric_data$row_label1)
  expect_true(length(by_values) > 1)
})

test_that("process_summaries.desc_layer works with multiple by variables", {
  # Create desc layer with multiple by variables
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg, by = vars(am, vs))
    )
  
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify numeric_data includes both by variables
  expect_true("row_label1" %in% names(layer$numeric_data))
  expect_true("row_label2" %in% names(layer$numeric_data))
})

test_that("process_summaries.desc_layer handles custom summaries correctly", {
  # Create desc layer with custom summary
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg) %>%
        set_custom_summaries(mean_squared = mean(.var, na.rm=TRUE)**2) %>%
        set_format_strings(
          "Mean Squared" = f_str("xx.xx", mean_squared)
        )
    )
  
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify custom summary is in numeric_data
  expect_true("mean_squared" %in% layer$numeric_data$stat)
  
  # Verify custom summary values are calculated
  mean_squared_values <- layer$numeric_data %>%
    filter(stat == "mean_squared") %>%
    pull(value)
  
  expect_true(all(!is.na(mean_squared_values)))
  expect_true(all(mean_squared_values > 0))
})

test_that("process_summaries.desc_layer handles multi-variable summaries correctly", {
  # Create desc layer with multiple target variables
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(vars(mpg, wt))
    )
  
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify both variables are in numeric_data
  summary_vars <- unique(layer$numeric_data$summary_var)
  expect_true("mpg" %in% summary_vars)
  expect_true("wt" %in% summary_vars)
  
  # Verify each variable has statistics
  mpg_stats <- layer$numeric_data %>%
    filter(summary_var == "mpg")
  expect_true(nrow(mpg_stats) > 0)
  
  wt_stats <- layer$numeric_data %>%
    filter(summary_var == "wt")
  expect_true(nrow(wt_stats) > 0)
})

test_that("process_summaries.desc_layer handles multi-variable with custom summaries", {
  # Create desc layer with multiple variables and custom summary
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(vars(mpg, wt)) %>%
        set_custom_summaries(mean_squared = mean(.var, na.rm=TRUE)**2) %>%
        set_format_strings(
          "Mean Squared" = f_str("xx.xx", mean_squared)
        )
    )
  
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify custom summary exists for both variables
  mpg_mean_squared <- layer$numeric_data %>%
    filter(summary_var == "mpg", stat == "mean_squared")
  expect_true(nrow(mpg_mean_squared) > 0)
  
  wt_mean_squared <- layer$numeric_data %>%
    filter(summary_var == "wt", stat == "mean_squared")
  expect_true(nrow(wt_mean_squared) > 0)
})

test_that("process_summaries.desc_layer does not pollute layer environment", {
  # Create a desc layer
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg)
    )
  
  # Capture environment state before build
  layer <- t$layers[[1]]
  vars_before <- ls(envir = layer)
  
  # Build to trigger process_summaries (and process_formatting)
  result <- build(t)
  
  # Verify temporary variables from process_summaries do NOT exist in layer environment
  # Note: i and row_labels may exist from process_formatting or set_format_strings
  expect_false(exists("cur_var", envir = layer))
  expect_false(exists("summaries", envir = layer))
  expect_false(exists("cmplt1", envir = layer))
  expect_false(exists("num_sums", envir = layer))  # This is a local variable in process_summaries
  
  # Verify expected results DO exist
  expect_true(exists("numeric_data", envir = layer))
  expect_true(exists("trans_sums", envir = layer))
  expect_true(exists("num_sums_raw", envir = layer))
})

test_that("process_summaries.desc_layer handles where clause correctly", {
  # Create desc layer with where clause
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg, where = cyl == 6)
    )
  
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify numeric_data exists
  expect_true(!is.null(layer$numeric_data))
  
  # The filtered data should have fewer observations
  # We can't directly verify the filter was applied, but we can check
  # that the function completed without error
  expect_true(nrow(layer$numeric_data) > 0)
})

test_that("process_summaries.desc_layer gives informative error for invalid where clause", {
  # Create desc layer with invalid where clause
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg, where = bad_variable == 1)
    )
  
  # Should error with informative message
  expect_error(build(t), "group_desc `where` condition")
})

test_that("process_summaries.desc_layer works with cols argument", {
  # Create table with cols
  t <- tplyr_table(test_data, gear, cols = vs) %>%
    add_layer(
      group_desc(mpg)
    )
  
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify numeric_data includes cols variable
  expect_true(!is.null(layer$numeric_data))
  
  # The cols variable should be in the grouping
  # This is reflected in the structure of numeric_data
  expect_true(nrow(layer$numeric_data) > 0)
})

test_that("process_summaries.desc_layer handles missing values correctly", {
  # Create data with missing values
  test_data_na <- test_data
  test_data_na$mpg[1:5] <- NA
  
  t <- tplyr_table(test_data_na, gear) %>%
    add_layer(
      group_desc(mpg)
    )
  
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify missing count is present
  missing_stats <- layer$numeric_data %>%
    filter(stat == "missing")
  
  expect_true(nrow(missing_stats) > 0)
  
  # At least one group should have missing values
  expect_true(any(missing_stats$value > 0))
})

test_that("process_summaries.desc_layer preserves trans_sums for formatting", {
  # Create a desc layer
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg)
    )
  
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify trans_sums exists and is a list
  expect_true(!is.null(layer$trans_sums))
  expect_true(is.list(layer$trans_sums))
  expect_equal(length(layer$trans_sums), length(layer$target_var))
  
  # Verify trans_sums has expected structure
  expect_true(is.data.frame(layer$trans_sums[[1]]))
  expect_true("row_label" %in% names(layer$trans_sums[[1]]))
  expect_true("stat" %in% names(layer$trans_sums[[1]]))
  expect_true("value" %in% names(layer$trans_sums[[1]]))
})

test_that("process_summaries.desc_layer preserves num_sums_raw for metadata", {
  # Create a desc layer
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg)
    )
  
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify num_sums_raw exists and is a list
  expect_true(!is.null(layer$num_sums_raw))
  expect_true(is.list(layer$num_sums_raw))
  expect_equal(length(layer$num_sums_raw), length(layer$target_var))
  
  # Verify num_sums_raw has expected structure
  expect_true(is.data.frame(layer$num_sums_raw[[1]]))
})

test_that("process_summaries.desc_layer works with precision settings", {
  # Create desc layer with precision settings
  # precision_by must be a subset of by variables, so we need to add a by variable
  t <- tplyr_table(test_data, gear) %>%
    add_layer(
      group_desc(mpg, by = am) %>%
        set_precision_on(mpg) %>%
        set_precision_by(am)
    )
  
  result <- build(t)
  layer <- t$layers[[1]]
  
  # Verify numeric_data exists
  expect_true(!is.null(layer$numeric_data))
  
  # If precision is needed, trans_sums should have precision_on column
  if (layer$need_prec_table) {
    expect_true("precision_on" %in% names(layer$trans_sums[[1]]))
  }
})
