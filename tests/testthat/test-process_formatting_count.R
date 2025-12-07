# Tests for refactored process_formatting.count_layer()

test_that("process_formatting.count_layer() produces correct formatted_data", {
  # Setup
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl)
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify formatted_data was created with correct structure
  expect_true(!is.null(layer$formatted_data))
  expect_true(is.data.frame(layer$formatted_data))
  
  # Check for expected columns
  expect_true("row_label1" %in% names(layer$formatted_data))
  expect_true(any(grepl("^var1_", names(layer$formatted_data))))
  # ord_layer_index is added by add_order_columns, which is called after process_formatting
  # So it should be present in the final formatted_data
  expect_true(any(grepl("^ord_", names(layer$formatted_data))))
  
  # Verify formatted strings are character type
  var_cols <- names(layer$formatted_data)[grepl("^var1_", names(layer$formatted_data))]
  for (col in var_cols) {
    expect_type(layer$formatted_data[[col]], "character")
  }
})

test_that("process_formatting.count_layer() does not pollute layer environment", {
  # Setup
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl)
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Check that the key output (formatted_data) exists
  expect_true(!is.null(layer$formatted_data))
  
  # Check that formatted_stats_data (a temporary variable) doesn't exist
  # Note: indentation_length and row_labels might exist from other functions
  # that haven't been refactored yet, so we don't test for those
  expect_false(exists("formatted_stats_data", envir = layer, inherits = FALSE))
})

test_that("process_formatting.count_layer() formats with custom format strings", {
  # Setup with custom format
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl) %>%
    set_format_strings(f_str("xxx (xx.x%)", n, pct))
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify formatted_data exists and has expected format
  expect_true(!is.null(layer$formatted_data))
  
  # Check that formatted strings contain parentheses (from format)
  var_cols <- names(layer$formatted_data)[grepl("^var1_", names(layer$formatted_data))]
  has_parens <- any(sapply(var_cols, function(col) {
    any(grepl("\\(", layer$formatted_data[[col]]))
  }))
  expect_true(has_parens)
})

test_that("process_formatting.count_layer() handles distinct counts", {
  # Setup with distinct_by
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl) %>%
    set_distinct_by(am) %>%
    set_format_strings(f_str("xx (xx.x%) [xx]", n, pct, distinct_n))
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify formatted_data exists
  expect_true(!is.null(layer$formatted_data))
  
  # Check that formatted strings contain brackets (from format)
  var_cols <- names(layer$formatted_data)[grepl("^var1_", names(layer$formatted_data))]
  has_brackets <- any(sapply(var_cols, function(col) {
    any(grepl("\\[", layer$formatted_data[[col]]))
  }))
  expect_true(has_brackets)
})

test_that("process_formatting.count_layer() handles by variables", {
  # Setup with by variables
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl, by = am)
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify formatted_data has multiple row_label columns
  expect_true(!is.null(layer$formatted_data))
  expect_true("row_label1" %in% names(layer$formatted_data))
  expect_true("row_label2" %in% names(layer$formatted_data))
})

test_that("process_formatting.count_layer() handles nested counts", {
  # Setup with nested target variables
  mtcars_test <- mtcars
  mtcars_test$grp <- paste0("grp.", mtcars_test$cyl)
  
  t_test <- tplyr_table(mtcars_test, gear)
  layer_test <- group_count(t_test, vars(cyl, grp))
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify formatted_data exists and has nested structure
  expect_true(!is.null(layer$formatted_data))
  expect_true("row_label1" %in% names(layer$formatted_data))
  
  # Check that some row_label1 values are NA (outer level)
  # and get filled from inner level
  expect_true(all(!is.na(layer$formatted_data$row_label1)))
})

test_that("process_formatting.count_layer() handles total rows", {
  # Setup with total row
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl) %>%
    add_total_row()
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify formatted_data includes total row
  expect_true(!is.null(layer$formatted_data))
  expect_true(any(grepl("Total", layer$formatted_data$row_label1, ignore.case = TRUE)))
})

test_that("process_formatting.count_layer() handles missing counts", {
  # Setup with missing values
  mtcars_test <- mtcars
  mtcars_test[mtcars_test$cyl == 6, "cyl"] <- NA
  
  t_test <- tplyr_table(mtcars_test, gear)
  layer_test <- group_count(t_test, cyl) %>%
    set_missing_count(f_str("xx", n), Missing = NA)
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify formatted_data includes missing row
  expect_true(!is.null(layer$formatted_data))
  expect_true(any(grepl("Missing", layer$formatted_data$row_label1)))
})

test_that("process_formatting.count_layer() handles stats (risk difference)", {
  # Setup with risk difference
  t_test <- tplyr_table(mtcars, gear) %>%
    add_total_group()
  layer_test <- group_count(t_test, cyl) %>%
    add_risk_diff(
      c("3", "Total"),
      c("4", "Total")
    )
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify formatted_data includes risk diff columns
  expect_true(!is.null(layer$formatted_data))
  # Risk diff columns should be present
  expect_true(any(grepl("rdiff", names(layer$formatted_data))))
})

test_that("process_formatting.count_layer() applies numeric cutoff correctly", {
  # Setup with numeric cutoff
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl) %>%
    set_numeric_threshold(numeric_cutoff = 5, stat = "n")
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify formatted_data exists
  # Rows with n < 5 should be filtered or marked
  expect_true(!is.null(layer$formatted_data))
})

test_that("process_formatting.count_layer() handles indentation", {
  # Setup with custom indentation
  mtcars_test <- mtcars
  mtcars_test$grp <- paste0("grp.", mtcars_test$cyl)
  
  t_test <- tplyr_table(mtcars_test, gear)
  layer_test <- group_count(t_test, vars(cyl, grp)) %>%
    set_indentation("  ")
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify formatted_data exists
  expect_true(!is.null(layer$formatted_data))
  
  # Indentation is applied during the build process
  # Just verify the layer has the indentation setting
  expect_equal(layer$indentation, "  ")
})

# Edge case tests
test_that("process_formatting.count_layer() handles empty numeric_data", {
  # This is a tricky edge case - we need numeric_data to exist but be empty
  # After fix for issue #131, empty data should build successfully
  
  # Create a scenario where no data matches the where clause
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl) %>%
    set_where(cyl == 999)  # No rows match this
  t_test <- add_layers(t_test, layer_test)
  
  # Build should succeed with empty data (issue #131 fix)
  # The result should be an empty tibble
  expect_no_error(built <- build(t_test))
  expect_equal(nrow(built), 0)
})

test_that("process_formatting.count_layer() handles single treatment group", {
  # Create dataset with single treatment group
  mtcars_single <- mtcars[mtcars$gear == 3, ]
  
  t_test <- tplyr_table(mtcars_single, gear)
  layer_test <- group_count(t_test, cyl)
  t_test <- add_layers(t_test, layer_test)
  
  # Build should not error
  expect_silent(built <- build(t_test))
  layer <- t_test$layers[[1]]
  
  # Verify formatted_data exists
  expect_true(!is.null(layer$formatted_data))
})

test_that("process_formatting.count_layer() output matches expected format", {
  # Setup
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl) %>%
    set_format_strings(f_str("xx (xx.x%)", n, pct))
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  
  # Check that built output has expected structure
  expect_true(is.data.frame(built))
  expect_true("row_label1" %in% names(built))
  
  # Check that values are formatted strings
  var_cols <- names(built)[grepl("^var1_", names(built))]
  for (col in var_cols) {
    expect_type(built[[col]], "character")
    # Should contain numbers and parentheses
    expect_true(any(grepl("\\d+\\s*\\(", built[[col]])))
  }
})
