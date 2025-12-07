# Tests for refactored process_metadata.desc_layer()

load(test_path('adsl.Rdata'))
load(test_path('adlb.Rdata'))

test_that("process_metadata.desc_layer() produces correct metadata structure", {
  # Setup
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_desc(AGE)
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Check that metadata exists
  expect_true(!is.null(t_test$metadata))
  expect_true(inherits(t_test$metadata, "data.frame"))
  
  # Check that metadata has required columns
  expect_true("row_id" %in% names(t_test$metadata))
  expect_true(any(grepl("^var1_", names(t_test$metadata))))
  
  # Check that metadata contains tplyr_meta objects
  meta_cols <- names(t_test$metadata)[grepl("^var1_", names(t_test$metadata))]
  expect_true(length(meta_cols) > 0)
  
  # Check first metadata object
  first_meta <- t_test$metadata[[meta_cols[1]]][[1]]
  expect_true(inherits(first_meta, "tplyr_meta"))
  expect_true(!is.null(first_meta$names))
  expect_true(!is.null(first_meta$filters))
})

test_that("process_metadata.desc_layer() includes complete traceability information", {
  # Setup with more complex table
  t_test <- tplyr_table(adsl, TRT01A, where = SAFFL == "Y") %>%
    add_layer(
      group_desc(AGE, by = SEX)
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Get a specific metadata object
  meta_cols <- names(t_test$metadata)[grepl("^var1_", names(t_test$metadata))]
  first_meta <- t_test$metadata[[meta_cols[1]]][[1]]
  
  # Check that metadata includes treatment variable
  expect_true(any(sapply(first_meta$names, function(x) as_label(x) == "TRT01A")))
  
  # Check that metadata includes by variable
  expect_true(any(sapply(first_meta$names, function(x) as_label(x) == "SEX")))
  
  # Check that metadata includes target variable (AGE)
  expect_true(any(sapply(first_meta$names, function(x) as_label(x) == "AGE")))
  
  # Check that metadata includes table where filter
  expect_true(any(sapply(first_meta$filters, function(x) grepl("SAFFL", as_label(x)))))
})

test_that("process_metadata.desc_layer() creates formatted_meta in layer environment", {
  # Setup
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_desc(AGE)
    )
  
  # Get the layer
  layer <- t_test$layers[[1]]
  
  # Build to trigger processing
  result <- build(t_test, metadata = TRUE)
  
  # Check that formatted_meta IS in the environment
  expect_true(env_has(layer, "formatted_meta"))
  expect_true(inherits(layer$formatted_meta, "data.frame"))
  
  # Check that formatted_meta has the expected structure
  expect_true("row_id" %in% names(layer$formatted_meta))
  expect_true(any(grepl("^var1_", names(layer$formatted_meta))))
})

test_that("process_metadata.desc_layer() does not leave temporary variables in layer environment", {
  # Setup
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_desc(AGE)
    )
  
  # Get the layer
  layer <- t_test$layers[[1]]
  
  # Build to trigger processing
  result <- build(t_test, metadata = TRUE)
  
  # Check that temporary variables are NOT in the environment
  expect_false(env_has(layer, "meta_sums"))
  expect_false(env_has(layer, "form_meta"))
  expect_false(env_has(layer, "i"))
  expect_false(env_has(layer, "cur_var"))
  expect_false(env_has(layer, "meta_sum"))
})

test_that("process_metadata.desc_layer() handles multiple target variables", {
  # Setup with multiple target variables
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_desc(vars(AGE, HEIGHTBL, WEIGHTBL))
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Check that metadata exists
  expect_true(!is.null(t_test$metadata))
  
  # Check that metadata has columns for all three variables
  expect_true(any(grepl("^var1_", names(t_test$metadata))))
  expect_true(any(grepl("^var2_", names(t_test$metadata))))
  expect_true(any(grepl("^var3_", names(t_test$metadata))))
})

test_that("process_metadata.desc_layer() handles stats_as_columns", {
  # Setup with stats_as_columns
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_desc(AGE) %>%
        set_stats_as_columns()
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Check that metadata exists
  expect_true(!is.null(t_test$metadata))
  
  # Check that metadata has row_id column
  expect_true("row_id" %in% names(t_test$metadata))
  
  # Check that row_ids start with 'd' for desc layer
  expect_true(all(grepl("^d", t_test$metadata$row_id)))
})

test_that("process_metadata.desc_layer() handles column grouping", {
  # Setup with cols parameter
  t_test <- tplyr_table(adsl, TRT01A, cols = SEX) %>%
    add_layer(
      group_desc(AGE)
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Check that metadata exists
  expect_true(!is.null(t_test$metadata))
  
  # Check that metadata columns include column grouping
  meta_cols <- names(t_test$metadata)[grepl("^var1_", names(t_test$metadata))]
  # Should have columns for each treatment x sex combination
  expect_true(length(meta_cols) > 3)  # More than just treatment groups
})

test_that("process_metadata.desc_layer() handles layer where filters", {
  # Setup with layer where filter
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_desc(AGE, where = SEX == "F")
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Get a specific metadata object
  meta_cols <- names(t_test$metadata)[grepl("^var1_", names(t_test$metadata))]
  first_meta <- t_test$metadata[[meta_cols[1]]][[1]]
  
  # Check that metadata includes layer where filter
  expect_true(any(sapply(first_meta$filters, function(x) grepl("SEX", as_label(x)))))
})

test_that("process_metadata.desc_layer() formatted_meta has correct row_id prefix", {
  # Setup
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_desc(AGE)
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Check that all row_ids start with 'd' for desc layer
  expect_true(all(grepl("^d\\d+_\\d+$", t_test$metadata$row_id)))
})

test_that("process_metadata.desc_layer() handles custom summaries", {
  # Setup with custom summaries
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_desc(AGE) %>%
        set_custom_summaries(
          geometric_mean = exp(mean(log(.var)))
        )
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Check that metadata exists
  expect_true(!is.null(t_test$metadata))
  
  # Check that metadata has row_id column
  expect_true("row_id" %in% names(t_test$metadata))
})

test_that("process_metadata.desc_layer() handles precision data", {
  # Setup with precision data
  # Note: precision_by must be a subset of by variables
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_desc(AGE, by = SEX) %>%
        set_precision_on(AGE) %>%
        set_precision_by(SEX)
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Check that metadata exists
  expect_true(!is.null(t_test$metadata))
  
  # Metadata should still be created correctly even with precision settings
  meta_cols <- names(t_test$metadata)[grepl("^var1_", names(t_test$metadata))]
  expect_true(length(meta_cols) > 0)
})

test_that("process_metadata.desc_layer() handles multiple by variables", {
  # Setup with multiple by variables
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_desc(AGE, by = vars(SEX, RACE))
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Check that metadata exists
  expect_true(!is.null(t_test$metadata))
  
  # Check that metadata has required columns
  expect_true("row_id" %in% names(t_test$metadata))
  expect_true(any(grepl("^var1_", names(t_test$metadata))))
  
  # Check that result has proper structure with multiple by variables
  expect_true(nrow(result) > 0)
})
