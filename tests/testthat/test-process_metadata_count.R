# Tests for refactored process_metadata.count_layer()

load(test_path('adsl.Rdata'))

test_that("process_metadata.count_layer() produces correct metadata structure", {
  # Setup
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_count(RACE)
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

test_that("process_metadata.count_layer() includes complete traceability information", {
  # Setup with more complex table
  t_test <- tplyr_table(adsl, TRT01A, where = SAFFL == "Y") %>%
    add_layer(
      group_count(RACE, by = SEX)
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
  
  # Check that metadata includes target variable
  expect_true(any(sapply(first_meta$names, function(x) as_label(x) == "RACE")))
  
  # Check that metadata includes table where filter
  expect_true(any(sapply(first_meta$filters, function(x) grepl("SAFFL", as_label(x)))))
})

test_that("process_metadata.count_layer() creates formatted_meta in layer environment", {
  # Setup
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_count(RACE)
    )
  
  # Get the layer
  layer <- t_test$layers[[1]]
  
  # Build to trigger processing
  result <- build(t_test, metadata = TRUE)
  
  # Note: process_metadata.count_layer() cannot be fully refactored to Extract-Process-Bind
  # because build_count_meta() uses match.call() for metaprogramming and requires evalq().
  # However, we can verify that the intended result is created.
  
  # Check that the intended result IS in the environment
  expect_true(env_has(layer, "formatted_meta"))
  expect_true(inherits(layer$formatted_meta, "data.frame"))
  
  # Check that formatted_meta has the expected structure
  expect_true("row_id" %in% names(layer$formatted_meta))
  expect_true(any(grepl("^var1_", names(layer$formatted_meta))))
})

test_that("process_metadata.count_layer() handles nested counts", {
  # Setup with nested target variables
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_count(vars(RACE, ETHNIC))
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Check that metadata exists
  expect_true(!is.null(t_test$metadata))
  
  # Check that metadata has row_id column
  expect_true("row_id" %in% names(t_test$metadata))
  
  # Check that row_ids start with 'c' for count layer
  expect_true(all(grepl("^c", t_test$metadata$row_id)))
})

test_that("process_metadata.count_layer() handles total rows", {
  # Setup with total row
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_count(RACE) %>%
        add_total_row()
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Check that metadata exists
  expect_true(!is.null(t_test$metadata))
  
  # Check that there's a row for the total
  expect_true(any(grepl("Total", result$row_label1, ignore.case = TRUE)))
})

test_that("process_metadata.count_layer() handles missing counts", {
  # Setup with missing values
  adsl_test <- adsl
  adsl_test$RACE[1:5] <- NA
  
  t_test <- tplyr_table(adsl_test, TRT01A) %>%
    add_layer(
      group_count(RACE) %>%
        set_missing_count(f_str("xx", n), Missing = NA)
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Check that metadata exists
  expect_true(!is.null(t_test$metadata))
  
  # Check that there's a row for missing
  expect_true(any(grepl("Missing", result$row_label1, ignore.case = TRUE)))
})

test_that("process_metadata.count_layer() handles column grouping", {
  # Setup with cols parameter
  t_test <- tplyr_table(adsl, TRT01A, cols = SEX) %>%
    add_layer(
      group_count(RACE)
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

test_that("process_metadata.count_layer() handles distinct counts", {
  # Setup with distinct_by
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_count(RACE) %>%
        set_distinct_by(USUBJID)
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Check that metadata exists
  expect_true(!is.null(t_test$metadata))
  
  # Metadata should still be created correctly even with distinct counts
  meta_cols <- names(t_test$metadata)[grepl("^var1_", names(t_test$metadata))]
  expect_true(length(meta_cols) > 0)
})

test_that("process_metadata.count_layer() handles layer where filters", {
  # Setup with layer where filter
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_count(RACE, where = AGE > 50)
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Get a specific metadata object
  meta_cols <- names(t_test$metadata)[grepl("^var1_", names(t_test$metadata))]
  first_meta <- t_test$metadata[[meta_cols[1]]][[1]]
  
  # Check that metadata includes layer where filter
  expect_true(any(sapply(first_meta$filters, function(x) grepl("AGE", as_label(x)))))
})

test_that("process_metadata.count_layer() formatted_meta has correct row_id prefix", {
  # Setup
  t_test <- tplyr_table(adsl, TRT01A) %>%
    add_layer(
      group_count(RACE)
    )
  
  # Build with metadata
  result <- build(t_test, metadata = TRUE)
  
  # Check that all row_ids start with 'c' for count layer
  expect_true(all(grepl("^c\\d+_\\d+$", t_test$metadata$row_id)))
})
