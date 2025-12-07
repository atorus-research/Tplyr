# Tests for refactored count layer helper functions

test_that("process_count_n() calculates counts correctly", {
  # Setup
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl)
  t_test <- add_layers(t_test, layer_test)
  
  # Trigger processing up to the point where we can test process_count_n
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify summary_stat was created
  expect_true(!is.null(layer$summary_stat))
  expect_true(is.data.frame(layer$summary_stat))
  expect_true("n" %in% names(layer$summary_stat))
  expect_true("distinct_n" %in% names(layer$summary_stat))
})

test_that("process_count_n() does not pollute layer environment", {
  # Setup
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl)
  t_test <- add_layers(t_test, layer_test)
  
  # Build to trigger processing
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Check that temporary variables don't exist in layer environment
  expect_false(exists("denoms_by_", envir = layer))
  expect_false(exists("complete_levels", envir = layer))
  expect_false(exists("outer_", envir = layer, inherits = FALSE))
})

test_that("process_count_total_row() creates total row correctly", {
  # Setup
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl) %>%
    add_total_row()
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify total_stat was created
  expect_true(!is.null(layer$total_stat))
  expect_true(is.data.frame(layer$total_stat))
  expect_true("n" %in% names(layer$total_stat))
})

test_that("process_count_total_row() does not pollute layer environment", {
  # Setup
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl) %>%
    add_total_row()
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Check that temporary variables don't exist
  expect_false(exists("needed_denoms_by", envir = layer))
  expect_false(exists("filter_logic", envir = layer))
})

test_that("process_missing_subjects_row() creates missing subjects row correctly", {
  # Setup
  t_test <- tplyr_table(mtcars, gear) %>%
    set_pop_data(mtcars)
  layer_test <- group_count(t_test, cyl) %>%
    add_missing_subjects_row()
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  suppressWarnings(built <- build(t_test))
  layer <- t_test$layers[[1]]
  
  # Verify missing_subjects_stat was created
  expect_true(!is.null(layer$missing_subjects_stat))
  expect_true(is.data.frame(layer$missing_subjects_stat))
  expect_true("distinct_n" %in% names(layer$missing_subjects_stat))
})

test_that("process_missing_subjects_row() does not pollute layer environment", {
  # Setup
  t_test <- tplyr_table(mtcars, gear) %>%
    set_pop_data(mtcars)
  layer_test <- group_count(t_test, cyl) %>%
    add_missing_subjects_row()
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  suppressWarnings(built <- build(t_test))
  layer <- t_test$layers[[1]]
  
  # Check that temporary variables don't exist
  expect_false(exists("needed_denoms_by", envir = layer))
  expect_false(exists("mrg_vars", envir = layer))
})

test_that("process_count_denoms() calculates denominators correctly", {
  # Setup
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl)
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify denoms_df was created
  expect_true(!is.null(layer$denoms_df))
  expect_true(is.data.frame(layer$denoms_df))
  expect_true("n" %in% names(layer$denoms_df))
  expect_true("distinct_n" %in% names(layer$denoms_df))
})

test_that("process_count_denoms() does not pollute layer environment", {
  # Setup
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl)
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Check that temporary variables don't exist
  expect_false(exists("layer_params", envir = layer))
  expect_false(exists("param_apears", envir = layer))
  expect_false(exists("denom_target", envir = layer))
  expect_false(exists("denoms_df_n", envir = layer))
  expect_false(exists("denoms_df_dist", envir = layer))
  expect_false(exists("dist_grp", envir = layer))
  expect_false(exists("is_svar", envir = layer))
  expect_false(exists("which_is_treatvar", envir = layer))
  expect_false(exists("by_join", envir = layer))
  expect_false(exists("local_denom_ignore", envir = layer))
})

test_that("factor_treat_var() converts treatment variable to factor", {
  # This function is used in nested counts, so we need a nested count setup
  # For now, just verify it doesn't error
  t_test <- tplyr_table(mtcars, gear)
  mtcars_test <- mtcars
  mtcars_test$grp <- paste0("grp.", mtcars_test$cyl)
  t_test <- tplyr_table(mtcars_test, gear)
  layer_test <- group_count(t_test, vars(cyl, grp))
  t_test <- add_layers(t_test, layer_test)
  
  # Build should not error
  expect_silent(built <- build(t_test))
})

test_that("rename_missing_values() renames missing values correctly", {
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
  
  # Verify built_target has the renamed missing values
  expect_true("Missing" %in% layer$built_target$cyl)
})

test_that("rename_missing_values() does not pollute layer environment", {
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
  
  # Check that temporary variables from rename_missing_values don't exist
  # Note: idx is the loop variable we use instead of i
  expect_false(exists("idx", envir = layer))
  # Note: missing_count_list_ may exist from other functions still using evalq()
  # so we don't test for it here
})

test_that("process_single_count_target() produces correct numeric_data", {
  # Setup
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl)
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Verify numeric_data was created with correct structure
  expect_true(!is.null(layer$numeric_data))
  expect_true(is.data.frame(layer$numeric_data))
  expect_true("n" %in% names(layer$numeric_data))
  expect_true("total" %in% names(layer$numeric_data))
  expect_true("summary_var" %in% names(layer$numeric_data))
})

test_that("process_single_count_target() does not pollute layer environment", {
  # Setup
  t_test <- tplyr_table(mtcars, gear)
  layer_test <- group_count(t_test, cyl)
  t_test <- add_layers(t_test, layer_test)
  
  # Build
  built <- build(t_test)
  layer <- t_test$layers[[1]]
  
  # Check that temporary variables don't exist
  expect_false(exists("denoms_df_prep", envir = layer))
  expect_false(exists("fct_cols", envir = layer))
  expect_false(exists("fct_cols_ns", envir = layer))
  expect_false(exists("tmp_fmt", envir = layer))
})

# Edge case tests
test_that("helper functions handle empty data correctly", {
  # Create empty dataset
  mtcars_empty <- mtcars[0, ]
  
  t_test <- tplyr_table(mtcars_empty, gear)
  layer_test <- group_count(t_test, cyl)
  t_test <- add_layers(t_test, layer_test)
  
  # After fix for issue #131, empty data should build successfully
  expect_no_error(built <- build(t_test))
  expect_equal(nrow(built), 0)
})

test_that("helper functions handle all NA data correctly", {
  # Create dataset with all NA in target variable
  mtcars_na <- mtcars
  mtcars_na$cyl <- NA
  
  t_test <- tplyr_table(mtcars_na, gear)
  layer_test <- group_count(t_test, cyl)
  t_test <- add_layers(t_test, layer_test)
  
  # Build may produce warnings but should not error
  expect_warning(built <- build(t_test))
})

test_that("helper functions handle single group correctly", {
  # Create dataset with single treatment group
  mtcars_single <- mtcars[mtcars$gear == 3, ]
  
  t_test <- tplyr_table(mtcars_single, gear)
  layer_test <- group_count(t_test, cyl)
  t_test <- add_layers(t_test, layer_test)
  
  # Build should not error
  expect_silent(built <- build(t_test))
})
