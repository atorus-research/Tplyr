library(dplyr)

# Load test data
load(test_path('adsl.Rdata'))

test_that("process_nested_count_target() creates correct nested structure", {
  # Setup nested count layer
  mtcars_test <- mtcars
  mtcars_test$grp <- paste0("grp.", mtcars_test$cyl)
  
  t_test <- tplyr_table(mtcars_test, gear)
  layer_test <- group_count(t_test, vars(cyl, grp))
  t_test <- add_layers(t_test, layer_test)
  
  # Build the table's target data first (required for process_summaries)
  treatment_group_build(t_test)
  
  # Process summaries (which calls process_nested_count_target)
  # This tests the core nested count logic without going through formatting/sorting
  layer_test <- process_summaries(layer_test)
  
  # Verify nested structure is created
  expect_true(!is.null(layer_test$numeric_data))
  expect_true(nrow(layer_test$numeric_data) > 0)
  
  # Verify both outer and inner variables are present in the data
  expect_true("cyl" %in% names(layer_test$numeric_data))
  expect_true("summary_var" %in% names(layer_test$numeric_data))
  
  # Verify by_saved and target_var_saved are set for rebuild capability
  expect_true(!is.null(layer_test$by_saved))
  expect_true(!is.null(layer_test$target_var_saved))
  expect_true(layer_test$is_built_nest)
})

test_that("process_nested_count_target() handles indentation correctly", {
  mtcars_test <- mtcars
  mtcars_test$grp <- paste0("grp.", mtcars_test$cyl)
  
  # Test with default indentation
  t_test1 <- tplyr_table(mtcars_test, gear)
  layer_test1 <- group_count(t_test1, vars(cyl, grp))
  t_test1 <- add_layers(t_test1, layer_test1)
  treatment_group_build(t_test1)
  layer_test1 <- process_summaries(layer_test1)
  
  # Check that indentation is set to default
  expect_equal(layer_test1$indentation, "   ")
  
  # Check that inner layer values have indentation in summary_var
  inner_rows <- layer_test1$numeric_data %>% 
    filter(!is.na(cyl) & grepl("^   ", summary_var))
  expect_true(nrow(inner_rows) > 0)
  
  # Test with custom indentation
  t_test2 <- tplyr_table(mtcars_test, gear)
  layer_test2 <- group_count(t_test2, vars(cyl, grp)) %>%
    set_count_row_prefix("  ")
  t_test2 <- add_layers(t_test2, layer_test2)
  treatment_group_build(t_test2)
  layer_test2 <- process_summaries(layer_test2)
  
  # Check that custom indentation is set
  expect_equal(layer_test2$indentation, "  ")
  
  # Check that inner layer values have custom indentation
  inner_rows2 <- layer_test2$numeric_data %>% 
    filter(!is.na(cyl) & grepl("^  ", summary_var))
  expect_true(nrow(inner_rows2) > 0)
})

test_that("process_nested_count_target() does not pollute layer environment", {
  mtcars_test <- mtcars
  mtcars_test$grp <- paste0("grp.", mtcars_test$cyl)
  
  t_test <- tplyr_table(mtcars_test, gear)
  layer_test <- group_count(t_test, vars(cyl, grp))
  t_test <- add_layers(t_test, layer_test)
  
  # Build the table's target data first
  treatment_group_build(t_test)
  
  # Process summaries (which calls process_nested_count_target)
  layer_test <- process_summaries(layer_test)
  
  # Verify no temporary variables remain in layer environment
  # These are variables that were used during processing but should not persist
  expect_false(exists("change_denom_ind", envir = layer_test))
  expect_false(exists("second_denoms_by", envir = layer_test))
  expect_false(exists("fl", envir = layer_test))
  expect_false(exists("first_layer", envir = layer_test))
  expect_false(exists("second_layer", envir = layer_test))
  expect_false(exists("first_layer_final", envir = layer_test))
  expect_false(exists("second_layer_final", envir = layer_test))
  expect_false(exists("ignored_filter_rows", envir = layer_test))
  expect_false(exists("by_new", envir = layer_test))
  expect_false(exists("target_var_new", envir = layer_test))
  
  # Verify expected bindings DO exist
  expect_true(exists("numeric_data", envir = layer_test))
  expect_true(exists("by_saved", envir = layer_test))
  expect_true(exists("target_var_saved", envir = layer_test))
  expect_true(exists("is_built_nest", envir = layer_test))
  expect_true(exists("by", envir = layer_test))
  expect_true(exists("target_var", envir = layer_test))
})

test_that("process_nested_count_target() handles where conditions", {
  mtcars_test <- mtcars
  mtcars_test$grp <- paste0("grp.", mtcars_test$cyl)
  
  t_test <- tplyr_table(mtcars_test, gear)
  layer_test <- group_count(t_test, vars(cyl, grp)) %>%
    set_where(am == 1)
  t_test <- add_layers(t_test, layer_test)
  
  # Build the table's target data first
  treatment_group_build(t_test)
  
  # Process summaries
  layer_test <- process_summaries(layer_test)
  
  # Verify the layer was processed successfully
  expect_true(!is.null(layer_test$numeric_data))
  expect_true(nrow(layer_test$numeric_data) > 0)
  
  # The where condition should filter the data
  # Verify the structure is correct
  expect_true("summary_var" %in% names(layer_test$numeric_data))
  expect_true("cyl" %in% names(layer_test$numeric_data))
})

test_that("process_nested_count_target() errors on total rows", {
  mtcars_test <- mtcars
  mtcars_test$grp <- paste0("grp.", mtcars_test$cyl)
  
  expect_error(
    tplyr_table(mtcars_test, gear) %>%
      add_layer(
        group_count(vars(cyl, grp)) %>%
          add_total_row()
      ) %>%
      build(),
    "You can't include total rows in nested counts"
  )
})

test_that("process_nested_count_target() handles denoms_by correctly", {
  mtcars_test <- mtcars
  mtcars_test$grp <- paste0("grp.", mtcars_test$cyl)
  
  # Test with custom denoms_by
  t_test <- tplyr_table(mtcars_test, gear)
  layer_test <- group_count(t_test, vars(cyl, grp)) %>%
    set_denoms_by(gear)
  t_test <- add_layers(t_test, layer_test)
  
  # Build the table's target data first
  treatment_group_build(t_test)
  
  # Process summaries
  layer_test <- process_summaries(layer_test)
  
  # Verify the layer was processed successfully
  expect_true(!is.null(layer_test$numeric_data))
  expect_true(nrow(layer_test$numeric_data) > 0)
  expect_true("summary_var" %in% names(layer_test$numeric_data))
})

test_that("process_nested_count_target() can be rebuilt", {
  mtcars_test <- mtcars
  mtcars_test$grp <- paste0("grp.", mtcars_test$cyl)
  
  t_test <- tplyr_table(mtcars_test, gear)
  layer_test <- group_count(t_test, vars(cyl, grp))
  t_test <- add_layers(t_test, layer_test)
  
  # Build the table's target data first
  treatment_group_build(t_test)
  
  # Process summaries twice
  result1 <- process_summaries(layer_test)
  result2 <- process_summaries(layer_test)
  
  # Results should be identical
  expect_equal(result1$numeric_data, result2$numeric_data)
  expect_equal(result1$by_saved, result2$by_saved)
  expect_equal(result1$target_var_saved, result2$target_var_saved)
})

test_that("process_nested_count_target() handles factor warnings", {
  mtcars_test <- mtcars
  mtcars_test$cyl <- factor(mtcars_test$cyl)
  mtcars_test$grp <- paste0("grp.", mtcars_test$cyl)
  
  t_test <- tplyr_table(mtcars_test, gear)
  layer_test <- group_count(t_test, vars(cyl, grp))
  t_test <- add_layers(t_test, layer_test)
  
  # Build the table's target data first
  treatment_group_build(t_test)
  
  # Should warn about factors
  expect_warning(
    process_summaries(layer_test),
    "Factors are not currently supported in nested count layers"
  )
})

test_that("process_nested_count_target() warns when inner variable is a factor", {
  mtcars_test <- mtcars
  mtcars_test$grp <- factor(paste0("grp.", mtcars_test$cyl))

  t_test <- tplyr_table(mtcars_test, gear)
  layer_test <- group_count(t_test, vars(cyl, grp))
  t_test <- add_layers(t_test, layer_test)

  treatment_group_build(t_test)

  expect_warning(
    process_summaries(layer_test),
    "Factors are not currently supported in nested count layers"
  )
})

test_that("process_nested_count_target() validates inner variable", {
  mtcars_test <- mtcars

  # Should error when inner variable is not a symbol
  expect_error(
    tplyr_table(mtcars_test, gear) %>%
      add_layer(group_count(vars(cyl, "text"))) %>%
      build(),
    "Inner layers must be data driven variables"
  )
})

test_that("Nested count with numeric_threshold and column parameter works", {
  load(test_path("adae.Rdata"))

  x <- adae %>%
    tplyr_table(TRTA) %>%
    add_layer(
      group_count(vars(AEBODSYS, AEDECOD)) %>%
        set_numeric_threshold(3, "n", "Placebo") %>%
        set_order_count_method("bycount")
    ) %>%
    build()

  expect_true(is.data.frame(x))
  expect_true(nrow(x) > 0)
  expect_true("ord_layer_1" %in% names(x))
})
