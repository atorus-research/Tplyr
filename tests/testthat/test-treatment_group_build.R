
# Tests for treatment_group_build() refactoring
# These tests verify the Extract-Process-Bind pattern implementation

# Load test data
load("adsl.Rdata")

test_that("treatment_group_build creates built_target correctly", {
  # Create a simple table
  tab <- tplyr_table(adsl, TRT01A)
  
  # Call treatment_group_build
  treatment_group_build(tab)
  
  # Verify built_target exists
  expect_true(exists("built_target", envir = tab))
  
  # Verify built_target is a data frame
  expect_true(is.data.frame(tab$built_target))
  
  # Verify built_target has same number of rows as target (no filter applied)
  expect_equal(nrow(tab$built_target), nrow(tab$target))
  
  # Verify treatment variable is a factor
  expect_true(is.factor(tab$built_target[[as_name(tab$treat_var)]]))
})

test_that("treatment_group_build creates built_pop_data correctly", {
  # Create a simple table
  tab <- tplyr_table(adsl, TRT01A)
  
  # Call treatment_group_build
  treatment_group_build(tab)
  
  # Verify built_pop_data exists
  expect_true(exists("built_pop_data", envir = tab))
  
  # Verify built_pop_data is a data frame
  expect_true(is.data.frame(tab$built_pop_data))
  
  # Verify built_pop_data has same number of rows as pop_data (no filter applied)
  expect_equal(nrow(tab$built_pop_data), nrow(tab$pop_data))
  
  # Verify population treatment variable is a factor
  expect_true(is.factor(tab$built_pop_data[[as_name(tab$pop_treat_var)]]))
})

test_that("treatment_group_build does not leave temporary variables in table environment", {
  # Create a simple table
  tab <- tplyr_table(adsl, TRT01A)
  
  # Call treatment_group_build
  treatment_group_build(tab)
  
  # Verify temporary variables do NOT exist in table environment
  # Use inherits=FALSE to check only in the table environment, not parent environments
  expect_false(exists("fct_levels", envir = tab, inherits = FALSE))
  expect_false(exists("grp_i", envir = tab, inherits = FALSE))
  expect_false(exists("i", envir = tab, inherits = FALSE))
})

test_that("treatment_group_build handles filter errors correctly", {
  # Create a table with an invalid where condition
  tab <- tplyr_table(adsl, TRT01A, where = nonexistent_column == "value")
  
  # Expect an error with specific message
  expect_error(
    treatment_group_build(tab),
    "tplyr_table `where` condition.*is invalid"
  )
})

test_that("treatment_group_build handles pop_where filter errors correctly", {
  # Create a table with valid target filter but invalid pop filter
  tab <- tplyr_table(adsl, TRT01A)
  tab <- set_pop_where(tab, nonexistent_column == "value")
  
  # Expect an error with specific message about population data
  expect_error(
    treatment_group_build(tab),
    "Population data `pop_where` condition.*is invalid"
  )
})

test_that("treatment_group_build expands treatment groups correctly", {
  # Create a table with treatment groups
  tab <- tplyr_table(adsl, TRT01A) %>%
    add_treat_grps("Xanomeline" = c("Xanomeline High Dose", "Xanomeline Low Dose"))
  
  # Call treatment_group_build
  treatment_group_build(tab)
  
  # Verify the new treatment group exists in built_target
  expect_true("Xanomeline" %in% tab$built_target[[as_name(tab$treat_var)]])
  
  # Verify the new treatment group exists in built_pop_data
  expect_true("Xanomeline" %in% tab$built_pop_data[[as_name(tab$pop_treat_var)]])
  
  # Verify the combined group has correct number of rows
  xan_rows <- tab$built_target[tab$built_target[[as_name(tab$treat_var)]] == "Xanomeline", ]
  xan_high_rows <- adsl[adsl$TRT01A == "Xanomeline High Dose", ]
  xan_low_rows <- adsl[adsl$TRT01A == "Xanomeline Low Dose", ]
  expect_equal(nrow(xan_rows), nrow(xan_high_rows) + nrow(xan_low_rows))
})

test_that("treatment_group_build handles total groups correctly", {
  # Create a table with total group
  tab <- tplyr_table(adsl, TRT01A) %>%
    add_total_group()
  
  # Call treatment_group_build
  treatment_group_build(tab)
  
  # Verify the Total group exists
  expect_true("Total" %in% tab$built_target[[as_name(tab$treat_var)]])
  
  # Verify Total group has all rows
  total_rows <- tab$built_target[tab$built_target[[as_name(tab$treat_var)]] == "Total", ]
  expect_equal(nrow(total_rows), nrow(adsl))
})

test_that("treatment_group_build preserves factor levels", {
  # Create a table
  tab <- tplyr_table(adsl, TRT01A)
  
  # Get original factor levels
  original_levels <- levels(factor(adsl$TRT01A))
  
  # Call treatment_group_build
  treatment_group_build(tab)
  
  # Verify factor levels are preserved
  built_levels <- levels(tab$built_target[[as_name(tab$treat_var)]])
  expect_true(all(original_levels %in% built_levels))
})

test_that("treatment_group_build handles where filters correctly", {
  # Create a table with a where filter
  tab <- tplyr_table(adsl, TRT01A, where = AGE >= 65)
  
  # Call treatment_group_build
  treatment_group_build(tab)
  
  # Verify filter was applied
  expect_true(all(tab$built_target$AGE >= 65))
  
  # Verify row count is reduced
  expect_lt(nrow(tab$built_target), nrow(adsl))
})

test_that("treatment_group_build handles separate pop_where filters correctly", {
  # Create a table with different target and population filters
  tab <- tplyr_table(adsl, TRT01A, where = AGE >= 65) %>%
    set_pop_where(AGE >= 18)
  
  # Call treatment_group_build
  treatment_group_build(tab)
  
  # Verify target filter was applied
  expect_true(all(tab$built_target$AGE >= 65))
  
  # Verify population filter was applied
  expect_true(all(tab$built_pop_data$AGE >= 18))
  
  # Verify different row counts
  expect_lt(nrow(tab$built_target), nrow(tab$built_pop_data))
})

test_that("treatment_group_build converts non-factor treatment variables to factors", {
  # Create a copy of adsl with character treatment variable
  adsl_char <- adsl
  adsl_char$TRT01A <- as.character(adsl_char$TRT01A)
  
  # Create a table
  tab <- tplyr_table(adsl_char, TRT01A)
  
  # Verify original is not a factor
  expect_false(is.factor(tab$target$TRT01A))
  
  # Call treatment_group_build
  treatment_group_build(tab)
  
  # Verify built_target has factor treatment variable
  expect_true(is.factor(tab$built_target[[as_name(tab$treat_var)]]))
})

test_that("treatment_group_build preserves cols factor levels", {
  # Create a table with cols
  tab <- tplyr_table(adsl, TRT01A, cols = vars(SEX))
  
  # Call treatment_group_build
  treatment_group_build(tab)
  
  # Verify cols are preserved in built_target
  expect_true("SEX" %in% names(tab$built_target))
  
  # Verify cols are preserved in built_pop_data
  expect_true("SEX" %in% names(tab$built_pop_data))
})

test_that("treatment_group_build returns table invisibly", {
  # Create a table
  tab <- tplyr_table(adsl, TRT01A)
  
  # Call treatment_group_build and capture result
  result <- treatment_group_build(tab)
  
  # Verify result is the table (returned invisibly)
  expect_identical(result, tab)
})

test_that("treatment_group_build handles empty treatment groups", {
  # Create a table with no treatment groups
  tab <- tplyr_table(adsl, TRT01A)
  
  # Verify treat_grps is empty
  expect_equal(length(tab$treat_grps), 0)
  
  # Call treatment_group_build (should not error)
  expect_silent(treatment_group_build(tab))
  
  # Verify built_target exists
  expect_true(exists("built_target", envir = tab))
})

test_that("treatment_group_build handles multiple treatment groups", {
  # Create a table with multiple treatment groups
  tab <- tplyr_table(adsl, TRT01A) %>%
    add_treat_grps(
      "Xanomeline" = c("Xanomeline High Dose", "Xanomeline Low Dose"),
      "Active" = c("Xanomeline High Dose", "Xanomeline Low Dose", "Placebo")
    )
  
  # Call treatment_group_build
  treatment_group_build(tab)
  
  # Verify both treatment groups exist
  expect_true("Xanomeline" %in% tab$built_target[[as_name(tab$treat_var)]])
  expect_true("Active" %in% tab$built_target[[as_name(tab$treat_var)]])
})

test_that("treatment_group_build maintains data integrity", {
  # Create a table
  tab <- tplyr_table(adsl, TRT01A)
  
  # Get original column names
  original_cols <- names(adsl)
  
  # Call treatment_group_build
  treatment_group_build(tab)
  
  # Verify all original columns are preserved
  expect_true(all(original_cols %in% names(tab$built_target)))
  expect_true(all(original_cols %in% names(tab$built_pop_data)))
})
