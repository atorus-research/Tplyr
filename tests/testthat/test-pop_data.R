##### add_total_group #####

test_that("add_treat_grps errors function properly", {

  t <- tplyr_table(mtcars, gear)

  # Must be named
  expect_snapshot_error(add_treat_grps(t, c("blah", "bloh")))

  # Must attach to tplyr_table
  l <- group_count(t, am)
  expect_snapshot_error(add_treat_grps(l, "one" = c(1,2,3)))
})

test_that("add_total_group errors function properly", {
  t <- tplyr_table(mtcars, gear)
  expect_snapshot_error(add_total_group(t, 1))
})

test_that("add_total_group adds treat_grps bindings properly", {
  tab <- tplyr_table(iris, Species)

  expect_equal(treat_grps(tab), list())
  add_total_group(tab)
  expect_equal(treat_grps(tab), list(Total = c("setosa", "versicolor", "virginica")))

})

# Multiple calls continually append
test_that("add_treat_grps and add_total_grps properly append existing groups", {
  t <- tplyr_table(mtcars, gear) %>%
    add_treat_grps(a = c("3", "4"), b = c("1", "2")) %>%
    add_total_group()

  expect_equal(treat_grps(t), list(a = c("3", "4"), b=c("1", "2"), Total=c("4", "3", "5")))

})

test_that("default header_n is built properly", {
  t <- tplyr_table(mtcars, gear) %>%
    add_total_group() %>%
    set_distinct_by(cyl) %>%
    add_layer(
      group_count(vs)
    )
  t_b <- build(t)

  expect_equal(header_n(t), tibble(gear = factor(c(3, 4, 5, "Total")), n = c(3, 2, 3, 3)))
})

##### build_header_n #####

# Tests for build_header_n() refactoring
# These tests verify the Extract-Process-Bind pattern implementation

# Load test data
load("adsl.Rdata")

test_that("build_header_n creates header_n correctly with population data", {
  # Create a simple table
  tab <- tplyr_table(adsl, TRT01A)
  
  # Build treatment groups first (prerequisite)
  treatment_group_build(tab)
  
  # Call build_header_n
  build_header_n(tab)
  
  # Verify header_n exists
  expect_true(exists("header_n", envir = tab))
  
  # Verify header_n is a data frame
  expect_true(is.data.frame(tab$header_n))
  
  # Verify header_n has expected columns
  expect_true("TRT01A" %in% names(tab$header_n))
  expect_true("n" %in% names(tab$header_n))
  
  # Verify header_n has correct number of treatment groups
  expect_equal(nrow(tab$header_n), length(unique(adsl$TRT01A)))
})

test_that("build_header_n calculates N values correctly", {
  # Create a simple table
  tab <- tplyr_table(adsl, TRT01A)
  
  # Build treatment groups first
  treatment_group_build(tab)
  
  # Call build_header_n
  build_header_n(tab)
  
  # Verify N values match actual counts
  for (trt in unique(adsl$TRT01A)) {
    expected_n <- sum(adsl$TRT01A == trt)
    actual_n <- tab$header_n$n[tab$header_n$TRT01A == trt]
    expect_equal(actual_n, expected_n)
  }
})

test_that("build_header_n works with column grouping variables", {
  # Create a table with cols
  tab <- tplyr_table(adsl, TRT01A, cols = vars(SEX))
  
  # Build treatment groups first
  treatment_group_build(tab)
  
  # Call build_header_n
  build_header_n(tab)
  
  # Verify header_n has cols variable
  expect_true("SEX" %in% names(tab$header_n))
  
  # Verify header_n has rows for each treatment x sex combination
  expected_rows <- length(unique(adsl$TRT01A)) * length(unique(adsl$SEX))
  expect_equal(nrow(tab$header_n), expected_rows)
  
  # Verify N values are correct for a specific combination
  trt_val <- unique(adsl$TRT01A)[1]
  sex_val <- unique(adsl$SEX)[1]
  expected_n <- sum(adsl$TRT01A == trt_val & adsl$SEX == sex_val)
  actual_n <- tab$header_n$n[tab$header_n$TRT01A == trt_val & tab$header_n$SEX == sex_val]
  expect_equal(actual_n, expected_n)
})

test_that("build_header_n works with distinct_by", {
  # Create a table with distinct_by
  tab <- tplyr_table(adsl, TRT01A) %>%
    set_distinct_by(USUBJID)
  
  # Build treatment groups first
  treatment_group_build(tab)
  
  # Call build_header_n
  build_header_n(tab)
  
  # Verify header_n exists
  expect_true(exists("header_n", envir = tab))
  
  # Verify N values reflect distinct counts
  for (trt in unique(adsl$TRT01A)) {
    expected_n <- length(unique(adsl$USUBJID[adsl$TRT01A == trt]))
    actual_n <- tab$header_n$n[tab$header_n$TRT01A == trt]
    expect_equal(actual_n, expected_n)
  }
})

test_that("build_header_n works with distinct_by and cols", {
  # Create a table with both distinct_by and cols
  tab <- tplyr_table(adsl, TRT01A, cols = vars(SEX)) %>%
    set_distinct_by(USUBJID)
  
  # Build treatment groups first
  treatment_group_build(tab)
  
  # Call build_header_n
  build_header_n(tab)
  
  # Verify header_n has both treatment and cols variables
  expect_true("TRT01A" %in% names(tab$header_n))
  expect_true("SEX" %in% names(tab$header_n))
  
  # Verify N values reflect distinct counts for a specific combination
  trt_val <- unique(adsl$TRT01A)[1]
  sex_val <- unique(adsl$SEX)[1]
  expected_n <- length(unique(adsl$USUBJID[adsl$TRT01A == trt_val & adsl$SEX == sex_val]))
  actual_n <- tab$header_n$n[tab$header_n$TRT01A == trt_val & tab$header_n$SEX == sex_val]
  expect_equal(actual_n, expected_n)
})

test_that("build_header_n does not leave temporary variables in table environment", {
  # Create a simple table
  tab <- tplyr_table(adsl, TRT01A)
  
  # Build treatment groups first
  treatment_group_build(tab)
  
  # Get list of bindings before build_header_n
  bindings_before <- ls(envir = tab, all.names = TRUE)
  
  # Call build_header_n
  build_header_n(tab)
  
  # Get list of bindings after build_header_n
  bindings_after <- ls(envir = tab, all.names = TRUE)
  
  # The only new binding should be header_n
  new_bindings <- setdiff(bindings_after, bindings_before)
  expect_equal(new_bindings, "header_n")
  
  # Verify the old temporary variable "df" does NOT exist
  expect_false(exists("df", envir = tab, inherits = FALSE))
})

test_that("build_header_n works with treatment groups", {
  # Create a table with treatment groups
  tab <- tplyr_table(adsl, TRT01A) %>%
    add_treat_grps("Xanomeline" = c("Xanomeline High Dose", "Xanomeline Low Dose"))
  
  # Build treatment groups first
  treatment_group_build(tab)
  
  # Call build_header_n
  build_header_n(tab)
  
  # Verify header_n includes the combined treatment group
  expect_true("Xanomeline" %in% tab$header_n$TRT01A)
  
  # Verify N value for combined group is correct
  expected_n <- sum(adsl$TRT01A %in% c("Xanomeline High Dose", "Xanomeline Low Dose"))
  actual_n <- tab$header_n$n[tab$header_n$TRT01A == "Xanomeline"]
  expect_equal(actual_n, expected_n)
})

test_that("build_header_n works with total group", {
  # Create a table with total group
  tab <- tplyr_table(adsl, TRT01A) %>%
    add_total_group()
  
  # Build treatment groups first
  treatment_group_build(tab)
  
  # Call build_header_n
  build_header_n(tab)
  
  # Verify header_n includes Total group
  expect_true("Total" %in% tab$header_n$TRT01A)
  
  # Verify N value for Total group is correct
  expected_n <- nrow(adsl)
  actual_n <- tab$header_n$n[tab$header_n$TRT01A == "Total"]
  expect_equal(actual_n, expected_n)
})

test_that("build_header_n handles empty groups correctly", {
  # Create a table with cols that might have empty combinations
  tab <- tplyr_table(adsl, TRT01A, cols = vars(SEX))
  
  # Build treatment groups first
  treatment_group_build(tab)
  
  # Call build_header_n
  build_header_n(tab)
  
  # Verify complete() filled in any missing combinations with n=0
  # All treatment x sex combinations should exist
  expected_rows <- length(unique(tab$built_pop_data$TRT01A)) * length(unique(tab$built_pop_data$SEX))
  expect_equal(nrow(tab$header_n), expected_rows)
})

test_that("build_header_n returns table object", {
  # Create a simple table
  tab <- tplyr_table(adsl, TRT01A)
  
  # Build treatment groups first
  treatment_group_build(tab)
  
  # Call build_header_n and capture result
  result <- build_header_n(tab)
  
  # Verify result is the table
  expect_identical(result, tab)
})

test_that("build_header_n works with separate population data", {
  # Create a subset for target data
  target_subset <- adsl[adsl$AGE >= 65, ]
  
  # Create a table with separate population data
  tab <- tplyr_table(target_subset, TRT01A) %>%
    set_pop_data(adsl) %>%
    set_pop_treat_var(TRT01A)
  
  # Build treatment groups first
  treatment_group_build(tab)
  
  # Call build_header_n
  build_header_n(tab)
  
  # Verify header_n uses population data (not target data)
  # N values should reflect full population, not filtered target
  for (trt in unique(adsl$TRT01A)) {
    expected_n <- sum(adsl$TRT01A == trt)  # Full population
    actual_n <- tab$header_n$n[tab$header_n$TRT01A == trt]
    expect_equal(actual_n, expected_n)
  }
})

test_that("build_header_n handles multiple cols correctly", {
  # Create a table with multiple cols
  tab <- tplyr_table(adsl, TRT01A, cols = vars(SEX, AGEGR1))
  
  # Build treatment groups first
  treatment_group_build(tab)
  
  # Call build_header_n
  build_header_n(tab)
  
  # Verify header_n has all cols variables
  expect_true("SEX" %in% names(tab$header_n))
  expect_true("AGEGR1" %in% names(tab$header_n))
  
  # Verify N values are correct for a specific combination
  trt_val <- unique(adsl$TRT01A)[1]
  sex_val <- unique(adsl$SEX)[1]
  age_val <- unique(adsl$AGEGR1)[1]
  expected_n <- sum(adsl$TRT01A == trt_val & adsl$SEX == sex_val & adsl$AGEGR1 == age_val)
  actual_n <- tab$header_n$n[tab$header_n$TRT01A == trt_val & 
                              tab$header_n$SEX == sex_val & 
                              tab$header_n$AGEGR1 == age_val]
  expect_equal(actual_n, expected_n)
})

test_that("build_header_n errors when cols variables not in pop_data", {
  # Create a table
  tab <- tplyr_table(adsl, TRT01A)
  
  # Build treatment groups first
  treatment_group_build(tab)
  
  # Manually set cols to a variable that doesn't exist in built_pop_data
  # This simulates the scenario where cols are set after treatment_group_build
  tab$cols <- quos(NONEXISTENT_VAR)
  
  # Expect an error when calling build_header_n because NONEXISTENT_VAR is not in built_pop_data
  expect_error(
    build_header_n(tab),
    "NONEXISTENT_VAR"
  )
})

test_that("build_header_n maintains factor levels", {
  # Create a table
  tab <- tplyr_table(adsl, TRT01A)
  
  # Build treatment groups first
  treatment_group_build(tab)
  
  # Call build_header_n
  build_header_n(tab)
  
  # Verify treatment variable in header_n is a factor
  expect_true(is.factor(tab$header_n$TRT01A))
  
  # Verify factor levels match built_pop_data
  expect_equal(levels(tab$header_n$TRT01A), levels(tab$built_pop_data$TRT01A))
})
