# Tplyr Testing Strategy Guide

## Purpose

This document outlines the testing strategy for Tplyr and provides guidance for maintaining test coverage during refactoring. Comprehensive testing is critical for a package used in regulated pharmaceutical environments.

## Testing Philosophy

### Core Principles

1. **Test Behavior, Not Implementation**: Tests should verify what the code does, not how it does it
2. **Comprehensive Coverage**: All user-facing functionality must be tested
3. **Fast Feedback**: Tests should run quickly to enable frequent execution
4. **Clear Failures**: Test failures should clearly indicate what broke
5. **Regression Prevention**: Tests should catch regressions from refactoring

### Testing Pyramid

```
        ┌─────────────┐
        │   Manual    │  ← Minimal: Vignette examples
        │   Testing   │
        └─────────────┘
       ┌───────────────┐
       │  Integration  │  ← Moderate: Full table builds
       │    Tests      │
       └───────────────┘
      ┌─────────────────┐
      │   Unit Tests    │  ← Extensive: Individual functions
      └─────────────────┘
```

## Test Suite Structure

### Current Test Organization

```
tests/
├── testthat.R                    # Test runner
└── testthat/
    ├── _snaps/                   # Snapshot test expectations
    │   ├── apply_formats.md
    │   ├── count.md
    │   ├── desc.md
    │   └── ...
    ├── test-*.R                  # Test files (one per module)
    ├── *.Rdata                   # Test data
    └── count_t*                  # Reference files
```

### Test File Naming Convention

- `test-<module>.R`: Tests for R/<module>.R
- Example: `test-count.R` tests `R/count.R`
- One test file per source file (generally)

## Types of Tests

### 1. Unit Tests

**Purpose**: Test individual functions in isolation

**Characteristics**:
- Fast execution (< 1ms per test)
- No dependencies on other modules
- Test single function or method
- Use minimal test data

**Example**:
```r
test_that("f_str captures format correctly", {
  fs <- f_str('xx.x', mean)
  
  expect_equal(fs$format_string, 'xx.x')
  expect_equal(fs$vars, list(quote(mean)))
  expect_equal(fs$size, 4)
})
```

**Coverage Areas**:
- Format string parsing
- Numeric formatting
- String manipulation
- Validation functions
- Helper utilities

### 2. Integration Tests

**Purpose**: Test multiple components working together

**Characteristics**:
- Moderate execution time (< 100ms per test)
- Test layer processing end-to-end
- Use realistic test data
- Verify output structure

**Example**:
```r
test_that("count layer builds correctly", {
  tab <- tplyr_table(mtcars, am) %>%
    add_layer(
      group_count(cyl) %>%
        set_format_strings(f_str('xx (xx.x%)', n, pct))
    )
  
  result <- build(tab)
  
  expect_s3_class(result, "data.frame")
  expect_true("row_label1" %in% names(result))
  expect_true("var1_0" %in% names(result))
  expect_equal(nrow(result), 3)  # 3 cylinder values
})
```

**Coverage Areas**:
- Full layer processing
- Table building
- Multiple layers together
- Treatment group expansion
- Population data integration

### 3. Snapshot Tests

**Purpose**: Detect unintended changes in output format

**Characteristics**:
- Captures full output as text
- Compares against saved snapshot
- Detects any formatting changes
- Requires manual review of changes

**Example**:
```r
test_that("count layer output format is stable", {
  tab <- tplyr_table(mtcars, am) %>%
    add_layer(group_count(cyl))
  
  result <- build(tab)
  
  expect_snapshot(result)
})
```

**Coverage Areas**:
- Output data frame structure
- Column names and ordering
- Formatted string output
- Print methods

### 4. Property-Based Tests

**Purpose**: Test with wide range of inputs

**Characteristics**:
- Generate random test cases
- Verify properties hold across inputs
- Catch edge cases
- Currently limited in Tplyr

**Potential Example**:
```r
test_that("percentages always sum to 100", {
  # Generate random data
  # Build count table
  # Verify percentages sum to 100 (within rounding)
})
```

**Potential Coverage Areas**:
- Percentage calculations
- Numeric formatting
- Sorting stability
- Denominator calculations

### 5. Regression Tests

**Purpose**: Prevent reintroduction of fixed bugs

**Characteristics**:
- Created when bugs are found
- Verify bug stays fixed
- Often edge cases
- Document the bug in test name

**Example**:
```r
test_that("min/max handle all-NA data correctly (issue #123)", {
  data <- data.frame(
    treat = c("A", "A", "B", "B"),
    value = c(NA, NA, NA, NA)
  )
  
  tab <- tplyr_table(data, treat) %>%
    add_layer(
      group_desc(value) %>%
        set_format_strings('Min, Max' = f_str('xx, xx', min, max))
    )
  
  result <- build(tab)
  
  # Should show NA, not Inf
  expect_true(grepl("NA", result$var1_A[1]))
})
```

## Test Data Strategy

### Built-in Test Datasets

Tplyr includes several test datasets in `/data`:
- `tplyr_adsl`: Subject-level data
- `tplyr_adae`: Adverse events
- `tplyr_adlb`: Laboratory data
- `tplyr_adas`: ADAS cognitive scores
- `tplyr_adpe`: Pharmacokinetic data

**Usage**:
```r
test_that("works with ADSL data", {
  tab <- tplyr_table(tplyr_adsl, TRT01P)
  # ...
})
```

### Minimal Test Data

For unit tests, create minimal data inline:

```r
test_that("handles single group", {
  data <- data.frame(
    treat = c("A", "A", "A"),
    value = c(1, 2, 3)
  )
  
  # Test with minimal data
})
```

### Edge Case Data

Create specific datasets for edge cases:

```r
# Empty group
data_empty <- data.frame(
  treat = character(0),
  value = numeric(0)
)

# All NA
data_all_na <- data.frame(
  treat = c("A", "A"),
  value = c(NA, NA)
)

# Single observation
data_single <- data.frame(
  treat = "A",
  value = 1
)
```

## Critical Test Scenarios

### Scenario 1: Basic Table Building

**What to Test**:
- Table construction
- Layer addition
- Build execution
- Output structure

**Test Cases**:
```r
test_that("basic count table builds", { })
test_that("basic desc table builds", { })
test_that("basic shift table builds", { })
test_that("multi-layer table builds", { })
```

### Scenario 2: Treatment Groups

**What to Test**:
- Treatment group expansion
- Total group addition
- Column naming
- Header N calculation

**Test Cases**:
```r
test_that("add_treat_grps creates new columns", { })
test_that("add_total_group sums all groups", { })
test_that("header_n uses population data", { })
```

### Scenario 3: Format Strings

**What to Test**:
- Format string parsing
- Numeric formatting
- Auto-precision
- Parenthesis hugging

**Test Cases**:
```r
test_that("f_str parses format correctly", { })
test_that("auto-precision calculates correctly", { })
test_that("parenthesis hugging works", { })
test_that("multiple summaries combine correctly", { })
```

### Scenario 4: Count Layers

**What to Test**:
- Basic counting
- Distinct counting
- Nested counting
- Total rows
- Missing values
- Denominators

**Test Cases**:
```r
test_that("counts are correct", { })
test_that("percentages are correct", { })
test_that("distinct counts work", { })
test_that("nested counts work", { })
test_that("total row sums correctly", { })
test_that("denominators use pop_data", { })
```

### Scenario 5: Desc Layers

**What to Test**:
- Built-in statistics
- Custom statistics
- Multi-variable summaries
- Precision calculation

**Test Cases**:
```r
test_that("mean calculates correctly", { })
test_that("sd calculates correctly", { })
test_that("custom summaries work", { })
test_that("multi-variable summaries work", { })
test_that("precision_by works", { })
```

### Scenario 6: Shift Layers

**What to Test**:
- Row/column matrix creation
- Factor-based dummy values
- Denominators

**Test Cases**:
```r
test_that("shift matrix is correct", { })
test_that("factor levels create dummy rows", { })
test_that("shift denominators work", { })
```

### Scenario 7: Metadata

**What to Test**:
- Metadata generation
- Metadata structure
- Metadata extraction
- Custom metadata

**Test Cases**:
```r
test_that("metadata builds correctly", { })
test_that("get_metadata returns correct structure", { })
test_that("get_meta_result works", { })
test_that("append_metadata works", { })
```

### Scenario 8: Edge Cases

**What to Test**:
- Empty data
- Single observation
- All NA values
- Single treatment group
- No by variables
- Missing factor levels

**Test Cases**:
```r
test_that("handles empty data", { })
test_that("handles all NA", { })
test_that("handles single group", { })
test_that("handles no by variables", { })
```

## Testing Workflow

### Before Refactoring

1. **Run Full Test Suite**
   ```r
   devtools::test()
   ```
   
2. **Document Baseline**
   - Record number of tests
   - Record any failures
   - Note execution time

3. **Run R CMD Check**
   ```r
   devtools::check()
   ```
   
4. **Identify Affected Tests**
   - Which tests cover code you're changing?
   - Which tests might be affected indirectly?

### During Refactoring

1. **Run Affected Tests Frequently**
   ```r
   devtools::test_file("tests/testthat/test-count.R")
   ```

2. **Fix Failures Immediately**
   - Don't accumulate test failures
   - Understand why each test fails
   - Fix code or update test (with justification)

3. **Add Tests for New Code**
   - New functions need new tests
   - New branches need new test cases
   - Edge cases need explicit tests

### After Refactoring

1. **Run Full Test Suite**
   ```r
   devtools::test()
   ```
   
2. **Verify All Tests Pass**
   - No new failures
   - No skipped tests
   - No warnings

3. **Check Coverage**
   ```r
   covr::package_coverage()
   ```
   
4. **Run R CMD Check**
   ```r
   devtools::check()
   ```

5. **Manual Testing**
   - Run vignette examples
   - Test with real data if available
   - Verify output looks correct

## Test Maintenance

### Updating Snapshot Tests

When output format intentionally changes:

1. **Review Changes Carefully**
   ```r
   testthat::snapshot_review()
   ```

2. **Verify Changes Are Correct**
   - Is the new output correct?
   - Is the change intentional?
   - Is it documented?

3. **Accept Changes**
   ```r
   testthat::snapshot_accept()
   ```

4. **Document in NEWS.md**
   - Note the output change
   - Explain why it changed
   - Provide migration guidance if needed

### Updating Tests After API Changes

If refactoring changes internal APIs:

1. **Update Test Code**
   - Change function calls to match new API
   - Update expectations if behavior changed

2. **Don't Change Test Intent**
   - Test should still verify same behavior
   - Only change how test achieves verification

3. **Add Deprecation Tests**
   - If old API is deprecated, test deprecation warning
   - Verify old API still works (during deprecation period)

## Test Quality Guidelines

### Good Test Characteristics

1. **Focused**: Tests one thing
2. **Independent**: Doesn't depend on other tests
3. **Repeatable**: Same result every time
4. **Fast**: Runs in milliseconds
5. **Clear**: Easy to understand what's being tested

### Test Naming

Use descriptive names that explain what's being tested:

```r
# GOOD
test_that("count layer calculates percentages correctly", { })
test_that("f_str handles auto-precision", { })
test_that("build() fails with invalid layer", { })

# BAD
test_that("test1", { })
test_that("it works", { })
test_that("count", { })
```

### Test Structure

Follow Arrange-Act-Assert pattern:

```r
test_that("description", {
  # Arrange: Set up test data and objects
  data <- data.frame(...)
  tab <- tplyr_table(data, treat)
  
  # Act: Execute the code being tested
  result <- build(tab)
  
  # Assert: Verify the results
  expect_equal(nrow(result), 3)
  expect_true("var1_A" %in% names(result))
})
```

### Assertion Guidelines

Use specific assertions:

```r
# GOOD - specific
expect_equal(result$n, c(10, 20, 30))
expect_s3_class(tab, "tplyr_table")
expect_true("var1_A" %in% names(result))

# BAD - too general
expect_true(is.data.frame(result))
expect_true(length(result) > 0)
```

## Common Testing Pitfalls

### Pitfall 1: Testing Implementation Instead of Behavior

**Problem**: Test breaks when implementation changes, even if behavior is correct

**Example**:
```r
# BAD - tests implementation
test_that("uses dplyr::group_by internally", {
  # Don't test internal implementation details
})

# GOOD - tests behavior
test_that("groups data by treatment", {
  # Test the result, not how it's achieved
})
```

### Pitfall 2: Overly Specific Expectations

**Problem**: Test is brittle, breaks with minor changes

**Example**:
```r
# BAD - too specific
expect_equal(result, expected_result)  # Breaks if column order changes

# GOOD - test what matters
expect_equal(result$var1_A, expected_result$var1_A)
expect_true(all(c("row_label1", "var1_A") %in% names(result)))
```

### Pitfall 3: Not Testing Edge Cases

**Problem**: Code works for typical inputs but fails on edge cases

**Solution**: Explicitly test edge cases

```r
test_that("handles empty data", { })
test_that("handles all NA", { })
test_that("handles single observation", { })
```

### Pitfall 4: Slow Tests

**Problem**: Tests take too long, developers don't run them frequently

**Solution**: 
- Use minimal test data
- Mock expensive operations
- Separate slow integration tests from fast unit tests

### Pitfall 5: Unclear Failure Messages

**Problem**: Test fails but message doesn't explain why

**Solution**: Use descriptive expectations

```r
# GOOD
expect_equal(
  result$n, 
  c(10, 20, 30),
  info = "Count values should match expected frequencies"
)
```

## Continuous Integration

### CI Workflow

Tplyr uses GitHub Actions for CI:

1. **On Every Push**:
   - Run R CMD check
   - Run test suite
   - Check code coverage

2. **On Pull Requests**:
   - Same as push
   - Require passing tests before merge

3. **On Release**:
   - Full test suite
   - R CMD check on multiple R versions
   - Check on multiple OS (Windows, Mac, Linux)

### Local Pre-Commit Checks

Before committing, run:

```r
# Quick check
devtools::test()

# Full check (slower)
devtools::check()
```

## Test Coverage Goals

### Current Coverage

Check current coverage:
```r
covr::package_coverage()
```

### Coverage Goals

- **Overall**: > 80%
- **Core modules**: > 90%
  - table.R
  - layer.R
  - count.R
  - desc.R
  - shift.R
  - build.R
  - format.R
- **Utility modules**: > 70%

### Coverage Exceptions

Some code is difficult to test:
- Interactive functions
- Print methods (tested via snapshots)
- Error handling for rare conditions

## Debugging Failed Tests

### Step 1: Understand the Failure

- Read the error message carefully
- Identify which test failed
- Understand what the test was checking

### Step 2: Reproduce Locally

```r
# Run single test file
devtools::test_file("tests/testthat/test-count.R")

# Run single test
testthat::test_that("specific test", { ... })
```

### Step 3: Debug Interactively

```r
# Set breakpoint in test
browser()

# Or use debugonce
debugonce(function_being_tested)
```

### Step 4: Fix and Verify

- Fix the code or test
- Run test again
- Run full suite to check for side effects

## Conclusion

Comprehensive testing is essential for maintaining Tplyr's reliability, especially during refactoring. Follow these guidelines:

1. **Test behavior, not implementation**
2. **Maintain high coverage**
3. **Run tests frequently**
4. **Fix failures immediately**
5. **Add tests for new code**
6. **Update tests carefully when refactoring**
7. **Use clear, descriptive test names**
8. **Test edge cases explicitly**

Remember: **Tests are documentation of how the code should behave. They're as important as the code itself.**
