# Design Document: Refactor evalq() Pattern to Functional Design

## Overview

This document outlines the technical design for refactoring Tplyr's use of `evalq()` to a functional Extract-Process-Bind pattern. The refactoring will eliminate code complexity, prevent unintended side effects, and improve testability while maintaining complete backward compatibility.

### Goals

1. Eliminate all uses of `evalq()` for multi-line code blocks
2. Adopt Extract-Process-Bind pattern consistently across codebase
3. Maintain 100% backward compatibility
4. Improve code clarity and testability
5. Eliminate unintended side effects in table/layer environments

### Non-Goals

1. Changing user-facing APIs
2. Modifying environment-based object model
3. Changing S3 dispatch patterns
4. Adding new features
5. Performance optimization beyond maintaining current performance

## Architecture

### Current Architecture Problem

The current architecture uses `evalq()` to execute entire function bodies within table/layer environments:

```
┌─────────────────────────────────────┐
│ Function Call                       │
│ treatment_group_build(table)        │
└─────────────────────────────────────┘
           ↓
┌─────────────────────────────────────┐
│ evalq({ ... }, envir=table)         │
│ ┌─────────────────────────────────┐ │
│ │ Entire function body executes   │ │
│ │ INSIDE table environment        │ │
│ │                                 │ │
│ │ - Creates temporary variables   │ │
│ │ - Modifies existing variables   │ │
│ │ - Side effects everywhere       │ │
│ │ - Manual cleanup required       │ │
│ └─────────────────────────────────┘ │
└─────────────────────────────────────┘
           ↓
┌─────────────────────────────────────┐
│ Table Environment (polluted)        │
│ - built_target (intended)           │
│ - built_pop_data (intended)         │
│ - grp_i (unintended)                │
│ - i (unintended)                    │
│ - fct_levels (unintended)           │
└─────────────────────────────────────┘
```

**Problems:**
- Temporary variables pollute environment
- Unclear what's being read vs written
- Difficult to test in isolation
- Manual cleanup required (and often forgotten)
- Side effects can impact subsequent functions

### New Architecture Solution

The new architecture uses Extract-Process-Bind pattern:

```
┌─────────────────────────────────────┐
│ Function Call                       │
│ treatment_group_build(table)        │
└─────────────────────────────────────┘
           ↓
┌─────────────────────────────────────┐
│ EXTRACT Phase                       │
│ target <- table$target              │
│ treat_var <- table$treat_var        │
│ pop_data <- table$pop_data          │
│ ... (explicit reads)                │
└─────────────────────────────────────┘
           ↓
┌─────────────────────────────────────┐
│ PROCESS Phase                       │
│ ┌─────────────────────────────────┐ │
│ │ Function Environment            │ │
│ │ (isolated from table)           │ │
│ │                                 │ │
│ │ built_target <- process(...)    │ │
│ │ fct_levels <- unique(...)       │ │
│ │ ... (all local variables)       │ │
│ └─────────────────────────────────┘ │
└─────────────────────────────────────┘
           ↓
┌─────────────────────────────────────┐
│ BIND Phase                          │
│ table$built_target <- built_target  │
│ table$built_pop_data <- built_pop_data │
│ ... (explicit writes)               │
└─────────────────────────────────────┘
           ↓
┌─────────────────────────────────────┐
│ Table Environment (clean)           │
│ - built_target (intended)           │
│ - built_pop_data (intended)         │
│ - NO temporary variables            │
└─────────────────────────────────────┘
```

**Benefits:**
- Clear separation of concerns
- No environment pollution
- Easy to test
- No manual cleanup needed
- Explicit data flow

## Components and Interfaces

### Core Pattern Interface

All refactored functions will follow this interface:

```r
#' Function description
#'
#' @param env_obj Table or layer environment object
#' @return The environment object (invisibly) or specific data
#' @noRd
function_name <- function(env_obj) {
  # EXTRACT: Get what we need
  var1 <- env_obj$var1
  var2 <- env_obj$var2
  
  # PROCESS: Do work in function environment
  result <- process_data(var1, var2)
  
  # BIND: Write results back
  env_obj$result <- result
  
  invisible(env_obj)
}
```

### Function Categories

Functions fall into three categories based on their refactoring needs:

#### Category 1: Environment Modifiers
Functions that modify table/layer environment and return invisibly.

**Pattern:**
```r
function_name <- function(env_obj) {
  # Extract
  # Process
  # Bind
  invisible(env_obj)
}
```

**Examples:**
- `treatment_group_build()`
- `build_header_n()`
- `factor_treat_var()`

#### Category 2: Data Processors
Functions that process data and bind results to environment.

**Pattern:**
```r
function_name <- function(env_obj) {
  # Extract
  # Process
  # Bind results
  invisible(env_obj)
}
```

**Examples:**
- `process_summaries()` methods
- `process_formatting()` methods
- `process_metadata()` methods

#### Category 3: Data Extractors
Functions that extract and return data without modifying environment.

**Pattern:**
```r
function_name <- function(env_obj) {
  # Extract
  # Process
  # Return (no bind)
  return(result)
}
```

**Examples:**
- `get_data_order()`
- Helper functions that compute values

## Data Models

### Environment Bindings

#### Table Environment Bindings

**Read-Only (Input) Bindings:**
- `target`: Source dataset
- `treat_var`: Treatment variable quosure
- `pop_data`: Population dataset
- `pop_treat_var`: Population treatment variable quosure
- `table_where`: Filter quosure
- `pop_where`: Population filter quosure
- `treat_grps`: List of treatment group definitions
- `cols`: Column grouping variables
- `count_layer_formats`: Default count formats
- `desc_layer_formats`: Default desc formats
- `shift_layer_formats`: Default shift formats

**Write (Output) Bindings:**
- `built_target`: Processed target dataset
- `built_pop_data`: Processed population dataset
- `header_n`: Header N values

**Temporary Bindings to Eliminate:**
- `grp_i`, `i`: Loop counters
- `fct_levels`: Temporary factor levels
- Any other variables created during processing

#### Layer Environment Bindings

**Read-Only (Input) Bindings:**
- `target_var`: Target variable quosure(s)
- `by`: Grouping variable quosures
- `where`: Filter quosure
- `cols`: Column grouping variables (inherited)
- `treat_var`: Treatment variable (inherited)
- Layer-specific configuration (format_strings, distinct_by, etc.)

**Write (Output) Bindings:**
- `numeric_data`: Numeric calculation results
- `built_table`: Processed data table
- `formatted_data`: Formatted output
- `metadata`: Traceability metadata
- Layer-specific results

**Temporary Bindings to Eliminate:**
- Loop counters
- Intermediate calculation variables
- Temporary data frames

### Data Flow

```
User Code
    ↓
tplyr_table() ← Creates table environment
    ↓
add_layer() ← Creates layer environments
    ↓
build() ← Triggers processing
    ↓
┌─────────────────────────────────────┐
│ Table Pre-Processing                │
│                                     │
│ treatment_group_build(table)        │
│   Extract: target, treat_var, ...  │
│   Process: Filter, factor, expand  │
│   Bind: built_target, built_pop_data│
│                                     │
│ build_header_n(table)               │
│   Extract: built_pop_data, ...     │
│   Process: Calculate N values      │
│   Bind: header_n                   │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│ Layer Processing (per layer)        │
│                                     │
│ process_summaries(layer)            │
│   Extract: built_target, target_var │
│   Process: Calculate statistics    │
│   Bind: numeric_data               │
│                                     │
│ process_formatting(layer)           │
│   Extract: numeric_data, formats   │
│   Process: Format strings, pivot   │
│   Bind: formatted_data             │
│                                     │
│ process_metadata(layer) [optional]  │
│   Extract: numeric_data, filters   │
│   Process: Build metadata          │
│   Bind: metadata                   │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│ Table Post-Processing               │
│ - Stack layers                      │
│ - Order columns                     │
│ - Assemble metadata                 │
└─────────────────────────────────────┘
    ↓
Output DataFrame
```

## Detailed Component Design

### 1. treatment_group_build()

**Location:** `R/prebuild.R`

**Current Implementation:**
```r
treatment_group_build <- function(table) {
  output <- evalq({
    # 100+ lines of code executing in table environment
  }, envir=table)
  invisible(table)
}
```

**New Implementation:**
```r
treatment_group_build <- function(table) {
  # EXTRACT
  target <- table$target
  treat_var <- table$treat_var
  pop_data <- table$pop_data
  pop_treat_var <- table$pop_treat_var
  table_where <- table$table_where
  pop_where <- table$pop_where
  treat_grps <- table$treat_grps
  cols <- table$cols
  
  # PROCESS
  # Make built_target a copy of target
  built_target <- clean_attr(target)
  
  # Convert to factor if needed
  if (!is.factor(built_target[[as_name(treat_var)]])) {
    built_target <- built_target %>%
      mutate(!!treat_var := factor(!!treat_var))
  }
  
  # Same for pop_data
  built_pop_data <- clean_attr(pop_data)
  if (!is.factor(built_pop_data[[as_name(pop_treat_var)]])) {
    built_pop_data <- built_pop_data %>%
      mutate(!!pop_treat_var := factor(!!pop_treat_var))
  }
  
  # Capture all source factor levels (local variable)
  fct_levels <- unique(c(
    levels(built_pop_data[[as_name(pop_treat_var)]]),
    levels(built_target[[as_name(treat_var)]]),
    names(treat_grps)
  ))
  
  # Apply filters with error handling
  tryCatch({
    built_target <- built_target %>% filter(!!table_where)
  }, error = function(e) {
    abort(paste0("tplyr_table `where` condition `",
                 as_label(table_where),
                 "` is invalid. Filter error:\n", e))
  })
  
  tryCatch({
    built_pop_data <- built_pop_data %>% filter(!!pop_where)
  }, error = function(e) {
    abort(paste0("Population data `pop_where` condition `",
                 as_label(pop_where),
                 "` is invalid. Filter error:\n", e,
                 "If the population data and target data subsets should be different, use `set_pop_where`."))
  })
  
  # Preserve factors in cols
  for(i in seq_along(cols)) {
    built_target <- built_target %>%
      mutate(!!cols[[i]] := fct_expand(as.character(!!cols[[i]]),
                                       as.character(unique(target[[as_name(cols[[i]])]])),
                                       levels(target[, as_name(cols[[i]])])))
    built_pop_data <- built_pop_data %>%
      mutate(!!cols[[i]] := fct_expand(as.character(!!cols[[i]]),
                                       as.character(unique(pop_data[[as_name(cols[[i]])]])),
                                       levels(pop_data[, as_name(cols[[i]])])))
  }
  
  # Add treatment groups
  for (grp_i in seq_along(treat_grps)) {
    built_target <- built_target %>%
      filter(!!treat_var %in% treat_grps[[grp_i]]) %>%
      mutate(!!treat_var := names(treat_grps)[grp_i]) %>%
      bind_rows(built_target)
  }
  
  for (grp_i in seq_along(treat_grps)) {
    built_pop_data <- built_pop_data %>%
      filter(!!pop_treat_var %in% treat_grps[[grp_i]]) %>%
      mutate(!!pop_treat_var := names(treat_grps)[grp_i]) %>%
      bind_rows(built_pop_data)
  }
  
  # Restore factor levels
  built_target <- built_target %>%
    mutate(!!treat_var := factor(!!treat_var, levels = fct_levels))
  
  built_pop_data <- built_pop_data %>%
    mutate(!!pop_treat_var := factor(!!pop_treat_var, levels = fct_levels))
  
  # Note: fct_levels, i, grp_i are local variables - no cleanup needed
  
  # BIND
  table$built_target <- built_target
  table$built_pop_data <- built_pop_data
  
  invisible(table)
}
```

**Key Changes:**
- Explicit extraction of all needed bindings
- All processing in function environment
- Local variables (fct_levels, i, grp_i) don't pollute table environment
- Explicit binding of results
- No `evalq()` wrapper
- No manual cleanup needed

### 2. process_summaries() Methods

**Location:** `R/count.R`, `R/desc.R`, `R/shift.R`

**Pattern for All Methods:**
```r
process_summaries.count_layer <- function(x, ...) {
  # EXTRACT
  built_target <- x$built_target
  target_var <- x$target_var
  by <- x$by
  where <- x$where
  treat_var <- x$treat_var
  cols <- x$cols
  # ... other needed bindings
  
  # PROCESS
  # Perform calculations in function environment
  numeric_data <- built_target %>%
    filter(!!where) %>%
    group_by(!!treat_var, !!!by, !!!target_var) %>%
    summarize(n = n(), .groups = "drop")
  
  # Additional processing...
  
  # BIND
  x$numeric_data <- numeric_data
  
  invisible(x)
}
```

**Applies to:**
- `process_summaries.count_layer()`
- `process_summaries.desc_layer()`
- `process_summaries.shift_layer()`

### 3. Helper Functions

**Location:** Various files

**Pattern:**
```r
process_count_n <- function(layer) {
  # EXTRACT
  numeric_data <- layer$numeric_data
  denoms_by <- layer$denoms_by
  treat_var <- layer$treat_var
  cols <- layer$cols
  
  # PROCESS
  # Calculate percentages
  result <- numeric_data %>%
    group_by(!!!denoms_by) %>%
    mutate(pct = n / sum(n) * 100)
  
  # BIND
  layer$numeric_data <- result
  
  invisible(layer)
}
```

**Applies to:**
- `process_count_n()`
- `process_count_total_row()`
- `process_missing_subjects_row()`
- `process_count_denoms()`
- `process_shift_n()`
- `process_shift_total()`
- `process_shift_denoms()`
- `factor_treat_var()`
- `rename_missing_values()`
- And many others

### 4. Sorting Functions

**Location:** `R/sort.R`

**Pattern:**
```r
add_order_columns.count_layer <- function(x) {
  # EXTRACT
  formatted_data <- x$formatted_data
  by <- x$by
  target_var <- x$target_var
  order_count_method <- x$order_count_method
  # ... other needed bindings
  
  # PROCESS
  # Add ordering columns
  ordered_data <- formatted_data %>%
    mutate(ord_layer_1 = ..., ord_layer_2 = ...)
  
  # BIND
  x$formatted_data <- ordered_data
  
  invisible(x)
}
```

### 5. Metadata Functions

**Location:** `R/process_metadata.R`

**Pattern:**
```r
process_metadata.count_layer <- function(x, ...) {
  # EXTRACT
  numeric_data <- x$numeric_data
  where <- x$where
  by <- x$by
  target_var <- x$target_var
  # ... other needed bindings
  
  # PROCESS
  # Build metadata
  metadata <- numeric_data %>%
    mutate(
      filter_expr = as_label(where),
      grouping_vars = paste(map_chr(by, as_name), collapse = ", ")
    )
  
  # BIND
  x$metadata <- metadata
  
  invisible(x)
}
```

## Error Handling

### Principle
Error handling remains unchanged - same error messages, same error conditions.

### Pattern
```r
function_name <- function(env_obj) {
  # EXTRACT
  var1 <- env_obj$var1
  
  # PROCESS with error handling
  tryCatch({
    result <- process(var1)
  }, error = function(e) {
    abort(paste0("Clear error message: ", e))
  })
  
  # BIND
  env_obj$result <- result
  
  invisible(env_obj)
}
```

### Examples
- Filter errors in `treatment_group_build()`
- Variable validation errors in layer construction
- Format string parsing errors

## Testing Strategy

### Unit Testing Approach

**Before Refactoring:**
```r
# Difficult to test - requires full table setup
test_that("treatment_group_build works", {
  table <- tplyr_table(data, treat)
  treatment_group_build(table)
  expect_true(exists("built_target", envir = table))
})
```

**After Refactoring:**
```r
# Easier to test - can verify inputs/outputs
test_that("treatment_group_build works", {
  table <- tplyr_table(data, treat)
  treatment_group_build(table)
  
  # Can inspect results
  expect_true(!is.null(table$built_target))
  expect_equal(nrow(table$built_target), expected_rows)
  
  # Can verify no pollution
  expect_false(exists("fct_levels", envir = table))
  expect_false(exists("grp_i", envir = table))
})
```

### Integration Testing

All existing integration tests should pass without modification:
- Full table builds
- Multi-layer tables
- All layer types
- Metadata generation
- All vignette examples

### Regression Testing

Use existing test suite as regression tests:
- All tests in `tests/testthat/`
- All snapshot tests
- UAT test suite

### New Tests

Add tests for refactored functions:
- Verify no environment pollution
- Verify explicit bindings
- Verify error handling
- Verify edge cases

## Performance Considerations

### Expected Performance Impact

**Minimal to None:**
- Extracting bindings: O(1) operations
- Function calls: Negligible overhead in R
- No additional data copying

### Performance Testing

```r
library(bench)

# Before refactoring
old_time <- mark(
  old_treatment_group_build(table),
  iterations = 100
)

# After refactoring
new_time <- mark(
  new_treatment_group_build(table),
  iterations = 100
)

# Compare
compare <- summary(old_time)$median / summary(new_time)$median
expect_true(compare > 0.9 && compare < 1.1)  # Within 10%
```

### Optimization Opportunities

If performance degrades:
1. Profile to identify bottlenecks
2. Consider caching extracted bindings if accessed multiple times
3. Use `env_get()` for batch extraction if needed
4. Optimize data processing logic (not extraction/binding)

## Migration Strategy

### Phase 1: Preparation
1. Document all `evalq()` usage
2. Establish performance baseline
3. Ensure test suite is comprehensive
4. Create refactoring branch

### Phase 2: Core Functions
1. Refactor `treatment_group_build()`
2. Run tests, verify no regressions
3. Refactor `build_header_n()`
4. Run tests, verify no regressions

### Phase 3: Layer Processing
1. Refactor `process_summaries()` methods
2. Run tests after each method
3. Refactor `process_formatting()` methods
4. Run tests after each method
5. Refactor `process_metadata()` methods
6. Run tests after each method

### Phase 4: Helper Functions
1. Refactor count layer helpers
2. Refactor desc layer helpers
3. Refactor shift layer helpers
4. Refactor sorting functions
5. Run tests after each group

### Phase 5: Validation
1. Run full test suite
2. Run R CMD check
3. Run UAT tests
4. Performance benchmarking
5. Code review

### Phase 6: Documentation
1. Update internal documentation
2. Update NEWS.md
3. Add comments explaining pattern
4. Update developer guide

### Rollback Plan

If issues arise:
1. Each phase is in separate commits
2. Can rollback individual functions
3. Can rollback entire phases
4. Git history preserves all changes

## Backward Compatibility

### User-Facing API

**No Changes:**
- All user-facing functions unchanged
- All function signatures unchanged
- All return values unchanged
- All output formats unchanged

### Internal API

**Changes:**
- Functions no longer use `evalq()`
- Functions follow Extract-Process-Bind pattern
- Environment bindings remain the same
- S3 dispatch unchanged

### Verification

```r
# Before and after should produce identical output
before <- tplyr_table(data, treat) %>%
  add_layer(group_count(var)) %>%
  build()

after <- tplyr_table(data, treat) %>%
  add_layer(group_count(var)) %>%
  build()

expect_identical(before, after)
```

## Documentation

### Code Documentation

Each refactored function will have:
```r
#' Function description
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from environment
#' 2. Processes data in function environment
#' 3. Binds results back to environment
#'
#' @param env_obj Table or layer environment
#' @return The environment object (invisibly)
#' @noRd
function_name <- function(env_obj) {
  # Implementation
}
```

### Internal Documentation

Create `refactoring-notes.md` documenting:
- Pattern explanation
- Examples of refactored functions
- Common pitfalls
- Testing approach

### NEWS.md Entry

```markdown
## Internal Changes

* Refactored internal functions to eliminate `evalq()` usage and adopt
  Extract-Process-Bind pattern. This improves code clarity and testability
  without affecting user-facing functionality. (#issue-number)
```

## Success Criteria

The refactoring will be considered successful when:

1. ✓ Zero uses of `evalq()` for multi-line code blocks
2. ✓ All functions follow Extract-Process-Bind pattern
3. ✓ All existing tests pass
4. ✓ No new test failures
5. ✓ R CMD check passes
6. ✓ Performance within 10% of baseline
7. ✓ Code review approved
8. ✓ Documentation complete
9. ✓ No environment pollution (verified by tests)
10. ✓ Improved code clarity (verified by review)

## Risks and Mitigation

### Risk 1: Breaking Functionality
**Likelihood:** Medium  
**Impact:** High  
**Mitigation:**
- Comprehensive testing at each step
- Incremental changes
- Easy rollback via Git

### Risk 2: Performance Degradation
**Likelihood:** Low  
**Impact:** Medium  
**Mitigation:**
- Benchmark before/after
- Profile if issues arise
- Optimize if needed

### Risk 3: Introducing Bugs
**Likelihood:** Medium  
**Impact:** High  
**Mitigation:**
- Thorough code review
- Extensive testing
- Incremental rollout

### Risk 4: Incomplete Refactoring
**Likelihood:** Low  
**Impact:** Medium  
**Mitigation:**
- Comprehensive search for `evalq()`
- Checklist of all functions
- Code review verification

### Risk 5: Test Suite Inadequacy
**Likelihood:** Low  
**Impact:** High  
**Mitigation:**
- Review test coverage before starting
- Add tests where gaps exist
- Use snapshot tests for output verification

## Conclusion

This refactoring will significantly improve Tplyr's internal code quality by:
- Eliminating complex `evalq()` usage
- Adopting clear Extract-Process-Bind pattern
- Preventing environment pollution
- Improving testability
- Maintaining complete backward compatibility

The incremental migration strategy ensures safety, and the comprehensive testing approach ensures correctness. The result will be a more maintainable, understandable, and reliable codebase.
