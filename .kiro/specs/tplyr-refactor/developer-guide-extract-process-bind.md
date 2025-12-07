# Developer Guide: Extract-Process-Bind Pattern in Tplyr

## Overview

This guide explains the Extract-Process-Bind (EPB) pattern used throughout Tplyr's internal functions. This pattern replaced the previous `evalq()`-based approach to improve code clarity, testability, and maintainability.

## Table of Contents

1. [What is Extract-Process-Bind?](#what-is-extract-process-bind)
2. [Why We Refactored](#why-we-refactored)
3. [The Pattern in Detail](#the-pattern-in-detail)
4. [Implementation Examples](#implementation-examples)
5. [Testing EPB Functions](#testing-epb-functions)
6. [Common Patterns](#common-patterns)
7. [Troubleshooting](#troubleshooting)

## What is Extract-Process-Bind?

Extract-Process-Bind is a functional programming pattern that makes environment manipulation explicit and predictable. Instead of executing entire function bodies within table/layer environments using `evalq()`, functions now:

1. **EXTRACT**: Explicitly read needed values from the environment
2. **PROCESS**: Perform calculations in the function's own environment
3. **BIND**: Explicitly write results back to the environment

### Before (using evalq)

```r
treatment_group_build <- function(table) {
  output <- evalq({
    # Entire function body executes in table environment
    built_target <- clean_attr(target)  # Creates binding in table env
    fct_levels <- unique(...)            # Creates binding in table env
    # ... more code ...
    rm(grp_i, i, fct_levels)            # Manual cleanup required
  }, envir=table)
  invisible(table)
}
```

**Problems:**
- Temporary variables pollute table environment
- Unclear what's being read vs written
- Manual cleanup required and often forgotten
- Difficult to test in isolation
- Side effects can impact subsequent functions

### After (Extract-Process-Bind)

```r
treatment_group_build <- function(table) {
  # EXTRACT: Explicitly get what we need from environment
  target <- table$target
  treat_var <- table$treat_var
  pop_data <- table$pop_data
  
  # PROCESS: Work in function environment (no side effects)
  built_target <- clean_attr(target)
  fct_levels <- unique(...)  # Local variable, not in table env
  # ... more processing ...
  
  # BIND: Explicitly write results back
  table$built_target <- built_target
  table$built_pop_data <- built_pop_data
  
  invisible(table)
}
```

**Benefits:**
- Clear what's being read (extract section)
- Clear what's being written (bind section)
- No temporary variables in table environment
- Easy to test (can mock extracted values)
- Function body runs in function environment
- No manual cleanup needed

## Why We Refactored

The refactoring was driven by several key issues with the `evalq()` approach:

### 1. Environment Pollution

Temporary variables created during processing would remain in table/layer environments:

```r
# After evalq-based function
ls(table)
# [1] "built_target"    "built_pop_data"  "target"         
# [4] "treat_var"       "fct_levels"      "grp_i"          
# [7] "i"              # <- Unintended pollution!
```

### 2. Unclear Data Flow

It was difficult to determine what a function read from and wrote to the environment:

```r
# What does this function modify?
process_summaries.count_layer <- function(x, ...) {
  evalq({
    # 200 lines of code...
    # What gets modified? Hard to tell!
  }, envir=x)
}
```

### 3. Testing Challenges

Testing required full table/layer setup, making unit tests cumbersome:

```r
# Hard to test specific logic
test_that("treatment_group_build works", {
  # Need full table setup
  table <- tplyr_table(data, treat) %>%
    add_layer(...) %>%
    # ... more setup
  
  treatment_group_build(table)
  # Can only test end result, not intermediate steps
})
```

### 4. Debugging Difficulty

When errors occurred inside `evalq()`, stack traces were confusing and variables were hard to inspect.

## The Pattern in Detail

### Extract Phase

The extract phase explicitly retrieves all needed values from the environment:

```r
# EXTRACT: Get what we need from environment
target <- table$target
treat_var <- table$treat_var
pop_data <- table$pop_data
pop_treat_var <- table$pop_treat_var
table_where <- table$table_where
pop_where <- table$pop_where
treat_grps <- table$treat_grps
cols <- table$cols
```

**Key Points:**
- Use `$` accessor for direct environment access
- Use `env_get()` for inherited values: `env_get(x, "built_target", inherit = TRUE)`
- Use `env_get()` with defaults: `env_get(x, "include_total_row", default = FALSE)`
- Extract ALL needed values at the start
- Group related extractions together

### Process Phase

The process phase performs all calculations in the function's own environment:

```r
# PROCESS: Work in function environment (no side effects)
built_target <- clean_attr(target)

if (!is.factor(target[[as_name(treat_var)]])) {
  built_target <- built_target %>%
    mutate(!!treat_var := factor(!!treat_var))
}

# Local variables stay local
fct_levels <- unique(c(
  levels(built_pop_data[[as_name(pop_treat_var)]]),
  levels(built_target[[as_name(treat_var)]]),
  names(treat_grps)
))

# More processing...
```

**Key Points:**
- All variables are local to the function
- No side effects on table/layer environment
- Temporary variables don't need cleanup
- Easy to debug with standard R tools
- Can use standard control flow

### Bind Phase

The bind phase explicitly writes results back to the environment:

```r
# BIND: Explicitly write results back
table$built_target <- built_target
table$built_pop_data <- built_pop_data

invisible(table)
```

**Key Points:**
- Only write what needs to persist
- Use `$` accessor for direct binding
- Use `env_bind()` for multiple bindings: `env_bind(x, var1 = val1, var2 = val2)`
- Return invisibly to maintain method chaining
- Document what gets bound in function documentation

## Implementation Examples

### Example 1: Simple Environment Modifier

```r
#' Build header N values
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from table environment
#' 2. Processes data in function environment
#' 3. Binds results back to table environment
#'
#' @param table A tplyr_table object
#' @return The table invisibly
#' @noRd
build_header_n <- function(table) {
  # EXTRACT
  built_pop_data <- table$built_pop_data
  pop_treat_var <- table$pop_treat_var
  cols <- table$cols
  
  # PROCESS
  header_n <- built_pop_data %>%
    group_by(!!pop_treat_var, !!!cols) %>%
    summarize(n = n(), .groups = "drop")
  
  # BIND
  table$header_n <- header_n
  
  invisible(table)
}
```

### Example 2: Layer Processing with Inheritance

```r
#' Process summaries for count layer
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes data in function environment
#' 3. Binds results back to layer environment
#'
#' @param x A count_layer object
#' @return The layer invisibly
#' @noRd
process_summaries.count_layer <- function(x, ...) {
  # EXTRACT: Get needed bindings (with inheritance from parent)
  built_target <- env_get(x, "built_target", inherit = TRUE)
  target_var <- env_get(x, "target_var", inherit = TRUE)
  where <- env_get(x, "where", inherit = TRUE)
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  by <- env_get(x, "by")
  cols <- env_get(x, "cols", inherit = TRUE)
  
  # PROCESS: Calculate in function environment
  built_target <- built_target %>%
    filter(!!where)
  
  numeric_data <- built_target %>%
    group_by(!!treat_var, !!!by, !!!target_var) %>%
    summarize(n = n(), .groups = "drop")
  
  # BIND: Write results back
  x$built_target <- built_target
  x$numeric_data <- numeric_data
  
  invisible(x)
}
```

### Example 3: Helper Function with Error Handling

```r
#' Process count N values
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes data in function environment
#' 3. Binds results back to layer environment
#'
#' @param x A count_layer object
#' @return The layer invisibly
#' @noRd
process_count_n <- function(x) {
  # EXTRACT
  built_target <- env_get(x, "built_target")
  target_var <- env_get(x, "target_var")
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  by <- env_get(x, "by")
  distinct_by <- env_get(x, "distinct_by", default = NULL)
  
  # PROCESS with error handling
  tryCatch({
    summary_stat <- built_target %>%
      group_by(!!treat_var, !!!by, !!!target_var) %>%
      summarize(
        n = n(),
        distinct_n = n_distinct(!!!distinct_by, !!treat_var, !!!target_var)
      ) %>%
      ungroup()
  }, error = function(e) {
    abort(paste0("Error in process_count_n: ", e))
  })
  
  # BIND
  x$summary_stat <- summary_stat
  
  invisible(x)
}
```

## Testing EPB Functions

The EPB pattern makes testing much easier:

### Unit Testing

```r
test_that("treatment_group_build creates correct bindings", {
  # Setup
  table <- tplyr_table(mtcars, gear)
  
  # Execute
  treatment_group_build(table)
  
  # Verify outputs exist
  expect_true(!is.null(table$built_target))
  expect_true(!is.null(table$built_pop_data))
  
  # Verify no pollution
  expect_false(exists("fct_levels", envir = table))
  expect_false(exists("grp_i", envir = table))
  expect_false(exists("i", envir = table))
})
```

### Testing with Mock Data

```r
test_that("process_count_n calculates correctly", {
  # Create minimal layer
  layer <- new.env()
  layer$built_target <- data.frame(
    gear = c(3, 3, 4, 4, 5),
    cyl = c(8, 8, 6, 6, 8)
  )
  layer$target_var <- quos(cyl)
  layer$treat_var <- quo(gear)
  layer$by <- quos()
  
  # Execute
  process_count_n(layer)
  
  # Verify
  expect_equal(nrow(layer$summary_stat), 3)
  expect_true("n" %in% names(layer$summary_stat))
})
```

### Integration Testing

```r
test_that("full table build works end-to-end", {
  # Full integration test
  result <- tplyr_table(mtcars, gear) %>%
    add_layer(
      group_count(cyl)
    ) %>%
    build()
  
  # Verify output structure
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})
```

## Common Patterns

### Pattern 1: Conditional Extraction

```r
# Extract with defaults
include_total_row <- env_get(x, "include_total_row", default = FALSE)

# Extract with conditional logic
if (need_prec_table) {
  precision_by <- x$precision_by
  precision_on <- x$precision_on
}
```

### Pattern 2: Inherited Values

```r
# Get from layer, but inherit from parent table if not found
built_target <- env_get(x, "built_target", inherit = TRUE)
treat_var <- env_get(x, "treat_var", inherit = TRUE)
cols <- env_get(x, "cols", inherit = TRUE)
```

### Pattern 3: Multiple Bindings

```r
# Bind multiple values at once
env_bind(x,
  numeric_data = numeric_data,
  formatted_data = formatted_data,
  metadata = metadata
)
```

### Pattern 4: Calling Helper Functions

```r
# Main function delegates to helpers
process_summaries.count_layer <- function(x, ...) {
  # EXTRACT
  # ... extract phase ...
  
  # PROCESS
  # ... initial processing ...
  
  # BIND
  x$built_target <- built_target
  
  # Call helpers that also follow EPB
  process_count_n(x)
  process_count_denoms(x)
  
  invisible(x)
}
```

### Pattern 5: Error Handling

```r
# Wrap risky operations in tryCatch
tryCatch({
  built_target <- built_target %>%
    filter(!!where)
}, error = function(e) {
  abort(paste0("Filter condition `",
               as_label(where),
               "` is invalid. Error: ", e))
})
```

## Troubleshooting

### Issue: "Object not found" errors

**Symptom:** Error like `object 'target' not found`

**Cause:** Forgot to extract a needed binding

**Solution:** Add the extraction in the EXTRACT phase:
```r
# Add this
target <- table$target
```

### Issue: Unexpected values in environment

**Symptom:** Variables appear in table/layer that shouldn't be there

**Cause:** Accidentally binding to environment instead of using local variable

**Solution:** Check BIND phase - only bind what should persist:
```r
# WRONG - binds temporary variable
x$temp_var <- temp_var

# RIGHT - only bind results
x$result <- result
# temp_var stays local
```

### Issue: Tests fail with "inherit = TRUE"

**Symptom:** `env_get()` with `inherit = TRUE` doesn't find value

**Cause:** Parent environment not set up correctly in test

**Solution:** Ensure parent-child relationship in test:
```r
# Create parent table
table <- tplyr_table(data, treat)

# Layer will inherit from table
layer <- group_count(var)
# Set parent explicitly if needed
env_parent(layer) <- table
```

### Issue: Function modifies wrong environment

**Symptom:** Changes appear in unexpected places

**Cause:** Using wrong environment reference

**Solution:** Always use the parameter name:
```r
# WRONG - might modify wrong environment
env_bind(parent.env(x), result = result)

# RIGHT - modify the passed environment
x$result <- result
```

## Best Practices

1. **Always document the pattern**: Include the EPB comment in roxygen2 documentation
2. **Group extractions logically**: Related values together, inherited values together
3. **Use meaningful variable names**: Make it clear what each extracted value represents
4. **Comment each phase**: Mark EXTRACT, PROCESS, and BIND sections clearly
5. **Test for no pollution**: Always verify temporary variables don't leak
6. **Handle errors explicitly**: Use tryCatch with clear error messages
7. **Return invisibly**: Maintain method chaining by returning invisibly
8. **Document bindings**: Note what gets bound back in function documentation

## Migration Checklist

When refactoring a function to EPB:

- [ ] Identify all environment reads (what does the function need?)
- [ ] Identify all environment writes (what does the function modify?)
- [ ] Identify temporary variables (what should stay local?)
- [ ] Add EXTRACT phase with all needed bindings
- [ ] Move processing logic to PROCESS phase
- [ ] Add BIND phase with only persistent results
- [ ] Remove `evalq()` wrapper
- [ ] Update roxygen2 documentation
- [ ] Add tests for no environment pollution
- [ ] Verify all existing tests still pass
- [ ] Check for any manual cleanup code (rm()) and remove it

## Additional Resources

- **Requirements Document**: `.kiro/specs/tplyr-refactor/requirements.md`
- **Design Document**: `.kiro/specs/tplyr-refactor/design.md`
- **Code Quality Review**: `.kiro/specs/tplyr-refactor/code-quality-review.md`
- **Performance Validation**: `.kiro/specs/tplyr-refactor/performance-validation-report.md`

## Conclusion

The Extract-Process-Bind pattern has significantly improved Tplyr's internal code quality by:

- Eliminating environment pollution
- Making data flow explicit and clear
- Improving testability
- Simplifying debugging
- Maintaining complete backward compatibility

When writing new internal functions or refactoring existing ones, always follow this pattern to maintain consistency and code quality throughout the package.
