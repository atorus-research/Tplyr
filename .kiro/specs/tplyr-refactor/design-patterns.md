# Tplyr Design Patterns & Architecture Guide

## Purpose

This document explains the key design patterns used in Tplyr. Understanding these patterns is essential for effective refactoring while maintaining the package's architecture and user experience.

## Core Design Patterns

### 1. Environment-Based Object-Oriented Programming

#### Pattern Description
Tplyr uses R environments as objects rather than traditional S3/S4 classes or lists.

#### Why This Pattern?
- **Mutable State**: Environments allow in-place modification without copying
- **Reference Semantics**: Multiple references point to same object
- **Parent-Child Relationships**: Natural hierarchy through environment parents
- **Lazy Evaluation**: Can store unevaluated expressions (quosures)

#### Implementation in Tplyr

```r
# Table creation
new_tplyr_table <- function(target, treat_var, where, cols, target_name) {
  # Create environment with bindings
  table_ <- structure(rlang::env(
    target = target,
    treat_grps = list(),
    cols = cols,
    layers = structure(list(), class = c("tplyr_layer_container", "list"))
  ), class = c("tplyr_table", "environment"))
  
  # Environment is the object
  table_
}

# Layer creation with parent reference
new_tplyr_layer <- function(parent, target_var, by, where, type, ...) {
  # Create environment as child of parent
  e <- env(parent, 
           target_var = target_var,
           by = by,
           where = where,
           ...)
  
  structure(e, class = c('tplyr_layer', paste0(type,'_layer'), class(e)))
}
```

#### Key Characteristics
- Objects are environments with class attributes
- Parent-child relationships via environment hierarchy
- Bindings accessed via `$` or `env_get()`/`env_bind()`
- Validation happens at construction time

#### Refactoring Considerations
- Don't convert to lists without understanding implications
- Preserve parent-child relationships
- Be careful with environment copying (use `env_clone()` if needed)
- Understand that modifications are in-place

### 2. Lazy Evaluation / Delayed Execution

#### Pattern Description
Object construction and configuration are separate from data processing.

#### Why This Pattern?
- **Validation Before Execution**: Catch errors before expensive operations
- **Inspection**: Users can examine table structure before building
- **Flexibility**: Modify configuration after initial construction
- **Performance**: Don't process data until explicitly requested

#### Implementation in Tplyr

```r
# Construction phase - no data processing
tab <- tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_count(RACE) %>%
      set_format_strings(f_str('xx (xx.x%)', n, pct))
  )

# Inspection phase - examine structure
print(tab)  # Shows configuration, not results

# Execution phase - explicit trigger
results <- build(tab)
```

#### Key Characteristics
- `tplyr_table()` and layer constructors don't process data
- Configuration functions return modified object
- `build()` is the explicit execution trigger
- Environments store configuration until build time

#### Refactoring Considerations
- Never process data in constructors or setters
- Keep `build()` as the single execution entry point
- Maintain clear separation between configuration and execution
- Test that construction is fast (< 1ms)

### 3. S3 Method Dispatch for Polymorphism

#### Pattern Description
Different layer types have different processing logic, implemented via S3 generics.

#### Why This Pattern?
- **Extensibility**: Easy to add new layer types
- **Separation of Concerns**: Each layer type has its own implementation
- **R Idioms**: Follows R's object system conventions

#### Implementation in Tplyr

```r
# Generic functions
process_summaries <- function(x, ...) {
  UseMethod("process_summaries")
}

process_formatting <- function(x, ...) {
  UseMethod("process_formatting")
}

process_metadata <- function(x, ...) {
  UseMethod("process_metadata")
}

# Implementations for each layer type
process_summaries.count_layer <- function(x, ...) {
  # Count-specific logic
}

process_summaries.desc_layer <- function(x, ...) {
  # Desc-specific logic
}

process_summaries.shift_layer <- function(x, ...) {
  # Shift-specific logic (reuses count logic)
}
```

#### Key Characteristics
- Three main generics: `process_summaries`, `process_formatting`, `process_metadata`
- Each layer type has its own methods
- Shift layers inherit from count layers
- Consistent interface across layer types

#### Refactoring Considerations
- Maintain S3 method signatures
- Don't break dispatch by changing class names
- Test all implementations of a generic
- Consider inheritance (shift extends count)

### 4. Quosure-Based Non-Standard Evaluation

#### Pattern Description
User inputs are captured as quosures for delayed evaluation in data context.

#### Why This Pattern?
- **Tidy Evaluation**: Supports both quoted and unquoted inputs
- **Context Preservation**: Captures both expression and environment
- **Flexibility**: Enables NSE in user-facing API while maintaining programmatic use

#### Implementation in Tplyr

```r
# Capture user input as quosure
tplyr_table <- function(target, treat_var, where = TRUE, cols = vars()) {
  target_name <- enexpr(target)
  new_tplyr_table(target, enquo(treat_var), enquo(where), enquos(cols), target_name)
}

# Later evaluation in data context
treat_var_name <- as_name(treat_var)  # Convert to string
treat_values <- eval_tidy(treat_var, data = target)  # Evaluate in data
```

#### Key Characteristics
- `enquo()` captures single variable
- `enquos()` captures multiple variables (from `vars()`)
- `as_name()` converts quosure to string
- `eval_tidy()` evaluates in data context
- Supports both `TRT01P` and `"TRT01P"` inputs

#### Refactoring Considerations
- Don't evaluate quosures too early
- Use `as_name()` for variable names, `eval_tidy()` for values
- Test with both quoted and unquoted inputs
- Understand quosure environment capture

### 5. Builder Pattern with Fluent Interface

#### Pattern Description
Objects are constructed through chained method calls.

#### Why This Pattern?
- **Readability**: Code reads like a specification
- **Discoverability**: IDE autocomplete helps users
- **Flexibility**: Easy to add/remove configuration steps

#### Implementation in Tplyr

```r
# Fluent interface - each function returns modified object
tplyr_table(adsl, TRT01P) %>%
  add_total_group() %>%
  add_layer(
    group_count(RACE) %>%
      set_format_strings(f_str('xx (xx.x%)', n, pct)) %>%
      add_total_row() %>%
      set_order_count_method('bycount')
  ) %>%
  add_layer(
    group_desc(AGE) %>%
      set_format_strings(
        'Mean (SD)' = f_str('xx.x (xx.xx)', mean, sd)
      )
  ) %>%
  build()
```

#### Key Characteristics
- All configuration functions return modified object
- Supports `%>%` piping
- Declarative style
- Order of operations matters for some functions

#### Refactoring Considerations
- Always return the object being modified
- Don't break the chain (no functions that return NULL)
- Maintain consistent naming (set_*, add_*, etc.)
- Document order dependencies

### 6. Format String DSL (Domain-Specific Language)

#### Pattern Description
`f_str()` creates a mini-language for specifying numeric formatting.

#### Why This Pattern?
- **Declarative**: Users specify what they want, not how to get it
- **Compact**: Single line specifies format and statistics
- **Metadata Capture**: Format string contains information for processing

#### Implementation in Tplyr

```r
# Format string captures multiple pieces of information
f_str('xx.x (xx.xx)', mean, sd)

# Parsed to extract:
# - Format: 'xx.x (xx.xx)'
# - Statistics: mean, sd
# - Integer widths: 2, 2
# - Decimal precisions: 1, 2
# - Combination: both in same string
```

#### Key Characteristics
- 'x' represents digits
- 'a' represents auto-precision
- 'X' or 'A' triggers parenthesis hugging
- Multiple statistics can be combined
- Row label comes from left side of `=` in `set_format_strings()`

#### Refactoring Considerations
- Format string parsing is complex - don't break it
- Test all format string features
- Maintain backward compatibility of syntax
- Document any new format string features

### 7. Strategy Pattern for Denominators

#### Pattern Description
Different denominator calculation strategies are encapsulated and swappable.

#### Why This Pattern?
- **Flexibility**: Different tables need different denominators
- **Encapsulation**: Complex logic is isolated
- **Testability**: Each strategy can be tested independently

#### Implementation in Tplyr

```r
# Different denominator strategies
# 1. Treatment group total (default)
# 2. Population data total
# 3. Custom grouping via set_denoms_by()
# 4. Filtered denominator via set_denom_where()

# Strategy selected based on layer configuration
calculate_denominator <- function(layer, data) {
  if (has_pop_data(layer)) {
    # Use population data strategy
  } else if (has_custom_denom(layer)) {
    # Use custom grouping strategy
  } else {
    # Use default strategy
  }
}
```

#### Key Characteristics
- Multiple denominator calculation methods
- Selection based on layer configuration
- Affects percentage calculations
- Critical for correct results

#### Refactoring Considerations
- Test all denominator strategies
- Verify percentages are correct
- Don't change default behavior
- Document denominator logic clearly

### 8. Template Method Pattern for Layer Processing

#### Pattern Description
Layer processing follows a fixed algorithm with customizable steps.

#### Why This Pattern?
- **Consistency**: All layers follow same overall process
- **Customization**: Each layer type customizes specific steps
- **Maintainability**: Algorithm is in one place

#### Implementation in Tplyr

```r
# Template in build.R
build.tplyr_table <- function(x, metadata=FALSE) {
  # 1. Pre-processing (same for all)
  treatment_group_build(x)
  x <- build_header_n(x)
  
  # 2. Layer processing (customized per layer type)
  map(x$layers, process_summaries)    # S3 dispatch
  map(x$layers, process_formatting)   # S3 dispatch
  
  # 3. Post-processing (same for all)
  output <- bind_layers(...)
  
  # 4. Optional metadata (same for all)
  if (metadata) {
    map(x$layers, process_metadata)   # S3 dispatch
  }
}
```

#### Key Characteristics
- Fixed overall algorithm
- Customizable steps via S3 dispatch
- Pre and post-processing are consistent
- Layer-specific logic is isolated

#### Refactoring Considerations
- Don't change overall algorithm without strong justification
- Maintain S3 dispatch points
- Test pre and post-processing separately
- Verify all layer types work with algorithm

## Architectural Patterns

### 1. Layered Architecture

```
┌─────────────────────────────────────┐
│ User-Facing API Layer               │
│ - tplyr_table(), group_*()          │
│ - set_*(), add_*()                  │
│ - Fluent interface                  │
└─────────────────────────────────────┘
           ↓
┌─────────────────────────────────────┐
│ Configuration Layer                 │
│ - Object construction               │
│ - Validation                        │
│ - Quosure capture                   │
└─────────────────────────────────────┘
           ↓
┌─────────────────────────────────────┐
│ Processing Layer                    │
│ - process_summaries()               │
│ - process_formatting()              │
│ - process_metadata()                │
└─────────────────────────────────────┘
           ↓
┌─────────────────────────────────────┐
│ Data Manipulation Layer             │
│ - dplyr operations                  │
│ - tidyr operations                  │
│ - String formatting                 │
└─────────────────────────────────────┘
           ↓
┌─────────────────────────────────────┐
│ Output Layer                        │
│ - Data frame assembly               │
│ - Metadata assembly                 │
│ - Column ordering                   │
└─────────────────────────────────────┘
```

### 2. Separation of Concerns

**Concern**: Data filtering
**Location**: `where` parameters, `set_where()`, `set_pop_where()`

**Concern**: Grouping
**Location**: `by` parameters, `treat_var`, `cols`

**Concern**: Calculation
**Location**: `process_summaries()` methods

**Concern**: Formatting
**Location**: `f_str()`, `num_fmt()`, `process_formatting()` methods

**Concern**: Metadata
**Location**: `process_metadata()` methods, metadata builders

**Concern**: Validation
**Location**: `validate_*()` functions, assertions

### 3. Dependency Injection

Population data is injected into table:

```r
tplyr_table(adae, TRTA) %>%
  set_pop_data(adsl) %>%           # Inject population data
  set_pop_treat_var(TRT01A) %>%    # Configure for population data
  add_layer(...)
```

This allows:
- Testing with mock data
- Flexibility in data sources
- Separation of concerns

## Anti-Patterns to Avoid

### 1. Breaking Lazy Evaluation

**Anti-Pattern**: Processing data in constructors

```r
# BAD
tplyr_table <- function(target, treat_var) {
  # Don't do this!
  results <- target %>% 
    group_by(!!treat_var) %>%
    summarize(n = n())
  
  env(target = target, results = results)
}
```

**Correct Pattern**: Store configuration, process in `build()`

```r
# GOOD
tplyr_table <- function(target, treat_var) {
  env(target = target, treat_var = enquo(treat_var))
}
```

### 2. Tight Coupling Between Layers

**Anti-Pattern**: Layers directly accessing other layers

```r
# BAD
process_summaries.count_layer <- function(x) {
  # Don't access sibling layers
  other_layer <- x$parent$layers[[2]]
  other_data <- other_layer$built_table
}
```

**Correct Pattern**: Layers are independent, combined in post-processing

### 3. Mutating User Data

**Anti-Pattern**: Modifying the target dataset

```r
# BAD
process_summaries.count_layer <- function(x) {
  x$parent$target$new_column <- ...  # Don't modify user data!
}
```

**Correct Pattern**: Work with copies, never modify original

### 4. Inconsistent S3 Methods

**Anti-Pattern**: Different signatures for same generic

```r
# BAD
process_summaries.count_layer <- function(x, ...) { }
process_summaries.desc_layer <- function(x, extra_param) { }  # Different signature!
```

**Correct Pattern**: Consistent signatures, use `...` for flexibility

### 5. Premature Quosure Evaluation

**Anti-Pattern**: Evaluating quosures at construction time

```r
# BAD
tplyr_table <- function(target, treat_var) {
  treat_values <- eval_tidy(enquo(treat_var), target)  # Too early!
  env(target = target, treat_values = treat_values)
}
```

**Correct Pattern**: Store quosure, evaluate during `build()`

## Testing Patterns

### 1. Test Construction Separately from Execution

```r
test_that("table construction works", {
  tab <- tplyr_table(iris, Species)
  expect_s3_class(tab, "tplyr_table")
  expect_true(is.environment(tab))
})

test_that("table execution works", {
  tab <- tplyr_table(iris, Species) %>%
    add_layer(group_count(Species))
  
  result <- build(tab)
  expect_s3_class(result, "data.frame")
})
```

### 2. Test Each Layer Type

```r
test_that("count layer processes correctly", {
  # Test count-specific logic
})

test_that("desc layer processes correctly", {
  # Test desc-specific logic
})

test_that("shift layer processes correctly", {
  # Test shift-specific logic
})
```

### 3. Test Edge Cases

```r
test_that("handles empty groups", {
  # Test with data that has empty treatment groups
})

test_that("handles all NA data", {
  # Test with all missing values
})

test_that("handles single group", {
  # Test with only one treatment group
})
```

## Performance Patterns

### 1. Avoid Repeated Calculations

**Pattern**: Calculate once, store in environment

```r
# Calculate header N once
build_header_n <- function(x) {
  header_n <- calculate_header_n(x)
  env_bind(x, header_n = header_n)
  x
}
```

### 2. Use Vectorized Operations

**Pattern**: Prefer dplyr/tidyr over loops

```r
# GOOD - vectorized
data %>%
  group_by(treat, by_var) %>%
  summarize(n = n())

# AVOID - loops
for (treat in treats) {
  for (by_val in by_vals) {
    # ...
  }
}
```

### 3. Lazy Metadata Generation

**Pattern**: Only generate metadata if requested

```r
build <- function(x, metadata = FALSE) {
  # Always process summaries
  map(x$layers, process_summaries)
  
  # Only process metadata if requested
  if (metadata) {
    map(x$layers, process_metadata)
  }
}
```

## Conclusion

Understanding these design patterns is crucial for effective refactoring of Tplyr. The patterns work together to create a flexible, extensible, and user-friendly API while maintaining performance and correctness. When refactoring:

1. Identify which patterns are involved in the code you're changing
2. Understand why those patterns were chosen
3. Preserve the patterns unless you have a compelling reason to change them
4. If changing a pattern, update all related code consistently
5. Test thoroughly to ensure the pattern still works as intended

Remember: These patterns exist for good reasons. Don't change them without understanding the implications.
