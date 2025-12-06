# Requirements Document: Refactor evalq() Pattern to Functional Design

## Introduction

This document specifies the requirements for refactoring Tplyr's use of `evalq()` to execute code within table/layer environments. The current pattern has proven problematic due to code complexity, unintended side effects, and difficulty in testing and debugging. The refactoring will adopt a standard functional programming approach where functions accept parameters, execute in their own environment, and explicitly return or modify only necessary bindings.

## Glossary

- **evalq()**: R function that evaluates an expression in a specified environment
- **Environment**: R's mechanism for storing variable bindings and managing scope
- **Table Environment**: The environment object that represents a `tplyr_table`
- **Layer Environment**: The environment object that represents a `tplyr_layer`
- **Binding**: A name-value pair stored in an environment
- **Side Effect**: Unintended modification of state outside a function's explicit return value
- **Functional Design**: Programming pattern where functions are pure, accepting inputs and returning outputs without modifying external state
- **Built Target**: The processed version of the target dataset after filters and transformations
- **Treatment Variable**: Variable distinguishing treatment groups in clinical data
- **Extract-Process-Bind Pattern**: The refactoring pattern where functions (1) extract needed bindings from an environment, (2) process data in the function's own environment, and (3) explicitly bind results back to the environment

## Refactoring Pattern

The refactoring will follow the **Extract-Process-Bind** pattern:

### Current Pattern (Using evalq)
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
- Entire function body runs in table environment
- Temporary variables pollute table environment
- Manual cleanup required
- Difficult to test
- Unclear what's being read vs written

### New Pattern (Extract-Process-Bind)
```r
treatment_group_build <- function(table) {
  # EXTRACT: Explicitly get what we need from environment
  target <- table$target
  treat_var <- table$treat_var
  pop_data <- table$pop_data
  pop_treat_var <- table$pop_treat_var
  table_where <- table$table_where
  pop_where <- table$pop_where
  treat_grps <- table$treat_grps
  cols <- table$cols
  
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

## Requirements

### Requirement 1: Eliminate evalq() Usage

**User Story:** As a Tplyr maintainer, I want to eliminate `evalq()` usage from the codebase, so that code is easier to understand, test, and maintain.

#### Acceptance Criteria

1. WHEN refactoring is complete THEN the system SHALL contain zero uses of `evalq()` for executing multi-line code blocks in table/layer environments
2. WHEN refactoring is complete THEN the system SHALL use standard function calls with explicit parameters instead of `evalq()`
3. WHEN a function previously using `evalq()` is refactored THEN the system SHALL preserve all existing functionality
4. WHEN a function previously using `evalq()` is refactored THEN the system SHALL maintain the same inputs and outputs from a user perspective
5. WHEN code is executed THEN the system SHALL NOT create unintended variable bindings in table/layer environments

### Requirement 2: Adopt Functional Design Pattern

**User Story:** As a Tplyr developer, I want functions to follow functional programming principles, so that code behavior is predictable and testable.

#### Acceptance Criteria

1. WHEN a refactored function executes THEN the system SHALL accept the table or layer environment as a parameter
2. WHEN a refactored function begins execution THEN the system SHALL explicitly extract needed bindings from the environment (e.g., `target <- table$target`)
3. WHEN a refactored function processes data THEN the system SHALL execute within the function's own environment, not within the table/layer environment
4. WHEN a refactored function completes THEN the system SHALL explicitly bind results back to the environment (e.g., `table$built_target <- built_target`)
5. WHEN a refactored function executes THEN the system SHALL NOT use `evalq()` to wrap the entire function body
6. WHEN reading refactored code THEN a developer SHALL clearly see what is being read from the environment and what is being written back

### Requirement 3: Refactor treatment_group_build()

**User Story:** As a Tplyr user, I want treatment group building to work correctly without side effects, so that my tables build reliably.

#### Acceptance Criteria

1. WHEN `treatment_group_build()` is called THEN the system SHALL accept the table environment as a parameter
2. WHEN `treatment_group_build()` begins execution THEN the system SHALL explicitly extract necessary bindings from the table environment using `table$target`, `table$treat_var`, `table$pop_data`, etc.
3. WHEN `treatment_group_build()` processes data THEN the system SHALL create `built_target` and `built_pop_data` as local variables in the function environment
4. WHEN `treatment_group_build()` completes THEN the system SHALL explicitly bind results using `table$built_target <- built_target` and `table$built_pop_data <- built_pop_data`
5. WHEN `treatment_group_build()` completes THEN the system SHALL NOT leave temporary variables (grp_i, i, fct_levels) in the table environment
6. WHEN `treatment_group_build()` is called THEN the system SHALL preserve all existing functionality including factor handling, filtering, and treatment group expansion
7. WHEN `treatment_group_build()` encounters filter errors THEN the system SHALL report errors with the same clarity as the current implementation
8. WHEN `treatment_group_build()` executes THEN the system SHALL NOT use `evalq()` to wrap the function body

### Requirement 4: Refactor Layer Processing Functions

**User Story:** As a Tplyr developer, I want layer processing functions to be modular and testable, so that I can maintain and extend them easily.

#### Acceptance Criteria

1. WHEN `process_summaries()` methods are refactored THEN the system SHALL accept the layer environment as a parameter
2. WHEN `process_summaries()` methods begin execution THEN the system SHALL explicitly extract needed bindings from the layer environment (e.g., `target_var <- layer$target_var`)
3. WHEN `process_summaries()` methods execute THEN the system SHALL perform calculations in the function environment
4. WHEN `process_summaries()` methods complete THEN the system SHALL explicitly bind results to the layer environment (e.g., `layer$numeric_data <- numeric_data`)
5. WHEN `process_formatting()` methods are refactored THEN the system SHALL follow the same pattern of extract-process-bind
6. WHEN `process_metadata()` methods are refactored THEN the system SHALL follow the same pattern of extract-process-bind
7. WHEN any layer processing function is refactored THEN the system SHALL maintain S3 dispatch pattern
8. WHEN any layer processing function is refactored THEN the system SHALL preserve all existing functionality
9. WHEN any layer processing function executes THEN the system SHALL NOT use `evalq()` to wrap the function body

### Requirement 5: Refactor Sorting Functions

**User Story:** As a Tplyr user, I want sorting to work correctly, so that my table rows appear in the expected order.

#### Acceptance Criteria

1. WHEN `add_order_columns()` methods are refactored THEN the system SHALL accept the layer environment as a parameter
2. WHEN `add_order_columns()` begins execution THEN the system SHALL explicitly extract needed bindings from the layer environment
3. WHEN `add_order_columns()` executes THEN the system SHALL process data in the function environment
4. WHEN `add_order_columns()` completes THEN the system SHALL explicitly bind modified formatted_data back to the layer environment
5. WHEN `get_data_order()` is refactored THEN the system SHALL accept the layer environment as a parameter
6. WHEN `get_data_order()` executes THEN the system SHALL extract needed bindings and return ordering data without side effects
7. WHEN sorting functions are refactored THEN the system SHALL preserve all sorting methods (bycount, byfactor, byvarn)
8. WHEN sorting functions execute THEN the system SHALL NOT use `evalq()` to wrap the function body

### Requirement 6: Refactor Count Layer Functions

**User Story:** As a Tplyr user, I want count layers to calculate frequencies correctly, so that my summary tables are accurate.

#### Acceptance Criteria

1. WHEN `process_single_count_target()` is refactored THEN the system SHALL accept the layer environment as a parameter
2. WHEN `process_single_count_target()` begins execution THEN the system SHALL explicitly extract needed bindings from the layer environment
3. WHEN `process_single_count_target()` executes THEN the system SHALL calculate numeric_data in the function environment
4. WHEN `process_single_count_target()` completes THEN the system SHALL explicitly bind results back to the layer environment
5. WHEN `process_count_n()` is refactored THEN the system SHALL follow the extract-process-bind pattern
6. WHEN `process_count_total_row()` is refactored THEN the system SHALL follow the extract-process-bind pattern
7. WHEN `process_missing_subjects_row()` is refactored THEN the system SHALL follow the extract-process-bind pattern
8. WHEN `process_count_denoms()` is refactored THEN the system SHALL follow the extract-process-bind pattern
9. WHEN count layer functions are refactored THEN the system SHALL preserve distinct counting, nested counting, and all denominator options
10. WHEN count layer functions execute THEN the system SHALL NOT use `evalq()` to wrap the function body

### Requirement 7: Refactor Desc Layer Functions

**User Story:** As a Tplyr user, I want descriptive statistics layers to calculate summaries correctly, so that my continuous variable summaries are accurate.

#### Acceptance Criteria

1. WHEN desc layer `process_summaries()` is refactored THEN the system SHALL accept the layer environment as a parameter
2. WHEN desc layer `process_summaries()` begins execution THEN the system SHALL explicitly extract needed bindings from the layer environment
3. WHEN desc layer `process_summaries()` executes THEN the system SHALL calculate statistics in the function environment
4. WHEN desc layer `process_summaries()` completes THEN the system SHALL explicitly bind trans_sums back to the layer environment
5. WHEN desc layer `process_formatting()` is refactored THEN the system SHALL follow the extract-process-bind pattern
6. WHEN desc layer functions are refactored THEN the system SHALL preserve all built-in statistics
7. WHEN desc layer functions are refactored THEN the system SHALL preserve custom summary functionality
8. WHEN desc layer functions execute THEN the system SHALL NOT use `evalq()` to wrap the function body

### Requirement 8: Refactor Shift Layer Functions

**User Story:** As a Tplyr user, I want shift layers to calculate state changes correctly, so that my shift tables are accurate.

#### Acceptance Criteria

1. WHEN shift layer `process_summaries()` is refactored THEN the system SHALL accept the layer environment as a parameter
2. WHEN shift layer `process_summaries()` begins execution THEN the system SHALL explicitly extract needed bindings from the layer environment
3. WHEN shift layer `process_summaries()` executes THEN the system SHALL calculate shift counts in the function environment
4. WHEN shift layer `process_summaries()` completes THEN the system SHALL explicitly bind results back to the layer environment
5. WHEN `process_shift_n()` is refactored THEN the system SHALL follow the extract-process-bind pattern
6. WHEN `process_shift_total()` is refactored THEN the system SHALL follow the extract-process-bind pattern
7. WHEN `process_shift_denoms()` is refactored THEN the system SHALL follow the extract-process-bind pattern
8. WHEN shift layer functions are refactored THEN the system SHALL preserve row/column matrix structure
9. WHEN shift layer functions execute THEN the system SHALL NOT use `evalq()` to wrap the function body

### Requirement 9: Refactor Metadata Functions

**User Story:** As a Tplyr user, I want metadata generation to work correctly, so that I can trace results back to source data.

#### Acceptance Criteria

1. WHEN `process_metadata()` methods are refactored THEN the system SHALL accept the layer environment as a parameter
2. WHEN `process_metadata()` methods begin execution THEN the system SHALL explicitly extract needed bindings from the layer environment
3. WHEN `process_metadata()` methods execute THEN the system SHALL generate metadata in the function environment
4. WHEN `process_metadata()` methods complete THEN the system SHALL explicitly bind metadata results back to the layer environment
5. WHEN metadata functions are refactored THEN the system SHALL preserve all traceability information
6. WHEN metadata functions are refactored THEN the system SHALL maintain metadata structure and format
7. WHEN metadata functions execute THEN the system SHALL NOT use `evalq()` to wrap the function body

### Requirement 10: Refactor Helper Functions

**User Story:** As a Tplyr developer, I want helper functions to be pure and testable, so that I can rely on their behavior.

#### Acceptance Criteria

1. WHEN `build_header_n()` is refactored THEN the system SHALL accept the table environment as a parameter
2. WHEN `build_header_n()` begins execution THEN the system SHALL explicitly extract needed bindings from the table environment
3. WHEN `build_header_n()` executes THEN the system SHALL calculate header N values in the function environment
4. WHEN `build_header_n()` completes THEN the system SHALL explicitly bind results back to the table environment
5. WHEN `factor_treat_var()` is refactored THEN the system SHALL follow the extract-process-bind pattern
6. WHEN `rename_missing_values()` is refactored THEN the system SHALL follow the extract-process-bind pattern
7. WHEN helper functions are refactored THEN the system SHALL NOT modify environment state except through explicit binding
8. WHEN helper functions execute THEN the system SHALL NOT use `evalq()` to wrap the function body

### Requirement 11: Maintain Backward Compatibility

**User Story:** As a Tplyr user, I want my existing code to continue working after the refactoring, so that I don't have to rewrite my analyses.

#### Acceptance Criteria

1. WHEN refactoring is complete THEN the system SHALL maintain all user-facing APIs unchanged
2. WHEN a user builds a table THEN the system SHALL produce identical output to the pre-refactoring version
3. WHEN a user accesses layer data THEN the system SHALL provide the same bindings as before (numeric_data, built_table, etc.)
4. WHEN a user uses metadata features THEN the system SHALL produce identical metadata structure
5. WHEN a user runs existing code THEN the system SHALL NOT break due to refactoring changes

### Requirement 12: Improve Testability

**User Story:** As a Tplyr developer, I want refactored functions to be easily testable, so that I can ensure correctness and prevent regressions.

#### Acceptance Criteria

1. WHEN a function is refactored THEN the system SHALL allow unit testing without requiring full table/layer setup
2. WHEN a function is refactored THEN the system SHALL have clear inputs and outputs that can be verified
3. WHEN a function is refactored THEN the system SHALL NOT require mocking of environment state for testing
4. WHEN a function is refactored THEN the system SHALL allow testing of edge cases in isolation
5. WHEN refactoring is complete THEN the system SHALL maintain or improve test coverage

### Requirement 13: Improve Code Clarity

**User Story:** As a Tplyr developer, I want code to be clear and understandable, so that I can maintain and extend it efficiently.

#### Acceptance Criteria

1. WHEN a function is refactored THEN the system SHALL have a clear function signature with documented parameters
2. WHEN a function is refactored THEN the system SHALL have roxygen2 documentation explaining inputs, outputs, and behavior
3. WHEN a function executes THEN the system SHALL have clear data flow from inputs to outputs
4. WHEN reading refactored code THEN a developer SHALL understand what data is being accessed and modified
5. WHEN debugging refactored code THEN a developer SHALL be able to inspect function-local variables easily

### Requirement 14: Eliminate Unintended Side Effects

**User Story:** As a Tplyr developer, I want to eliminate unintended side effects, so that functions behave predictably.

#### Acceptance Criteria

1. WHEN a function executes THEN the system SHALL NOT create temporary variables in table/layer environments
2. WHEN a function executes THEN the system SHALL NOT modify variables that should remain unchanged
3. WHEN a function completes THEN the system SHALL leave table/layer environment in a predictable state
4. WHEN multiple functions execute sequentially THEN the system SHALL NOT have one function's side effects impact another
5. WHEN a function is called multiple times THEN the system SHALL produce consistent results without state pollution

### Requirement 15: Maintain Performance

**User Story:** As a Tplyr user, I want table building to remain fast after refactoring, so that my workflows are not slowed down.

#### Acceptance Criteria

1. WHEN refactoring is complete THEN the system SHALL maintain similar performance to pre-refactoring version
2. WHEN a table is built THEN the system SHALL complete in comparable time to the current implementation
3. WHEN performance is measured THEN the system SHALL NOT introduce significant overhead from function calls
4. WHEN large tables are built THEN the system SHALL handle them efficiently
5. WHEN performance degrades THEN the degradation SHALL be less than 10% and documented

### Requirement 16: Provide Clear Migration Path

**User Story:** As a Tplyr maintainer, I want a clear migration path for the refactoring, so that I can implement changes safely.

#### Acceptance Criteria

1. WHEN refactoring begins THEN the system SHALL have a documented plan for incremental changes
2. WHEN each function is refactored THEN the system SHALL maintain passing tests
3. WHEN refactoring is in progress THEN the system SHALL allow for parallel implementation if needed
4. WHEN refactoring is complete THEN the system SHALL have documentation of all changes made
5. WHEN issues arise THEN the system SHALL allow for easy rollback of individual changes

## Non-Functional Requirements

### NFR-1: Code Quality
- All refactored code SHALL follow tidyverse style guide
- All refactored code SHALL pass R CMD check
- All refactored code SHALL have roxygen2 documentation

### NFR-2: Testing
- All refactored code SHALL maintain or improve test coverage
- All refactored code SHALL pass existing test suite
- New tests SHALL be added for refactored functions where appropriate

### NFR-3: Documentation
- All refactored functions SHALL have updated documentation
- Changes SHALL be documented in NEWS.md
- Internal documentation SHALL explain the new pattern

### NFR-4: Compatibility
- Refactoring SHALL NOT break CRAN compliance
- Refactoring SHALL NOT break UAT test suite
- Refactoring SHALL NOT require users to change their code

## Success Criteria

The refactoring will be considered successful when:

1. Zero uses of `evalq()` for multi-line code blocks remain in the codebase
2. All functions follow functional design pattern with explicit parameters and returns
3. All existing tests pass without modification
4. Test coverage is maintained or improved
5. R CMD check passes with no errors, warnings, or notes
6. Performance is within 10% of pre-refactoring baseline
7. Code review confirms improved clarity and maintainability
8. Documentation is complete and accurate

## Out of Scope

The following are explicitly out of scope for this refactoring:

1. Changing user-facing APIs
2. Adding new features
3. Changing output format or structure
4. Modifying the environment-based object model
5. Changing S3 dispatch patterns
6. Refactoring code that doesn't use `evalq()`
7. Performance optimizations beyond maintaining current performance

## Risks and Mitigation

### Risk 1: Breaking Existing Functionality
**Mitigation**: Comprehensive testing at each step, incremental changes, maintain test coverage

### Risk 2: Performance Degradation
**Mitigation**: Benchmark before and after, profile if issues arise, optimize if needed

### Risk 3: Introducing New Bugs
**Mitigation**: Thorough code review, extensive testing, incremental rollout

### Risk 4: Scope Creep
**Mitigation**: Strict adherence to requirements, clear definition of out-of-scope items

### Risk 5: Difficulty in Testing
**Mitigation**: Write tests for refactored functions, use test-driven refactoring approach

## Dependencies

This refactoring depends on:

1. Existing test suite being comprehensive and passing
2. Understanding of current `evalq()` usage patterns
3. Understanding of environment-based architecture
4. Access to performance benchmarking tools

## Assumptions

1. Current functionality is correct and should be preserved
2. Test suite adequately covers existing functionality
3. Performance of current implementation is acceptable
4. Environment-based object model will be maintained

## Constraints

1. Must maintain backward compatibility
2. Must not break CRAN compliance
3. Must maintain UAT qualification
4. Must complete refactoring in manageable increments
5. Must maintain or improve code quality metrics
