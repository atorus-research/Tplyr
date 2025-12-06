# Tplyr Functional Requirements Document

## Purpose

This document catalogs the existing functional requirements of the Tplyr package. These requirements must be preserved during any refactoring effort to maintain backward compatibility and user expectations.

## Core Functional Requirements

### FR-1: Table Construction

#### FR-1.1: Table Object Creation
- **Requirement**: Users must be able to create a table object with a target dataset and treatment variable
- **API**: `tplyr_table(target, treat_var, where, cols)`
- **Behavior**: 
  - Creates table environment without processing data
  - Validates target is a data.frame
  - Captures treatment variable as quosure
  - Optionally accepts global filter (`where`)
  - Optionally accepts column grouping variables (`cols`)

#### FR-1.2: Layer Addition
- **Requirement**: Users must be able to add one or more layers to a table
- **API**: `add_layer()`, `add_layers()`
- **Behavior**:
  - `add_layer()` allows inline layer construction with piping
  - `add_layers()` accepts pre-constructed layers
  - Layers are stored in order of addition
  - Layer parent is set to table

#### FR-1.3: Treatment Group Expansion
- **Requirement**: Users must be able to add derived treatment groups
- **API**: `add_treat_grps()`, `add_total_group()`
- **Behavior**:
  - `add_treat_grps()` combines existing groups into new groups
  - `add_total_group()` creates "Total" group from all subjects
  - New groups appear as additional columns in output
  - Original groups are preserved

#### FR-1.4: Population Data Configuration
- **Requirement**: Users must be able to specify separate population dataset for denominators
- **API**: `set_pop_data()`, `set_pop_treat_var()`, `set_pop_where()`
- **Behavior**:
  - Population data used for header N and denominators
  - Population treatment variable can differ from target treatment variable
  - Population filter can differ from target filter
  - Defaults to target dataset if not specified

#### FR-1.5: Table Execution
- **Requirement**: Users must explicitly trigger data processing
- **API**: `build(metadata=FALSE)`
- **Behavior**:
  - Lazy evaluation - construction doesn't process data
  - `build()` triggers all processing
  - Returns data frame with formatted results
  - Optionally generates traceability metadata
  - Preserves layer environments for inspection

### FR-2: Count Layers

#### FR-2.1: Basic Counting
- **Requirement**: Count occurrences of categorical variable values
- **API**: `group_count(target_var, by, where)`
- **Behavior**:
  - Counts by treatment group and target variable
  - Supports additional grouping via `by` parameter
  - Supports layer-specific filtering via `where`
  - Calculates n and percentage

#### FR-2.2: Distinct Counting
- **Requirement**: Count distinct occurrences within a grouping variable
- **API**: `set_distinct_by()`
- **Behavior**:
  - Counts unique values of distinct_by variable
  - Calculates distinct_n and distinct_pct
  - Common use: count distinct subjects experiencing event
  - Can be combined with non-distinct counts in same row

#### FR-2.3: Nested Counting
- **Requirement**: Count hierarchical relationships (e.g., Body System > Preferred Term)
- **API**: `group_count(vars(outer, inner))`
- **Behavior**:
  - First variable is outer grouping
  - Second variable is inner grouping
  - Both levels are counted and displayed
  - Supports `set_nest_count()` for indented display

#### FR-2.4: Total Rows
- **Requirement**: Add total row summarizing all categories
- **API**: `add_total_row()`, `set_total_row_label()`
- **Behavior**:
  - Adds row with counts across all target variable values
  - Label is customizable
  - Respects distinct counting if configured

#### FR-2.5: Missing Value Handling
- **Requirement**: Control display of missing value counts
- **API**: `set_missing_count()`
- **Behavior**:
  - Can include or exclude missing values
  - Can display missing as separate row
  - Configurable label

#### FR-2.6: Denominator Control
- **Requirement**: Flexible denominator calculation for percentages
- **API**: `set_denoms_by()`, `set_denom_where()`, `set_denom_ignore()`
- **Behavior**:
  - Default: treatment group total
  - Can use population data total
  - Can use custom grouping for denominators
  - Can apply filters to denominator calculation
  - Can ignore specific grouping variables

### FR-3: Descriptive Statistics Layers

#### FR-3.1: Basic Descriptive Statistics
- **Requirement**: Calculate summary statistics for continuous variables
- **API**: `group_desc(target_var, by, where)`
- **Behavior**:
  - Summarizes by treatment group
  - Supports additional grouping via `by`
  - Supports layer-specific filtering via `where`
  - Default statistics: n, mean, sd, median, q1, q3, min, max, missing

#### FR-3.2: Built-in Statistics
- **Requirement**: Provide standard statistical summaries
- **Statistics**: n, mean, sd, median, var, min, max, iqr, q1, q3, missing
- **Behavior**:
  - All use `na.rm=TRUE`
  - Quantiles use configurable algorithm (default Type 7)
  - Min/max convert Inf to NA for consistency
  - Missing counts NA values

#### FR-3.3: Custom Statistics
- **Requirement**: Allow user-defined summary functions
- **API**: `set_custom_summaries()`, `options(tplyr.custom_summaries)`
- **Behavior**:
  - Users provide named list of functions
  - Functions use `.var` placeholder for target variable
  - Custom summaries available in format strings
  - Can override built-in summaries
  - Layer-level overrides session-level

#### FR-3.4: Multi-Variable Summaries
- **Requirement**: Summarize multiple variables in same layer
- **API**: `group_desc(vars(var1, var2, ...))`
- **Behavior**:
  - Each variable summarized independently
  - Results merged into single output
  - Columns named var1_<treat>, var2_<treat>, etc.
  - Same format strings applied to all variables

#### FR-3.5: Quantile Algorithm Configuration
- **Requirement**: Support different quantile calculation methods
- **API**: `options(tplyr.quantile_type = <1-9>)`
- **Behavior**:
  - Default Type 7 (R default)
  - Type 3 matches SAS
  - Affects q1, q3, iqr calculations

### FR-4: Shift Layers

#### FR-4.1: Basic Shift Tables
- **Requirement**: Count changes in state between two time points
- **API**: `group_shift(vars(row=from_var, column=to_var), by, where)`
- **Behavior**:
  - Row variable typically baseline
  - Column variable typically analysis value
  - Creates matrix of from/to counts
  - Calculates percentages

#### FR-4.2: Factor-Based Dummy Values
- **Requirement**: Display all factor levels even if not present in data
- **Behavior**:
  - Uses factor levels to create complete matrix
  - Zero-fills missing combinations
  - Respects factor ordering

### FR-5: String Formatting

#### FR-5.1: Format String Specification
- **Requirement**: Declarative format specification for numeric results
- **API**: `f_str(format, ...summaries)`
- **Behavior**:
  - Format string uses 'x' for digits
  - Integer width: number of x's before decimal
  - Decimal precision: number of x's after decimal
  - Multiple summaries can be combined in one string
  - Example: `f_str('xx.x (xx.xx)', mean, sd)`

#### FR-5.2: Auto-Precision
- **Requirement**: Automatically determine appropriate decimal precision
- **API**: Use 'a' in format string
- **Behavior**:
  - Analyzes numeric data to determine precision
  - Configurable via `set_precision_by()`, `set_precision_on()`
  - Ensures consistent precision within groups

#### FR-5.3: Parenthesis Hugging
- **Requirement**: Right-align numbers within fixed-width fields
- **API**: Use capital 'X' or 'A' in format string
- **Behavior**:
  - Preserves total width
  - Pulls preceding character right to "hug" number
  - Example: `'XX.x'` formats 5.2 as " 5.2" not "05.2"

#### FR-5.4: Format String Application
- **Requirement**: Apply format strings to calculated statistics
- **API**: `set_format_strings()`
- **Behavior**:
  - Left side of `=` becomes row label
  - Right side is `f_str()` specification
  - Different for count vs desc layers:
    - Count: uses n, pct, distinct_n, distinct_pct
    - Desc: uses statistic names (mean, sd, etc.)

### FR-6: Sorting

#### FR-6.1: Multiple Sort Methods
- **Requirement**: Support various sorting strategies
- **API**: `set_order_count_method()`, `set_ordering_cols()`
- **Methods**:
  - `bycount`: Sort by frequency (descending)
  - `byfactor`: Use factor level order
  - `byvarn`: Alphabetical by variable name
- **Behavior**:
  - Can specify which treatment group to sort by
  - Maintains sort order in output via ord_* columns

#### FR-6.2: Nested Sort Control
- **Requirement**: Control sort position of outer vs inner groups
- **API**: `set_outer_sort_position()`
- **Behavior**:
  - Determines if outer group appears first or last
  - Affects nested count displays

### FR-7: Metadata & Traceability

#### FR-7.1: Metadata Generation
- **Requirement**: Generate traceability information for each result
- **API**: `build(metadata=TRUE)`
- **Behavior**:
  - Creates metadata data frame parallel to output
  - Each cell contains filters, groupings, summaries applied
  - Links via row_id
  - Stored in table environment

#### FR-7.2: Metadata Extraction
- **Requirement**: Access metadata for specific results
- **API**: `get_metadata()`, `get_meta_result()`, `get_meta_subset()`
- **Behavior**:
  - Extract full metadata table
  - Get metadata for specific row/column
  - Filter metadata by criteria

#### FR-7.3: Custom Metadata
- **Requirement**: Allow users to add custom metadata
- **API**: `append_metadata()`
- **Behavior**:
  - Users can extend metadata with custom fields
  - Integrated into standard metadata structure

### FR-8: Risk Difference

#### FR-8.1: Risk Difference Calculation
- **Requirement**: Calculate risk differences between treatment groups
- **API**: `add_risk_diff()`
- **Behavior**:
  - Compares proportions between groups
  - Adds risk difference columns to output
  - Supports multiple comparison types
  - Only applicable to count layers

### FR-9: Data Completion

#### FR-9.1: Dummy Row Generation
- **Requirement**: Create rows for all possible combinations of grouping variables
- **Behavior**:
  - Default: creates all combinations of by variables
  - Zero-fills missing combinations
  - Uses factor levels when available

#### FR-9.2: Data Limiting
- **Requirement**: Limit output to values present in data
- **API**: `set_limit_data_by()`
- **Behavior**:
  - Restricts dummy rows to observed combinations
  - Can specify which variables to limit by
  - Reduces output size for sparse data

### FR-10: Column Headers

#### FR-10.1: Header N Calculation
- **Requirement**: Calculate N for each treatment group
- **API**: `header_n()`
- **Behavior**:
  - Uses population data if specified
  - Respects population filters
  - Accounts for column grouping variables
  - Returns data frame of N values

#### FR-10.2: Custom Headers
- **Requirement**: Add formatted headers to output
- **API**: `add_column_headers()`
- **Behavior**:
  - Combines treatment labels with N values
  - Customizable format

### FR-11: Layer Templates

#### FR-11.1: Template Creation
- **Requirement**: Save layer configurations for reuse
- **API**: `layer_template()`
- **Behavior**:
  - Captures layer configuration
  - Can be applied to multiple tables
  - Reduces code duplication

### FR-12: Numeric Data Access

#### FR-12.1: Numeric Data Extraction
- **Requirement**: Access unformatted numeric results
- **API**: `get_numeric_data()`
- **Behavior**:
  - Returns list of numeric data frames (one per layer)
  - Contains all calculated statistics before formatting
  - Useful for validation and debugging

### FR-13: Conditional Formatting

#### FR-13.1: Conditional Format Rules
- **Requirement**: Apply formatting based on data values
- **API**: `apply_conditional_format()`
- **Behavior**:
  - Supports complex conditional logic
  - Can modify formatting based on cell values

### FR-14: Options & Configuration

#### FR-14.1: Package Options
- **Options**:
  - `tplyr.scipen`: Scientific notation threshold
  - `tplyr.quantile_type`: Quantile algorithm
  - `tplyr.custom_summaries`: Session-level custom summaries
- **Behavior**:
  - Set via `options()`
  - Affect all tables in session
  - Can be overridden at table/layer level

### FR-15: Input Validation

#### FR-15.1: Comprehensive Validation
- **Requirement**: Validate user inputs at construction time
- **Behavior**:
  - Check variable existence in datasets
  - Validate data types (numeric for desc layers)
  - Validate parameter combinations
  - Provide clear error messages

### FR-16: Piping Support

#### FR-16.1: Magrittr Compatibility
- **Requirement**: All functions support pipe operator
- **Behavior**:
  - All modifier functions return modified object
  - Enables fluent, declarative syntax
  - Example: `table %>% add_layer(...) %>% build()`

## Non-Functional Requirements

### NFR-1: Backward Compatibility
- **Requirement**: Maintain API compatibility across versions
- **Behavior**: Deprecated functions maintained with warnings

### NFR-2: Performance
- **Requirement**: Handle typical clinical trial datasets efficiently
- **Typical Size**: 1000-10000 rows, 50-200 columns
- **Acceptable Build Time**: < 10 seconds for complex tables

### NFR-3: Documentation
- **Requirement**: Comprehensive documentation for all features
- **Components**:
  - Function documentation (roxygen2)
  - Vignettes for major features
  - Examples in documentation
  - Cheat sheet

### NFR-4: Testing
- **Requirement**: High test coverage for reliability
- **Components**:
  - Unit tests for all functions
  - Snapshot tests for output validation
  - UAT for qualification

### NFR-5: CRAN Compliance
- **Requirement**: Meet CRAN package standards
- **Behavior**:
  - Pass R CMD check
  - No errors, warnings, or notes
  - Proper licensing
  - Appropriate dependencies

## Critical Behaviors to Preserve

### CB-1: Lazy Evaluation
- Table/layer construction must not process data
- Only `build()` triggers processing
- Enables validation and inspection before execution

### CB-2: Environment-Based State
- Objects are environments, not lists
- Parent-child relationships via environment hierarchy
- Mutable state for configuration

### CB-3: S3 Dispatch
- `process_summaries()`, `process_formatting()`, `process_metadata()`
- Different implementations per layer type
- Extensible design

### CB-4: Quosure Handling
- Variables captured as quosures for tidy evaluation
- Supports both quoted and unquoted variable names
- Enables NSE in user-facing API

### CB-5: Factor Respect
- Factor levels used for ordering
- Factor levels used for dummy value generation
- Factor order preserved in output

### CB-6: Traceability
- Every result traceable to source data
- Metadata generation is optional but complete
- Supports regulatory requirements

## Edge Cases & Special Behaviors

### EC-1: Empty Groups
- Zero-fill when treatment group has no observations
- Maintain column structure

### EC-2: All Missing Data
- Handle gracefully when all values are NA
- Return appropriate missing indicators

### EC-3: Single Treatment Group
- Support tables with only one treatment group
- No comparison columns needed

### EC-4: No By Variables
- Support layers without grouping variables
- Single summary row per layer

### EC-5: Inf Handling
- Convert Inf/-Inf to NA in min/max
- Consistent with other statistics

### EC-6: Character Variables in Count Layers
- Support text strings as target variables
- Useful for custom row labels

## Conclusion

These functional requirements represent the complete feature set of Tplyr that must be preserved during refactoring. Any changes to the codebase should be validated against these requirements to ensure backward compatibility and user expectations are maintained.
