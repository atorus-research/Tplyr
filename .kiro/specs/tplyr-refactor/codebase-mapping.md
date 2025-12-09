# Tplyr Codebase Mapping Document

## Executive Summary

**Tplyr** is a mature R package (v1.2.1) designed for creating clinical summary tables in pharmaceutical research. It provides a grammar-based approach to building complex summary tables through a layered architecture. The package is production-ready, CRAN-published, and includes comprehensive user acceptance testing (UAT) documentation for use in qualified programming environments.

## Package Overview

### Purpose
Tplyr simplifies the creation of clinical summary tables by:
- Providing a declarative, grammar-based API for table construction
- Supporting traceability from source data to summary results
- Enabling consistent formatting across different summary types
- Reducing redundant code in clinical reporting workflows

### Key Statistics
- **Lines of Code**: ~45 R source files in `/R` directory
- **Test Coverage**: Comprehensive test suite in `/tests/testthat` with snapshot testing
- **Documentation**: 12+ vignettes covering all major features
- **Dependencies**: Built on tidyverse ecosystem (dplyr, tidyr, purrr, stringr, etc.)
- **Lifecycle**: Stable, production-ready

## Core Architecture

### Object Model

Tplyr uses an environment-based object-oriented design with two primary object types:

#### 1. `tplyr_table` Object
**Location**: `R/table.R`

**Purpose**: Container for the entire table, holding layers and table-level configuration

**Key Bindings**:
- `target`: Source dataset for summaries
- `treat_var`: Treatment group variable
- `pop_data`: Population dataset (defaults to target)
- `pop_treat_var`: Treatment variable in population data
- `cols`: Additional column grouping variables
- `table_where`: Global filter criteria
- `layers`: Container for tplyr_layer objects
- `treat_grps`: Additional treatment groups (e.g., "Total", "Treated")
- `header_n`: Header N values by treatment group
- Format defaults: `count_layer_formats`, `desc_layer_formats`, `shift_layer_formats`

**Constructor**: `tplyr_table(target, treat_var, where, cols)`

**Key Methods**:
- `add_layer()` / `add_layers()`: Attach layers to table
- `add_treat_grps()` / `add_total_group()`: Add treatment group combinations
- `set_pop_data()`, `set_pop_treat_var()`, `set_pop_where()`: Configure population data
- `build()`: Execute data processing and generate output

#### 2. `tplyr_layer` Object
**Location**: `R/layer.R`

**Purpose**: Represents a single summary section within a table

**Key Bindings**:
- `type`: Layer type ("count", "desc", or "shift")
- `target_var`: Variable(s) to summarize
- `by`: Grouping variables for rows
- `cols`: Column grouping variables (inherited from table)
- `where`: Layer-specific filter criteria
- `layers`: Container for sublayers (nested summaries)
- `precision_by`, `precision_on`: Variables controlling numeric precision

**Layer Types**:
1. **Count Layers** (`group_count`): Frequency tables, n (%) summaries
2. **Descriptive Statistics Layers** (`group_desc`): Continuous variable summaries
3. **Shift Layers** (`group_shift`): Change-in-state tables (e.g., baseline to endpoint)

**Constructor Functions**:
- `group_count(parent, target_var, by, where, ...)`
- `group_desc(parent, target_var, by, where, ...)`
- `group_shift(parent, vars(row=..., column=...), by, where, ...)`

### Data Flow Architecture

```
User Code
    ↓
tplyr_table() ← Configuration
    ↓
add_layer() × N ← Layer definitions
    ↓
build() ← Execution trigger
    ↓
┌─────────────────────────────────┐
│ Pre-processing                  │
│ - Treatment group expansion     │
│ - Header N calculation          │
└─────────────────────────────────┘
    ↓
┌─────────────────────────────────┐
│ Layer Processing (per layer)    │
│ 1. process_summaries()          │
│    - Data filtering             │
│    - Grouping & aggregation     │
│    - Numeric calculations       │
│ 2. process_formatting()         │
│    - String formatting          │
│    - Pivoting                   │
│    - Column arrangement         │
│ 3. process_metadata() [optional]│
│    - Traceability info          │
└─────────────────────────────────┘
    ↓
┌─────────────────────────────────┐
│ Post-processing                 │
│ - Layer stacking                │
│ - Column ordering               │
│ - Metadata assembly             │
└─────────────────────────────────┘
    ↓
Output DataFrame
```

## Module Breakdown

### 1. Table Management (`R/table.R`, `R/table_bindings.R`)
**Responsibilities**:
- Table object creation and validation
- Treatment group management
- Population data configuration
- Header N calculation

**Key Functions**:
- `tplyr_table()`: Constructor
- `add_treat_grps()`, `add_total_group()`: Treatment group expansion
- `set_pop_data()`, `set_pop_treat_var()`, `set_pop_where()`: Population configuration
- `build_header_n()`: Calculate header N values

### 2. Layer Management (`R/layer.R`, `R/layering.R`, `R/layer_bindings.R`)
**Responsibilities**:
- Layer object creation and validation
- Layer attachment to tables
- Layer-specific configuration

**Key Functions**:
- `tplyr_layer()`: Base constructor
- `add_layer()`, `add_layers()`: Layer attachment
- `set_where()`, `set_distinct_by()`, `set_nest_count()`: Layer configuration

### 3. Count Layers (`R/count.R`, `R/count_bindings.R`)
**Responsibilities**:
- Frequency counting (distinct and non-distinct)
- Nested count summaries (e.g., Body System > Preferred Term)
- Total rows and missing value handling
- Percentage calculations with flexible denominators

**Key Functions**:
- `group_count()`: Constructor
- `set_distinct_by()`: Configure distinct counting
- `set_nest_count()`: Enable nested display
- `add_total_row()`, `set_missing_count()`: Row additions
- `set_denoms_by()`, `set_denom_where()`: Denominator control

**Processing Flow**:
1. Filter data by `where` criteria
2. Group by treatment + `by` variables + target variable
3. Calculate counts (n, distinct_n)
4. Calculate percentages using appropriate denominators
5. Format strings using `f_str` specifications
6. Pivot to wide format by treatment groups

### 4. Descriptive Statistics Layers (`R/desc.R`, `R/desc_bindings.R`, `R/stats.R`)
**Responsibilities**:
- Continuous variable summaries
- Built-in statistics (mean, sd, median, min, max, q1, q3, iqr, var, missing)
- Custom summary functions
- Auto-precision calculation

**Key Functions**:
- `group_desc()`: Constructor
- `set_format_strings()`: Define summaries and formatting
- `set_custom_summaries()`: Add user-defined statistics
- `set_precision_by()`, `set_precision_on()`: Control auto-precision
- Built-in summary functions in `R/stats.R`

**Processing Flow**:
1. Filter data by `where` criteria
2. Group by treatment + `by` variables
3. Calculate all requested statistics
4. Determine precision (if auto-precision enabled)
5. Format strings using `f_str` specifications
6. Pivot to wide format by treatment groups

### 5. Shift Layers (`R/shift.R`, `R/shift_bindings.R`)
**Responsibilities**:
- Change-in-state summaries (e.g., baseline to analysis)
- Row/column matrix presentation
- Denominator handling for shift tables

**Key Functions**:
- `group_shift()`: Constructor
- Uses `vars(row=..., column=...)` syntax
- Shares much code with count layers

**Processing Flow**:
1. Filter data by `where` criteria
2. Group by treatment + `by` variables + row variable + column variable
3. Calculate counts
4. Calculate percentages
5. Format strings
6. Pivot to create row/column matrix

### 6. String Formatting (`R/format.R`, `R/num_fmt.R`, `R/set_format_strings.R`)
**Responsibilities**:
- `f_str` object creation and management
- Numeric formatting with precision control
- Auto-precision calculation
- Parenthesis hugging (capital X/A formatting)

**Key Components**:
- `f_str()`: Format string constructor
- `num_fmt()`: Numeric formatting engine
- `apply_formats()`: Apply formatting to numeric data
- `apply_conditional_format()`: Conditional formatting rules

**Format String Syntax**:
- `'xx.x'`: Integer width 2, decimal precision 1
- `'XX.x'`: Parenthesis hugging on integer side
- `'xx.A'`: Parenthesis hugging on decimal side
- `'a'`: Auto-precision

### 7. Build Process (`R/build.R`, `R/prebuild.R`)
**Responsibilities**:
- Orchestrate table construction
- Layer processing coordination
- Metadata generation

**Key Functions**:
- `build()`: Main entry point
- `process_summaries()`: S3 generic for numeric calculations
- `process_formatting()`: S3 generic for string formatting
- `process_metadata()`: S3 generic for traceability metadata

### 8. Metadata & Traceability (`R/meta.R`, `R/meta-builders.R`, `R/meta-helpers.R`, `R/meta_utils.R`, `R/process_metadata.R`)
**Responsibilities**:
- Generate traceability information for each result
- Link summary results back to source data
- Support custom metadata extensions

**Key Functions**:
- `get_metadata()`: Extract metadata from built table
- `get_meta_result()`: Get metadata for specific result
- `append_metadata()`: Add custom metadata
- Metadata builders for each layer type

### 9. Sorting (`R/sort.R`)
**Responsibilities**:
- Control row ordering in output
- Support multiple sorting strategies

**Sorting Methods**:
- `bycount`: Sort by frequency (descending)
- `byfactor`: Use factor levels
- `byvarn`: Use variable name alphabetically
- Custom ordering via `set_order_count_method()`, `set_ordering_cols()`

### 10. Denominators (`R/denom.R`)
**Responsibilities**:
- Calculate appropriate denominators for percentages
- Support multiple denominator strategies

**Denominator Types**:
- Treatment group totals (default)
- Population data totals
- Custom denominators via `set_denoms_by()`
- Conditional denominators via `set_denom_where()`

### 11. Risk Difference (`R/riskdiff.R`)
**Responsibilities**:
- Calculate risk differences between treatment groups
- Add risk difference columns to count layers

**Key Functions**:
- `add_risk_diff()`: Add risk difference calculation
- Supports multiple comparison types

### 12. Utilities (`R/utils.R`, `R/assertions.R`, `R/regex.R`, `R/str_extractors.R`)
**Responsibilities**:
- Helper functions
- Input validation
- String manipulation
- Regular expressions for parsing format strings

### 13. Column Headers (`R/column_headers.R`)
**Responsibilities**:
- Generate column headers with N values
- Support custom header text

**Key Functions**:
- `header_n()`: Extract header N values
- `add_column_headers()`: Add headers to output

### 14. Layer Templates (`R/layer_templates.R`)
**Responsibilities**:
- Save and reuse layer configurations
- Reduce code duplication

**Key Functions**:
- `layer_template()`: Create reusable layer template
- Templates can be applied to multiple tables

### 15. Data Completion (`R/set_limit_data_by.R`)
**Responsibilities**:
- Control dummy row generation
- Limit output to values present in data

**Key Functions**:
- `set_limit_data_by()`: Specify variables to limit by

### 16. Precision Control (`R/precision.R`, `R/get_numeric.R`)
**Responsibilities**:
- Auto-calculate decimal precision
- Extract numeric data from layers

**Key Functions**:
- `set_precision_by()`, `set_precision_on()`: Configure auto-precision
- `get_numeric_data()`: Extract numeric results

### 17. Population Data (`R/pop_data.R`)
**Responsibilities**:
- Handle separate population datasets
- Calculate denominators from population data

**Use Case**: When target dataset (e.g., ADAE) doesn't contain all subjects

### 18. Printing & Display (`R/print.R`)
**Responsibilities**:
- Custom print methods for Tplyr objects
- Display object summaries

### 19. Options (`R/zzz.R`)
**Responsibilities**:
- Package-level options
- Default settings

**Key Options**:
- `tplyr.scipen`: Scientific notation threshold
- `tplyr.quantile_type`: Quantile algorithm (default 7, SAS uses 3)
- `tplyr.custom_summaries`: Session-level custom summaries

### 20. Nested Counts (`R/nested.R`, `R/collapse_row_labels.R`)
**Responsibilities**:
- Handle nested count displays
- Collapse row labels for nested presentation

### 21. Conditional Formatting (`R/apply_conditional_format.R`)
**Responsibilities**:
- Apply conditional formatting rules
- Support complex formatting logic

### 22. Data Files (`R/data.R`, `/data/*.rda`)
**Responsibilities**:
- Example datasets for vignettes and testing
- CDISC ADaM-like datasets (ADSL, ADAE, ADLB, ADAS, ADPE)

## Key Design Patterns

### 1. Environment-Based OOP
- Objects are R environments, not S3/S4 classes
- Parent-child relationships via environment hierarchy
- Allows mutable state and reference semantics

### 2. Lazy Evaluation
- Table/layer construction doesn't process data
- `build()` triggers execution
- Enables validation before expensive operations

### 3. S3 Method Dispatch
- `process_summaries()`, `process_formatting()`, `process_metadata()`
- Different implementations for count/desc/shift layers
- Extensible design

### 4. Quosure-Based NSE
- Heavy use of `rlang` for non-standard evaluation
- Variables captured as quosures
- Enables tidy evaluation in user-facing API

### 5. Pipe-Friendly API
- All modifier functions return modified object
- Supports `%>%` chaining
- Declarative table construction

### 6. Format String DSL
- `f_str()` creates mini-language for formatting
- Captures both format and statistics to calculate
- Enables auto-precision and parenthesis hugging

## Data Structures

### Output DataFrame Structure
```
row_id          # Unique identifier (if metadata enabled)
row_label1      # Outer grouping label
row_label2      # Inner grouping label (if nested)
var1_<treat1>   # Results for treatment 1
var1_<treat2>   # Results for treatment 2
...
ord_layer_index # Layer ordering
ord_layer_1     # Primary sort order
ord_layer_2     # Secondary sort order
...
```

### Metadata Structure
```
row_id          # Links to output row_id
row_label1      # Matches output
row_label2      # Matches output
var1_<treat1>   # Metadata for treatment 1 result
...
```

Each metadata cell contains:
- Filters applied
- Grouping variables
- Summary type
- Source data reference

## Testing Strategy

### Unit Tests (`/tests/testthat/test-*.R`)
- Comprehensive coverage of all modules
- Snapshot testing for output validation
- Test data in `/tests/testthat/*.Rdata`

### UAT (`/uat/`)
- User acceptance testing framework
- Test cases mapped to requirements
- Independent validation
- Qualification documentation for regulated environments

## Dependencies

### Core Dependencies
- **dplyr**: Data manipulation, grouping, summarization
- **tidyr**: Pivoting, data reshaping
- **purrr**: Functional programming, iteration
- **stringr**: String manipulation
- **rlang**: Non-standard evaluation, quosures
- **tibble**: Modern data frames
- **magrittr**: Pipe operator

### Optional Dependencies
- **knitr**, **rmarkdown**: Vignettes
- **testthat**: Testing
- **huxtable**, **pharmaRTF**: Styled output

## Extension Points

### 1. Custom Summaries
Users can add custom statistics via:
- `set_custom_summaries()` at layer level
- `options(tplyr.custom_summaries = ...)` at session level

### 2. Custom Metadata
Users can extend metadata via:
- `append_metadata()` function
- Custom metadata builders

### 3. Layer Types
New layer types could be added via:
- New S3 methods for `process_summaries()`, `process_formatting()`, `process_metadata()`
- New constructor function (e.g., `group_<newtype>()`)

### 4. Format Strings
Custom formatting logic via:
- Conditional formatting rules
- Custom format string parsers

## Known Limitations & Considerations

### 1. Performance
- Environment-based design has memory overhead
- Large tables with many layers can be slow
- No built-in caching or memoization

### 2. Flexibility vs. Complexity
- Highly flexible API leads to many configuration options
- Learning curve for advanced features
- Documentation is extensive but necessary

### 3. R-Specific
- Tightly coupled to R ecosystem
- Not portable to other languages

### 4. Factor Dependency
- Relies on R factors for ordering and dummy values
- Factor behavior can be surprising to new R users

## File Organization

```
Tplyr/
├── R/                      # Source code (45 files)
│   ├── table.R            # Table object
│   ├── layer.R            # Layer object
│   ├── count.R            # Count layer logic
│   ├── desc.R             # Desc layer logic
│   ├── shift.R            # Shift layer logic
│   ├── build.R            # Build orchestration
│   ├── format.R           # String formatting
│   ├── meta*.R            # Metadata (4 files)
│   └── ...                # Supporting modules
├── tests/                 # Test suite
│   └── testthat/          # Unit tests
├── vignettes/             # Documentation (12+ vignettes)
├── man/                   # R documentation
├── data/                  # Example datasets
├── uat/                   # User acceptance testing
└── docs/                  # pkgdown website

```

## Critical Functions for Refactoring

If refactoring, these are the most critical functions to preserve:

### User-Facing API
1. `tplyr_table()` - Table constructor
2. `group_count()`, `group_desc()`, `group_shift()` - Layer constructors
3. `add_layer()`, `add_layers()` - Layer attachment
4. `build()` - Execution trigger
5. `f_str()` - Format string constructor
6. `set_format_strings()` - Format configuration
7. All `set_*()` modifier functions

### Internal Processing
1. `process_summaries()` - Numeric calculations
2. `process_formatting()` - String formatting
3. `process_metadata()` - Traceability
4. `num_fmt()` - Numeric formatting engine
5. Denominator calculation logic
6. Precision calculation logic

## Conclusion

Tplyr is a mature, well-architected package with a clear separation of concerns. The environment-based OOP design enables flexible configuration while maintaining clean interfaces. The lazy evaluation pattern and S3 dispatch provide extensibility. Any refactoring should carefully preserve the user-facing API and the traceability features, as these are core value propositions for users in regulated environments.
