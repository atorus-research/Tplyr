# evalq() Usage Inventory

This document catalogs all current uses of `evalq()` in the Tplyr codebase as of the refactoring preparation phase.

## Summary Statistics

- **Total evalq() calls found**: 42
- **Files containing evalq()**: 14
- **Multi-line code blocks (refactoring targets)**: 38
- **Single-line reads (may keep)**: 4

## Detailed Inventory by File

### 1. R/prebuild.R (2 uses)

#### 1.1 treatment_group_build() - Line 10
- **Type**: Multi-line code block (PRIMARY REFACTORING TARGET)
- **Purpose**: Build treatment groups, apply filters, expand factors
- **Environment**: table
- **Bindings Read**: target, treat_var, pop_data, pop_treat_var, table_where, pop_where, treat_grps, cols
- **Bindings Written**: built_target, built_pop_data
- **Temporary Variables**: grp_i, i, fct_levels (manually cleaned up)
- **Priority**: HIGH - Core table building function

#### 1.2 verify_layer_compatibility.count_layer() - Line 113
- **Type**: Multi-line code block
- **Purpose**: Verify layer compatibility
- **Environment**: layer
- **Priority**: MEDIUM

### 2. R/pop_data.R (2 uses)

#### 2.1 build_header_n() - Line 10
- **Type**: Multi-line code block (PRIMARY REFACTORING TARGET)
- **Purpose**: Calculate header N values for table columns
- **Environment**: table
- **Bindings Read**: cols, pop_data, pop_treat_var, treat_grps
- **Bindings Written**: header_n
- **Priority**: HIGH - Core table building function

#### 2.2 build_header_n() - Line 115
- **Type**: Multi-line code block (nested within above)
- **Purpose**: Create function arguments for treatment groups
- **Environment**: table
- **Priority**: HIGH - Part of header N calculation

### 3. R/count.R (8 uses)

#### 3.1 process_summaries.count_layer() - Line 12
- **Type**: Multi-line code block (PRIMARY REFACTORING TARGET)
- **Purpose**: Process count layer summaries
- **Environment**: layer
- **Bindings Read**: built_target, target_var, by, where, treat_var, cols
- **Bindings Written**: numeric_data
- **Priority**: HIGH - Core layer processing

#### 3.2 process_single_count_target() - Line 135
- **Type**: Multi-line code block
- **Purpose**: Process single count target variable
- **Environment**: layer
- **Priority**: HIGH

#### 3.3 process_count_n() - Line 215
- **Type**: Multi-line code block
- **Purpose**: Calculate count percentages
- **Environment**: layer
- **Priority**: HIGH

#### 3.4 process_count_total_row() - Line 301
- **Type**: Multi-line code block
- **Purpose**: Add total row to counts
- **Environment**: layer
- **Priority**: MEDIUM

#### 3.5 process_missing_subjects_row() - Line 348
- **Type**: Multi-line code block
- **Purpose**: Add missing subjects row
- **Environment**: layer
- **Priority**: MEDIUM

#### 3.6 prepare_format_metadata.count_layer() - Line 387
- **Type**: Multi-line code block
- **Purpose**: Prepare formatting metadata
- **Environment**: layer
- **Priority**: MEDIUM

#### 3.7 process_formatting.count_layer() - Line 443
- **Type**: Multi-line code block (PRIMARY REFACTORING TARGET)
- **Purpose**: Format count layer output
- **Environment**: layer
- **Bindings Read**: numeric_data, format_strings, indentation
- **Bindings Written**: formatted_data
- **Priority**: HIGH - Core layer processing

#### 3.8 factor_treat_var() - Line 701
- **Type**: Multi-line code block
- **Purpose**: Convert treatment variable to factor
- **Environment**: layer
- **Priority**: MEDIUM

#### 3.9 process_count_denoms() - Line 725
- **Type**: Multi-line code block
- **Purpose**: Process count denominators
- **Environment**: layer
- **Priority**: HIGH

#### 3.10 rename_missing_values() - Line 834
- **Type**: Multi-line code block
- **Purpose**: Rename missing values in output
- **Environment**: layer
- **Priority**: LOW

### 4. R/desc.R (2 uses)

#### 4.1 process_summaries.desc_layer() - Line 19
- **Type**: Multi-line code block (PRIMARY REFACTORING TARGET)
- **Purpose**: Process descriptive statistics layer
- **Environment**: layer
- **Bindings Read**: target_var, built_target, by, where, treat_var, cols, stats
- **Bindings Written**: trans_sums
- **Priority**: HIGH - Core layer processing

#### 4.2 process_formatting.desc_layer() - Line 107
- **Type**: Multi-line code block (PRIMARY REFACTORING TARGET)
- **Purpose**: Format descriptive statistics output
- **Environment**: layer
- **Bindings Read**: trans_sums, format_strings
- **Bindings Written**: form_sums
- **Priority**: HIGH - Core layer processing

### 5. R/shift.R (6 uses)

#### 5.1 process_summaries.shift_layer() - Line 5
- **Type**: Multi-line code block (PRIMARY REFACTORING TARGET)
- **Purpose**: Process shift layer summaries
- **Environment**: layer
- **Priority**: HIGH - Core layer processing

#### 5.2 process_shift_n() - Line 41
- **Type**: Multi-line code block
- **Purpose**: Calculate shift counts
- **Environment**: layer
- **Priority**: HIGH

#### 5.3 process_shift_total() - Line 70
- **Type**: Multi-line code block
- **Purpose**: Calculate shift totals
- **Environment**: layer
- **Priority**: MEDIUM

#### 5.4 prepare_format_metadata.shift_layer() - Line 83
- **Type**: Multi-line code block
- **Purpose**: Prepare shift formatting metadata
- **Environment**: layer
- **Priority**: MEDIUM

#### 5.5 process_formatting.shift_layer() - Line 110
- **Type**: Multi-line code block (PRIMARY REFACTORING TARGET)
- **Purpose**: Format shift layer output
- **Environment**: layer
- **Priority**: HIGH - Core layer processing

#### 5.6 process_shift_denoms() - Line 170
- **Type**: Multi-line code block
- **Purpose**: Process shift denominators
- **Environment**: layer
- **Priority**: MEDIUM

### 6. R/sort.R (4 uses)

#### 6.1 add_order_columns.count_layer() - Line 162
- **Type**: Multi-line code block
- **Purpose**: Add ordering columns to count layer
- **Environment**: layer
- **Priority**: HIGH

#### 6.2 add_order_columns.desc_layer() - Line 294
- **Type**: Multi-line code block
- **Purpose**: Add ordering columns to desc layer
- **Environment**: layer
- **Priority**: HIGH

#### 6.3 add_order_columns.shift_layer() - Line 321
- **Type**: Multi-line code block
- **Purpose**: Add ordering columns to shift layer
- **Environment**: layer
- **Priority**: HIGH

#### 6.4 get_data_order() - Line 429
- **Type**: Multi-line code block
- **Purpose**: Get data ordering based on sort method
- **Environment**: layer
- **Priority**: HIGH

### 7. R/process_metadata.R (4 uses)

#### 7.1 process_metadata.desc_layer() - Line 10
- **Type**: Multi-line code block (PRIMARY REFACTORING TARGET)
- **Purpose**: Generate metadata for desc layer
- **Environment**: layer
- **Priority**: HIGH - Metadata generation

#### 7.2 process_metadata.count_layer() - Line 77
- **Type**: Multi-line code block (PRIMARY REFACTORING TARGET)
- **Purpose**: Generate metadata for count layer
- **Environment**: layer
- **Priority**: HIGH - Metadata generation

#### 7.3 process_metadata.tplyr_riskdiff() - Line 137
- **Type**: Multi-line code block
- **Purpose**: Generate metadata for risk difference
- **Environment**: riskdiff object
- **Priority**: MEDIUM

#### 7.4 process_metadata.shift_layer() - Line 200
- **Type**: Multi-line code block (PRIMARY REFACTORING TARGET)
- **Purpose**: Generate metadata for shift layer
- **Environment**: layer
- **Priority**: HIGH - Metadata generation

### 8. R/stats.R (2 uses)

#### 8.1 process_statistic_data.tplyr_riskdiff() - Line 26
- **Type**: Multi-line code block
- **Purpose**: Process risk difference statistics
- **Environment**: riskdiff object
- **Priority**: MEDIUM

#### 8.2 process_statistic_formatting.tplyr_riskdiff() - Line 86
- **Type**: Multi-line code block
- **Purpose**: Format risk difference output
- **Environment**: riskdiff object
- **Priority**: MEDIUM

### 9. R/nested.R (1 use)

#### 9.1 process_nested_count_target() - Line 4
- **Type**: Multi-line code block
- **Purpose**: Process nested count targets
- **Environment**: layer
- **Priority**: MEDIUM

### 10. R/riskdiff.R (1 use)

#### 10.1 add_risk_diff() - Line 171
- **Type**: Multi-line code block
- **Purpose**: Add risk difference to layer
- **Environment**: layer
- **Priority**: MEDIUM

### 11. R/layer.R (2 uses)

#### 11.1 tplyr_layer() - Line 140
- **Type**: Multi-line code block
- **Purpose**: Initialize layer environment defaults
- **Environment**: layer
- **Priority**: LOW - Initialization only

#### 11.2 tplyr_layer() - Line 169
- **Type**: Single-line read (MAY KEEP)
- **Purpose**: Read target names for validation
- **Environment**: parent
- **Priority**: LOW - Simple read operation

#### 11.3 tplyr_layer() - Line 178
- **Type**: Single-line read (MAY KEEP)
- **Purpose**: Check if target variable is numeric
- **Environment**: parent
- **Priority**: LOW - Simple read operation

### 12. R/print.R (4 uses)

#### 12.1 print.tplyr_table() - Line 12
- **Type**: Multi-line code block
- **Purpose**: Print table object
- **Environment**: table
- **Priority**: LOW - Print method only

#### 12.2 print.tplyr_layer() - Line 81
- **Type**: Multi-line code block
- **Purpose**: Print layer object
- **Environment**: layer
- **Priority**: LOW - Print method only

#### 12.3 str.tplyr_table() - Line 123
- **Type**: Multi-line code block
- **Purpose**: Structure display for table
- **Environment**: table
- **Priority**: LOW - Print method only

#### 12.4 str.tplyr_layer() - Line 164
- **Type**: Multi-line code block
- **Purpose**: Structure display for layer
- **Environment**: layer
- **Priority**: LOW - Print method only

### 13. R/gather_defaults.R (3 uses)

#### 13.1 gather_desc_defaults() - Line 24
- **Type**: Single-line read (MAY KEEP)
- **Purpose**: Read desc layer format defaults
- **Environment**: table
- **Priority**: LOW - Simple read operation

#### 13.2 gather_count_defaults() - Line 46
- **Type**: Single-line read (MAY KEEP)
- **Purpose**: Read count layer format defaults
- **Environment**: table
- **Priority**: LOW - Simple read operation

#### 13.3 gather_shift_defaults() - Line 62
- **Type**: Single-line read (MAY KEEP)
- **Purpose**: Read shift layer format defaults
- **Environment**: table
- **Priority**: LOW - Simple read operation

### 14. R/assertions.R (1 use)

#### 14.1 assert_quo_var_present() - Line 98
- **Type**: Single-line read (MAY KEEP)
- **Purpose**: Read target names for assertion
- **Environment**: envir parameter
- **Priority**: LOW - Simple read operation

## Refactoring Priority Classification

### Priority 1: HIGH - Core Processing Functions (Must Refactor)
These are the main data processing functions that execute complex logic in environments:

1. **treatment_group_build()** - R/prebuild.R:10
2. **build_header_n()** - R/pop_data.R:10
3. **process_summaries.count_layer()** - R/count.R:12
4. **process_single_count_target()** - R/count.R:135
5. **process_count_n()** - R/count.R:215
6. **process_count_denoms()** - R/count.R:725
7. **process_formatting.count_layer()** - R/count.R:443
8. **process_summaries.desc_layer()** - R/desc.R:19
9. **process_formatting.desc_layer()** - R/desc.R:107
10. **process_summaries.shift_layer()** - R/shift.R:5
11. **process_shift_n()** - R/shift.R:41
12. **process_formatting.shift_layer()** - R/shift.R:110
13. **add_order_columns.count_layer()** - R/sort.R:162
14. **add_order_columns.desc_layer()** - R/sort.R:294
15. **add_order_columns.shift_layer()** - R/sort.R:321
16. **get_data_order()** - R/sort.R:429
17. **process_metadata.desc_layer()** - R/process_metadata.R:10
18. **process_metadata.count_layer()** - R/process_metadata.R:77
19. **process_metadata.shift_layer()** - R/process_metadata.R:200

**Total: 19 functions**

### Priority 2: MEDIUM - Helper Functions (Should Refactor)
Supporting functions that assist core processing:

1. **verify_layer_compatibility.count_layer()** - R/prebuild.R:113
2. **process_count_total_row()** - R/count.R:301
3. **process_missing_subjects_row()** - R/count.R:348
4. **prepare_format_metadata.count_layer()** - R/count.R:387
5. **factor_treat_var()** - R/count.R:701
6. **process_shift_total()** - R/shift.R:70
7. **prepare_format_metadata.shift_layer()** - R/shift.R:83
8. **process_shift_denoms()** - R/shift.R:170
9. **process_metadata.tplyr_riskdiff()** - R/process_metadata.R:137
10. **process_statistic_data.tplyr_riskdiff()** - R/stats.R:26
11. **process_statistic_formatting.tplyr_riskdiff()** - R/stats.R:86
12. **process_nested_count_target()** - R/nested.R:4
13. **add_risk_diff()** - R/riskdiff.R:171

**Total: 13 functions**

### Priority 3: LOW - Utility Functions (May Keep or Refactor Last)
Simple operations that may not need refactoring:

1. **rename_missing_values()** - R/count.R:834
2. **tplyr_layer() initialization** - R/layer.R:140
3. **print.tplyr_table()** - R/print.R:12
4. **print.tplyr_layer()** - R/print.R:81
5. **str.tplyr_table()** - R/print.R:123
6. **str.tplyr_layer()** - R/print.R:164

**Total: 6 functions**

### Priority 4: KEEP - Single-Line Reads (Likely No Refactoring Needed)
Simple environment reads that are idiomatic and clear:

1. **tplyr_layer() - target names** - R/layer.R:169
2. **tplyr_layer() - numeric check** - R/layer.R:178
3. **gather_desc_defaults()** - R/gather_defaults.R:24
4. **gather_count_defaults()** - R/gather_defaults.R:46
5. **gather_shift_defaults()** - R/gather_defaults.R:62
6. **assert_quo_var_present()** - R/assertions.R:98

**Total: 6 functions**

## Refactoring Strategy

### Phase 1: Core Table Functions
- treatment_group_build()
- build_header_n()

### Phase 2: Count Layer
- process_summaries.count_layer()
- process_single_count_target()
- process_count_n()
- process_count_denoms()
- process_formatting.count_layer()
- process_metadata.count_layer()
- Helper functions

### Phase 3: Desc Layer
- process_summaries.desc_layer()
- process_formatting.desc_layer()
- process_metadata.desc_layer()

### Phase 4: Shift Layer
- process_summaries.shift_layer()
- process_shift_n()
- process_formatting.shift_layer()
- process_metadata.shift_layer()
- Helper functions

### Phase 5: Sorting
- add_order_columns.* methods
- get_data_order()

### Phase 6: Remaining Functions
- Risk difference functions
- Nested count functions
- Other helpers

### Phase 7: Cleanup
- Review print/str methods
- Review utility functions
- Final verification

## Notes

- **Multi-line code blocks**: These are the primary refactoring targets where entire function bodies execute in table/layer environments
- **Single-line reads**: These simple reads may be acceptable to keep as they're clear and don't create side effects
- **Manual cleanup**: Several functions manually remove temporary variables (e.g., `rm(grp_i, i, fct_levels)`) - this will no longer be needed after refactoring
- **Nested evalq()**: build_header_n() has nested evalq() calls that need careful handling

## Success Criteria

Refactoring will be complete when:
- All Priority 1 (HIGH) functions are refactored
- All Priority 2 (MEDIUM) functions are refactored
- Priority 3 (LOW) functions are evaluated and refactored if beneficial
- Priority 4 (KEEP) functions are reviewed and kept or refactored as appropriate
- Zero uses of evalq() for multi-line code blocks remain
- All tests pass
- Performance is maintained
