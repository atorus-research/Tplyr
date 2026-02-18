# Nested Count Layer Refactor Plan

## Executive Summary

This document outlines a plan to refactor the nested count layer implementation in Tplyr to:
1. **Improve performance** through proper vectorization
2. **Enable arbitrary nesting depth** (not limited to 2 levels)
3. **Maintain full backwards compatibility** with existing user interfaces

---

## Current Implementation Problems

### Performance Issues

1. **Deprecated `do()` pattern** - Sequential group-by-group processing:
   - `nested.R:88-90`: `group_by(target_var[[1]]) %>% do(filter_nested_inner_layer(...))`
   - `sort.R:219-236`: `group_by(row_label1) %>% do(add_data_order_nested(...))`

2. **Repeated string operations** - Indentation prefix added/removed multiple times during processing

3. **Non-vectorized sorting** - Sort variables created per-group rather than vectorized across all data

### Architectural Limitations

1. **Hardcoded 2-level assumption** - Current code explicitly creates "first layer" and "second layer"
2. **Intermediate layer object creation** - Creates and processes separate layer objects for outer/inner
3. **Complex state management** - Saves/restores `by` and `target_var` for layer rebuilding

---

## Proposed Design

### Core Principle: Process Levels Independently, Then Reduce

For `target_vars = [level_1, level_2, ..., level_n]`:

```
Level 1: group_by(by_vars)                           → count level_1
Level 2: group_by(by_vars, level_1)                  → count level_2
Level 3: group_by(by_vars, level_1, level_2)         → count level_3
...
Level n: group_by(by_vars, level_1, ..., level_n-1)  → count level_n

Final: reduce(bind_rows) with proper indentation at each level
```

### Key Design Decisions

#### 1. Processing Strategy

Each nesting level is processed as a **complete count operation** using existing `process_count_n()` machinery:

```r
process_nested_level <- function(level_index, target_vars, by_vars, ...) {
  # Outer variables become additional by variables
  outer_vars <- target_vars[seq_len(level_index - 1)]
  current_target <- target_vars[[level_index]]

  # Process using existing vectorized count logic
  process_count_n(
    target_var = current_target,
    by = c(outer_vars, by_vars),
    ...
  )
}
```

#### 2. Denominator Handling

Denominators follow existing configuration via `set_denoms_by()`, `set_denom_where()`, `set_denom_ignore()`:

- **Level 1 (outermost)**: Uses `denoms_by` as configured
- **Level 2+**: Replaces `summary_var` in `denoms_by` with the outer target variable(s)

This preserves the current behavior where:
- If `denoms_by` includes `summary_var`, it refers to the appropriate outer level
- Population data denominators work unchanged

#### 3. Indentation Strategy

Each level gets cumulative indentation:
- Level 1: No indentation (outermost)
- Level 2: `indentation` (1x)
- Level 3: `indentation` + `indentation` (2x)
- Level n: `(n-1) * indentation`

Indentation is applied **once** after numeric processing, not during.

#### 4. Sorting Variable Creation

Sort variables are created per-level in a vectorized manner:

```r
# For each nesting level, create ord_layer_X
# Level 1 sort is independent
# Level 2 sort is within Level 1 groups
# etc.

# Instead of do(), use vectorized joins:
numeric_data %>%
  left_join(level_1_order, by = join_keys) %>%
  left_join(level_2_order, by = join_keys) %>%
  ...
```

#### 5. Recursive/Reduce Pattern

The implementation uses `purrr::reduce()` or equivalent:

```r
process_nested_count_target <- function(x) {
  target_vars <- x$target_var
  n_levels <- length(target_vars)

  # Process each level
  level_results <- map(seq_len(n_levels), function(i) {
    process_nested_level(i, target_vars, by_vars, ...)
  })

  # Add indentation to each level
  level_results <- imap(level_results, function(result, i) {
    add_level_indentation(result, level = i, indentation = indentation)
  })

  # Combine results
  numeric_data <- reduce(level_results, bind_rows)

  ...
}
```

---

## Implementation Plan

### Phase 1: Core Nested Processing Refactor

**File: `R/nested.R`**

1. Rewrite `process_nested_count_target()` to:
   - Accept arbitrary number of target variables
   - Process each level using existing `process_count_n()`
   - Use `reduce()` pattern for combining results
   - Apply indentation vectorized after processing

2. Replace `filter_nested_inner_layer()` with vectorized filtering:
   - Pre-compute valid inner values per outer group
   - Use vectorized `%in%` filtering instead of `do()`

3. Update `filter_nested_numeric()` to handle n-levels

### Phase 2: Denominator Handling

**File: `R/denom.R`**

1. Generalize `denoms_by` replacement logic for n-levels
2. Ensure `get_denom_total()` works correctly with multi-level grouping
3. Verify `set_denoms_by()`, `set_denom_where()`, `set_denom_ignore()` behavior preserved

### Phase 3: Sorting Refactor

**File: `R/sort.R`**

1. Replace `do(add_data_order_nested())` with vectorized approach:
   - Compute sort order for each level independently
   - Join sort orders to formatted data
   - Avoid per-group function application

2. Update `add_data_order_nested()` or replace entirely:
   - Handle arbitrary nesting depth
   - Use vectorized operations throughout

### Phase 4: Count Layer Integration

**File: `R/count.R`**

1. Update dispatch logic in `process_summaries.count_layer()`:
   - Route to nested processing when `length(target_var) > 1`
   - Remove assumption of exactly 2 target variables

2. Verify all helper functions work with new structure

### Phase 5: Testing & Validation

1. Run all existing tests - must pass unchanged
2. Profile performance improvements
3. Test with 3+ nesting levels (new capability)

---

## Backwards Compatibility Checklist

| Feature | Compatibility Strategy |
|---------|----------------------|
| `group_count(vars(outer, inner))` | Unchanged API |
| `set_denoms_by()` | Unchanged behavior |
| `set_denom_where()` | Unchanged behavior |
| `set_denom_ignore()` | Unchanged behavior |
| `set_count_row_prefix()` / indentation | Unchanged, extended to n-levels |
| `set_nest_count()` | Unchanged behavior |
| `add_total_row()` error on nested | Unchanged - error preserved |
| `add_missing_subjects_row()` | Unchanged - applies to inner levels |
| Sort order variables (`ord_layer_*`) | Same output structure |
| `numeric_cutoff` on nested | Unchanged behavior |
| All existing error messages | Preserved unless compelling reason |

---

## Risk Assessment

| Risk | Mitigation |
|------|------------|
| Subtle behavior changes | Comprehensive test suite must pass |
| Denominator calculation differences | Careful review of denom.Rmd examples |
| Sort order differences | Compare outputs before/after |
| Performance regression in edge cases | Profile with various data sizes |

---

## Phased Approach

### Phase 1 (This PR): Performance Optimization with 100% Backwards Compatibility

**Goal**: Refactor nested count layer implementation for performance while maintaining exact backwards compatibility.

**Scope**:
- Maintain support for exactly 2 nesting levels (current behavior)
- Preserve all existing error messages (including "total rows not allowed in nested")
- Keep current text/symbol pattern handling unchanged
- All existing unit tests must pass without modification
- **Performance target**: Minimum 2x speedup, with potential for significantly more

**Not in scope for Phase 1**:
- 3+ levels of nesting
- New text/symbol pattern combinations
- Changes to total row behavior
- Any new user-facing features

### Phase 2 (Future PR): Extended Nesting Capabilities

**Scope** (to be refined later):
- Support for 3+ levels of nesting
- Potential new text/symbol pattern combinations
- Any additional features that build on the refactored foundation

---

## Performance Benchmarks

Baseline benchmarks recorded on 2025-01-22. See `tests/benchmarks/nested_count_benchmark.R` for the benchmark script.

### Realistic AE-Style Data Benchmarks

Using multiplied `tplyr_adae` data with `set_order_count_method("bycount", break_ties="desc")`:

| Scale | Rows | Outer Values | Inner Values | Time (sec) |
|-------|------|--------------|--------------|------------|
| 10x | 2,760 | 10 | 210 | 0.44 |
| 25x | 6,900 | 25 | 525 | 0.57 |
| 50x | 13,800 | 50 | 1,050 | 0.84 |
| 75x | 20,700 | 75 | 1,575 | 1.28 |
| 100x | 27,600 | 100 | 2,100 | 1.73 |
| 150x | 41,400 | 150 | 3,150 | 2.81 |

### Complexity Analysis

Model fitting (R² values - higher = better fit):
- **O(n) linear**: 0.9792
- **O(n²) quadratic**: 0.9841
- **O(n log n)**: 0.9932
- **O(n + n²) mixed**: 0.9992

**Finding**: The complexity appears to be **O(n log n)** or slightly superlinear, not purely quadratic. The time per outer value stabilizes at ~0.017-0.019 sec for larger datasets.

### Profiling Results (100x scale, 27,600 rows)

| Function | % Total Time | Notes |
|----------|--------------|-------|
| `do()` | 59.6% | **Primary bottleneck** |
| `process_formatting.count_layer` | 52.4% | Includes nested formatting |
| `process_nested_count_target` | 44.5% | Nested count logic |
| `add_order_columns.count_layer` | 41.8% | Sort variable creation |
| `add_data_order_nested` | 41.2% | **Nested sorting bottleneck** |
| `select` | 32.9% | Column selection overhead |
| `pivot_wider` | 32.2% | Reshaping |
| `get_data_order_bycount` | 29.5% | Count-based ordering |

**Key insight**: The `do()` function takes **60% of total execution time**. The sorting logic (`add_data_order_nested` + `get_data_order_bycount`) accounts for **~41% of time** and is called within `do()`. Replacing the `do()` pattern with vectorized operations should yield 2-3x improvement.

### Target

- Minimum 2x improvement for nested layers
- Reduce complexity from O(n log n) toward O(n) if possible

---

## Files to Modify

| File | Changes |
|------|---------|
| `R/nested.R` | Major rewrite - core of the refactor |
| `R/sort.R` | Significant changes to `add_order_columns.count_layer()` and `add_data_order_nested()` |
| `R/denom.R` | Minor updates for n-level support |
| `R/count.R` | Minor dispatch logic updates |

## Files Unchanged

| File | Reason |
|------|--------|
| All exported function interfaces | Backwards compatibility |
| `R/layer.R` | No changes needed |
| `R/table.R` | No changes needed |
| `vignettes/*` | Document existing behavior |

