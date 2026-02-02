# Tplyr Nested Count Layer Optimization Plan (2026-02-02)

## Current State

**Baseline Performance:** ~8.8 seconds for 500x body systems benchmark (138,000 input rows → 11,000 output rows)

**Previous Optimizations:** Commit `47fd333` (2026-01-25) achieved 2.39x speedup through vectorization of denominator calculations and sorting operations.

---

## Fresh Profiling Results

### Top Time-Consuming Operations (from profvis)

| Rank | Function | Time Units | % of Total |
|------|----------|------------|------------|
| 1 | `process_summaries.count_layer` | 819,512 | 88.6% |
| 2 | `join_mutate` | 520,631 | 56.3% |
| 3 | `dplyr_locate_matches` / `vec_locate_matches` | 417,624 | 45.2% |
| 4 | `prepare_format_metadata.count_layer` | 410,368 | 44.4% |
| 5 | `process_nested_count_target` | 408,863 | 44.2% |
| 6 | `inner_join.data.frame` | 339,975 | 36.8% |
| 7 | `paste0` | 149,490 | 16.2% |
| 8 | `prefix_count_row` | 149,490 | 16.2% |
| 9 | `vec_slice` | 145,671 | 15.7% |
| 10 | `left_join.data.frame` | 136,781 | 14.8% |

### Call Frequency (Verified)

- `process_summaries.count_layer`: **3 calls** (1 main + 2 sublayers)
- `process_nested_count_target`: **1 call**
- `prepare_format_metadata.count_layer`: **3 calls**

### Key Observations

1. **Join operations dominate** - `join_mutate`, `vec_locate_matches`, `inner_join`, `left_join` collectively account for ~68% of processing time

2. **Call frequency is low** - Functions are called only a handful of times, so the bottleneck is the COST per call, not the NUMBER of calls

3. **prepare_format_metadata has one expensive line** - Line 522 `nchar(numeric_data$n)` consumes most of its time (410,368 time units), operating on 11,000 rows

4. **String operations are significant** - `paste0` and `prefix_count_row` together consume ~16% of time

---

## Optimization Recommendations

### Recommendation 1: Optimize nchar() Call in prepare_format_metadata

**Problem:** Line 522 in R/count.R calls `nchar(numeric_data$n)` on all 11,000 rows just to find the maximum width for formatting. This accounts for 44% of total time.

**Current Code:**
```r
n_width <- max(c(nchar(numeric_data$n), 1L), na.rm = TRUE)
```

**Solution:** Only calculate nchar for unique values or use a more efficient approach.

**Proposed Fix:**
```r
# Option 1: Only check unique values (likely much fewer than 11k)
n_width <- max(c(nchar(unique(numeric_data$n)), 1L), na.rm = TRUE)

# Option 2: Sample-based estimation if unique values are still large
# Option 3: Cache max width from previous calculation if numeric_data hasn't changed
```

**Expected Impact:** 30-50% speedup if this line is truly the bottleneck

**Files to modify:**
- `R/count.R` - Line 522 in `prepare_format_metadata.count_layer`

---

### Recommendation 2: Reduce Join Operations in filter_nested_inner_layer_vectorized

**Problem:** `inner_join` operations in nested layer filtering are expensive. The function `filter_nested_inner_layer_vectorized` (R/nested.R) was already vectorized, but joins are still the bottleneck.

**Current Approach:** Uses `inner_join` to validate (outer, inner) combinations.

**Solution:** Replace join with hash lookup or concatenated key filtering.

**Proposed Fix:**
```r
# Instead of inner_join, use a vectorized %in% check
valid_keys <- paste(valid_combinations[[outer_col]],
                    valid_combinations$valid_summary_var, sep = "\034")
data_keys <- paste(.data[[outer_col]], .data$summary_var, sep = "\034")
.data[data_keys %in% valid_keys, ]
```

**Expected Impact:** 15-25% speedup by eliminating expensive join operations

**Files to modify:**
- `R/nested.R` - `filter_nested_inner_layer_vectorized` function (around line 191-252)

---

### Recommendation 3: Optimize prefix_count_row String Operations

**Problem:** `paste0` and `prefix_count_row` consume 16% of time, likely due to repeated string concatenation with indentation.

**Current Approach:** Uses `paste0` to add indentation prefix to each inner layer row.

**Solution:** Use vectorized string operations more efficiently.

**Proposed Fix:**
```r
# If using paste0 repeatedly in a loop, vectorize it
# Use sprintf if the pattern is consistent
# Consider stringi::stri_paste which can be faster for large vectors
```

**Expected Impact:** 5-10% speedup

**Files to modify:**
- Identify where `prefix_count_row` is called and optimize the string concatenation

---

### Recommendation 4: Consider data.table for Large-Scale Join Operations

**Problem:** dplyr joins (`left_join`, `inner_join`) are optimized for convenience, but `data.table` joins are 5-10x faster for large datasets.

**Solution:** For the hotspot joins identified in Recommendation 2, optionally use `data.table` syntax.

**Implementation:** Create a conditional implementation that uses data.table when available and falls back to dplyr.

**Expected Impact:** Additional 10-20% speedup when data.table is enabled

**Files to modify:**
- `R/nested.R` - Join operations in filtering functions
- `R/utils.R` - Add utility functions for conditional data.table usage

---

## Execution Order

**Recommended sequence (highest impact first):**

1. **Recommendation 1** - Optimize nchar() call (highest single-line impact)
2. **Recommendation 2** - Reduce join operations (cumulative join time savings)
3. **Recommendation 3** - Optimize string operations (lower priority)
4. **Recommendation 4** - data.table for joins (optional, significant speedup)

---

## Benchmarking Script

Use this script to measure progress after each optimization:

```r
library(Tplyr)
library(dplyr)

# Create test data
adae <- do.call(rbind, lapply(1:500, function(i) {
  temp <- tplyr_adae
  temp$AEBODSYS <- paste0(temp$AEBODSYS, "_", i)
  temp$AEDECOD <- paste0(temp$AEDECOD, "_", i)
  temp
}))

cat("Test data rows:", nrow(adae), "\n")

# Benchmark function
benchmark_nested_count <- function(n_runs = 3) {
  times <- numeric(n_runs)

  for (i in seq_len(n_runs)) {
    gc()  # Clean up before each run

    t_start <- Sys.time()
    result <- tplyr_table(adae, TRTA) |>
      set_pop_data(tplyr_adsl) |>
      set_pop_treat_var(TRT01A) |>
      add_layer(
        group_count(vars(AEBODSYS, AEDECOD)) |>
          set_nest_count(TRUE) |>
          set_distinct_by(USUBJID) |>
          set_order_count_method("bycount", break_ties="desc")
      ) |>
      build()

    times[i] <- as.numeric(Sys.time() - t_start, units = "secs")
    cat(sprintf("Run %d: %.2f seconds (%d rows)\n", i, times[i], nrow(result)))
  }

  cat(sprintf("\nMedian time: %.2f seconds\n", median(times)))
  cat(sprintf("Mean time: %.2f seconds\n", mean(times)))

  invisible(times)
}

# Run benchmark
benchmark_nested_count()
```

---

## Progress Tracking

| Recommendation | Status | Before (sec) | After (sec) | Improvement |
|----------------|--------|--------------|-------------|-------------|
| 1. Optimize nchar() | Not started | 8.8 | - | - |
| 2. Reduce joins | Not started | - | - | - |
| 3. String operations | Not started | - | - | - |
| 4. data.table joins | Not started | - | - | - |

**Baseline (2026-02-02):** 8.8 seconds average for nested count benchmark

---

## Notes

- Previous optimization plan (OPTIMIZATION_PLAN.md) was based on older profiling data and is now outdated
- The high call frequency issues (335+ calls) mentioned in the old plan have been resolved by commit `47fd333`
- Current bottlenecks are in the COST per operation, not the NUMBER of operations
