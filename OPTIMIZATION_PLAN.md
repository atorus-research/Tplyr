# Tplyr Nested Count Layer Optimization Plan

## Benchmark Configuration

**Test Case:**
```r
adae <- do.call(rbind, lapply(1:500, function(i) {
  temp <- tplyr_adae
  temp$AEBODSYS <- paste0(temp$AEBODSYS, "_", i)
  temp$AEDECOD <- paste0(temp$AEDECOD, "_", i)
  temp
}))

tplyr_table(adae, TRTA) |>
    set_pop_data(tplyr_adsl) |>
    set_pop_treat_var(TRT01A) |>
    add_layer(
      group_count(vars(AEBODSYS, AEDECOD)) |>
        set_nest_count(TRUE) |>
        set_distinct_by(USUBJID) |>
        set_order_count_method("bycount", break_ties="desc")
    ) |>
    build()
```

**Data Scale:** 500x body systems, ~127,000 rows in adae, producing ~10,000+ output rows

---

## Diagnostic Summary

### Profiling Results (saved to `profiling_nested_count.html`)

#### Top Time-Consuming Operations

| Rank | Function | Time Units | Calls | % of Tplyr Time |
|------|----------|------------|-------|-----------------|
| 1 | `process_summaries.count_layer` | 1,251,831 | 685 | 55.2% |
| 2 | `join_mutate` | 943,532 | 658 | 41.6% |
| 3 | `vctrs::vec_locate_matches` | 680,677 | 432 | 30.0% |
| 4 | `build.tplyr_table` | 638,775 | 362 | 28.2% |
| 5 | `prepare_format_metadata.count_layer` | 623,909 | 335 | 27.5% |
| 6 | `process_nested_count_target` | 619,916 | 334 | 27.4% |
| 7 | `inner_join.data.frame` | 475,813 | 221 | 21.0% |
| 8 | `vec_slice` | 369,683 | 341 | 16.3% |
| 9 | `left_join.data.frame` | 346,687 | 293 | 15.3% |
| 10 | `paste0` | 278,774 | 189 | 12.3% |

#### File Breakdown

| File | % of Time |
|------|-----------|
| R/build.R | 66.1% |
| R/count.R | 32.5% |
| R/denom.R | 1.0% |
| R/sort.R | 0.1% |

#### Key Observations

1. **Join operations dominate** - `join_mutate`, `vec_locate_matches`, `inner_join`, `left_join` collectively account for ~60% of execution time

2. **High call frequency indicates loops** - Functions called 300+ times suggest per-row or per-group operations that could be batched:
   - `process_summaries.count_layer`: 685 calls
   - `prepare_format_metadata.count_layer`: 335 calls
   - `process_nested_count_target`: 334 calls

3. **Nested layer creates sublayers** - `process_nested_count_target` creates two sublayers (outer + inner), each triggering full `process_summaries` chains

4. **Metadata is built per-row** - 335 calls to `prepare_format_metadata` (one per output row) with expensive string operations each time

---

## Optimization Recommendations

### Recommendation 1: Batch Metadata Preparation

**Problem:** `prepare_format_metadata.count_layer` is called 335 times (once per output row), each time performing string parsing and filter construction.

**Solution:** Build all metadata at once using vectorized string operations instead of per-row loops.

**Files to modify:**
- `R/count.R` - `prepare_format_metadata.count_layer` function
- `R/meta-builders.R` - `build_count_meta` function (if used)

#### Checklist

- [ ] **1.1** Profile current `prepare_format_metadata.count_layer` to confirm per-row overhead
  ```r
  # Add timing inside the function or use profvis focused on this function
  ```

- [ ] **1.2** Review current implementation in `R/count.R`
  ```r
  # Find: prepare_format_metadata.count_layer
  # Note: What data structures are built per-row?
  ```

- [ ] **1.3** Identify the vectorizable components:
  - Filter string construction (currently uses `paste0` per row)
  - `str2lang` parsing (currently per row)
  - `tplyr_meta` object creation

- [ ] **1.4** Implement vectorized version:
  - Build all filter strings at once using vectorized `paste0`/`sprintf`
  - Parse all strings in one `lapply` call
  - Create all `tplyr_meta` objects in a single pass

- [ ] **1.5** Write test to verify identical output
  ```r
  # Compare metadata output before/after optimization
  ```

- [ ] **1.6** Benchmark improvement
  ```r
  # Run the test case and compare times
  # Expected: Significant reduction in prepare_format_metadata time
  ```

**Success Metric:** Reduce `prepare_format_metadata` calls from 335 to 1, and reduce its total time by 80%+

---

### Recommendation 2: Reduce Join Frequency in Nested Layers

**Problem:** `filter_nested_inner_layer_vectorized` (R/nested.R:247) performs an `inner_join` for each outer group to validate (outer, inner) combinations. With 500 body systems, this creates hundreds of join operations.

**Solution:** Pre-compute the complete set of valid combinations once and use a hash lookup or single-pass filter instead of repeated joins.

**Files to modify:**
- `R/nested.R` - `filter_nested_inner_layer_vectorized` function
- `R/nested.R` - `process_nested_count_target` function

#### Checklist

- [ ] **2.1** Profile joins specifically in nested layer processing
  ```r
  # Use profvis with interval=0.001 focused on nested.R
  ```

- [ ] **2.2** Review `filter_nested_inner_layer_vectorized` (R/nested.R:191-252)
  - Note: Currently builds `valid_combinations` lookup table
  - Note: Does `inner_join` at line 247

- [ ] **2.3** Analyze if the join can be replaced with:
  - A pre-computed hash/environment lookup
  - A single vectorized `%in%` check on concatenated keys
  - Example: `paste(outer, inner, sep="|||") %in% valid_keys`

- [ ] **2.4** Implement optimized filtering:
  ```r
  # Option A: Concatenated key lookup
  valid_keys <- paste(valid_combinations[[outer_col]],
                      valid_combinations$valid_summary_var, sep = "|||")
  data_keys <- paste(.data[[outer_col]], .data$summary_var, sep = "|||")
  .data[data_keys %in% valid_keys, ]

  # Option B: Use data.table for faster joins (see Recommendation 4)
  ```

- [ ] **2.5** Write test to verify identical filtering results

- [ ] **2.6** Benchmark improvement
  ```r
  # Expected: Reduce inner_join calls, significant time savings
  ```

**Success Metric:** Eliminate or reduce `inner_join` calls in nested layer processing by 90%+

---

### Recommendation 3: Optimize process_summaries Call Chain

**Problem:** `process_summaries.count_layer` is called 685 times. For nested layers, this is called twice per layer (outer + inner sublayers), and the function contains expensive operations.

**Solution:** Reduce redundant work in `process_summaries` for nested sublayers, or batch the sublayer processing.

**Files to modify:**
- `R/count.R` - `process_summaries.count_layer` function
- `R/nested.R` - `process_nested_count_target` function

#### Checklist

- [ ] **3.1** Trace why `process_summaries` is called 685 times
  ```r
  # Add a counter or debug message to track call sources
  # Expected: 2 calls per nested layer × number of groups?
  ```

- [ ] **3.2** Review `process_nested_count_target` (R/nested.R:11-135)
  - Line 70: `first_layer <- process_summaries(fl)`
  - Line 73-76: `second_layer <- process_summaries(...)`
  - These create new layer objects and process them

- [ ] **3.3** Identify shared work between first_layer and second_layer:
  - Both use same `built_target`, `treat_var`, `cols`
  - Both compute denominators
  - Can denominator computation be shared?

- [ ] **3.4** Profile inside `process_summaries.count_layer` to find hotspots:
  - `complete.data.frame` (192 calls in profiling)
  - Denominator lookups
  - Group-by operations

- [ ] **3.5** Implement optimizations:
  - Cache common computations at parent layer level
  - Pass pre-computed data to sublayers
  - Avoid re-computing denominators if already computed

- [ ] **3.6** Benchmark improvement

**Success Metric:** Reduce `process_summaries` execution time by 30%+ for nested layers

---

### Recommendation 4: Consider data.table for Large-Scale Operations

**Problem:** dplyr joins (`left_join`, `inner_join`, `full_join`) are optimized for convenience and readability, but `data.table` joins are 5-10x faster for large datasets.

**Solution:** For internal hotspot functions, use `data.table` syntax for joins and aggregations, then convert back to tibble for output compatibility.

**Files to modify:**
- `R/denom.R` - `get_denom_total_vectorized` function
- `R/nested.R` - `filter_nested_inner_layer_vectorized` function
- `R/count.R` - Any functions with heavy join usage

#### Checklist

- [ ] **4.1** Add data.table to Suggests in DESCRIPTION
  ```
  Suggests:
      data.table,
      ...
  ```

- [ ] **4.2** Create utility functions for conditional data.table usage
  ```r
  # In R/utils.R
  use_data_table <- function() {
    getOption("tplyr.use_data_table", default = FALSE) &&
      requireNamespace("data.table", quietly = TRUE)
  }

  fast_left_join <- function(x, y, by) {
    if (use_data_table()) {
      # data.table implementation
      x_dt <- data.table::as.data.table(x)
      y_dt <- data.table::as.data.table(y)
      result <- y_dt[x_dt, on = by]
      tibble::as_tibble(result)
    } else {
      dplyr::left_join(x, y, by = by)
    }
  }
  ```

- [ ] **4.3** Identify top 3 join hotspots to convert:
  1. `get_denom_total_vectorized` - left_join at line 269
  2. `filter_nested_inner_layer_vectorized` - inner_join at line 247
  3. `process_formatting.count_layer` - reduce/full_join

- [ ] **4.4** Implement data.table versions of hotspot joins

- [ ] **4.5** Add option to enable: `options(tplyr.use_data_table = TRUE)`

- [ ] **4.6** Write tests to verify identical results with both backends

- [ ] **4.7** Benchmark improvement
  ```r
  # Compare with and without data.table
  # Expected: 2-5x speedup on join-heavy operations
  ```

**Success Metric:** 2-5x speedup on large nested count tables when data.table is enabled

---

### Recommendation 5: Optimize String Operations in Formatting

**Problem:** `paste0` is called 189 times with significant time (278,774 units). String operations in `construct_count_string` and metadata building add up.

**Solution:** Use `sprintf` for formatted strings (faster than `paste0` for fixed patterns), and batch string operations.

**Files to modify:**
- `R/format.R` - `construct_count_string` function
- `R/meta-builders.R` - String construction in metadata builders

#### Checklist

- [ ] **5.1** Profile string operations specifically
  ```r
  # Look for paste0, paste, sprintf, str_* calls
  ```

- [ ] **5.2** Review `construct_count_string` in R/format.R
  - Is it called per-row or vectorized?
  - Can pattern matching use `sprintf` instead of `paste0`?

- [ ] **5.3** Benchmark `paste0` vs `sprintf` for the specific patterns used
  ```r
  # Example benchmark
  n <- 100000
  microbenchmark::microbenchmark(
    paste0 = paste0(1:n, " (", 1:n, ")"),
    sprintf = sprintf("%d (%d)", 1:n, 1:n)
  )
  ```

- [ ] **5.4** Replace `paste0` with `sprintf` where beneficial

- [ ] **5.5** Ensure string operations are fully vectorized (not in loops)

- [ ] **5.6** Benchmark improvement

**Success Metric:** Reduce string operation time by 30%+

---

## Execution Order

**Recommended sequence (highest impact first):**

1. **Recommendation 2** - Reduce join frequency in nested layers (highest impact for nested counts)
2. **Recommendation 1** - Batch metadata preparation (high call count reduction)
3. **Recommendation 3** - Optimize process_summaries chain (cumulative time savings)
4. **Recommendation 4** - data.table for joins (optional, significant speedup)
5. **Recommendation 5** - String operation optimization (lower priority)

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
| 1. Batch metadata | Not started | - | - | - |
| 2. Reduce joins | Not started | - | - | - |
| 3. Optimize process_summaries | Not started | - | - | - |
| 4. data.table joins | Not started | - | - | - |
| 5. String operations | Not started | - | - | - |

**Baseline:** Run the benchmark script above to establish baseline timing before any optimizations.
