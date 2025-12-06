# Performance Baseline for Tplyr Refactoring
# This script establishes performance baselines for key functions before refactoring
# Run this script before starting refactoring to capture baseline metrics

library(Tplyr)
library(dplyr)
library(bench)

# Load test data
data(tplyr_adsl)
data(tplyr_adae)
data(tplyr_adlb)

cat("=== Tplyr Performance Baseline ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R Version:", R.version.string, "\n")
cat("Tplyr Version:", packageVersion("Tplyr"), "\n\n")

# Helper function to format benchmark results
format_bench <- function(bench_result) {
  summary <- summary(bench_result)
  data.frame(
    median = as.character(summary$median),
    mean = as.character(summary$mean),
    min = as.character(summary$min),
    max = as.character(summary$max),
    mem_alloc = as.character(summary$mem_alloc)
  )
}

# ============================================================================
# 1. Table Pre-Processing Functions
# ============================================================================

cat("## 1. Table Pre-Processing Functions\n\n")

# 1.1 treatment_group_build() - Core table building
cat("### 1.1 treatment_group_build()\n")
bench_treatment_group <- mark(
  {
    t <- tplyr_table(tplyr_adsl, TRT01A) %>%
      add_treat_grps("Xanomeline" = c("Xanomeline High Dose", "Xanomeline Low Dose"))
    # This triggers treatment_group_build internally
    build(t)
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_treatment_group))
cat("\n")

# 1.2 build_header_n() - Header N calculation
cat("### 1.2 build_header_n()\n")
bench_header_n <- mark(
  {
    t <- tplyr_table(tplyr_adae, TRTA) %>%
      set_pop_data(tplyr_adsl) %>%
      set_pop_treat_var(TRT01A) %>%
      add_layer(group_count(AEDECOD))
    # This triggers build_header_n internally
    build(t)
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_header_n))
cat("\n")

# ============================================================================
# 2. Count Layer Functions
# ============================================================================

cat("## 2. Count Layer Functions\n\n")

# 2.1 Simple count layer
cat("### 2.1 Simple Count Layer\n")
bench_count_simple <- mark(
  {
    tplyr_table(tplyr_adsl, TRT01A) %>%
      add_layer(group_count(RACE)) %>%
      build()
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_count_simple))
cat("\n")

# 2.2 Count layer with by variables
cat("### 2.2 Count Layer with By Variables\n")
bench_count_by <- mark(
  {
    tplyr_table(tplyr_adsl, TRT01A) %>%
      add_layer(group_count(RACE, by = SEX)) %>%
      build()
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_count_by))
cat("\n")

# 2.3 Nested count layer
cat("### 2.3 Nested Count Layer\n")
bench_count_nested <- mark(
  {
    tplyr_table(tplyr_adae, TRTA) %>%
      add_layer(group_count(vars(AEBODSYS, AEDECOD))) %>%
      build()
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_count_nested))
cat("\n")

# 2.4 Count layer with distinct
cat("### 2.4 Count Layer with Distinct\n")
bench_count_distinct <- mark(
  {
    tplyr_table(tplyr_adae, TRTA) %>%
      add_layer(
        group_count(AEDECOD) %>%
          set_distinct_by(USUBJID)
      ) %>%
      build()
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_count_distinct))
cat("\n")

# 2.5 Count layer with total row
cat("### 2.5 Count Layer with Total Row\n")
bench_count_total <- mark(
  {
    tplyr_table(tplyr_adsl, TRT01A) %>%
      add_layer(
        group_count(RACE) %>%
          add_total_row()
      ) %>%
      build()
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_count_total))
cat("\n")

# ============================================================================
# 3. Desc Layer Functions
# ============================================================================

cat("## 3. Desc Layer Functions\n\n")

# 3.1 Simple desc layer
cat("### 3.1 Simple Desc Layer\n")
bench_desc_simple <- mark(
  {
    tplyr_table(tplyr_adlb, TRTA) %>%
      add_layer(group_desc(AVAL, by = PARAMCD)) %>%
      build()
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_desc_simple))
cat("\n")

# 3.2 Desc layer with custom summaries
cat("### 3.2 Desc Layer with Custom Summaries\n")
bench_desc_custom <- mark(
  {
    tplyr_table(tplyr_adlb, TRTA) %>%
      add_layer(
        group_desc(AVAL, by = PARAMCD) %>%
          set_format_strings(
            "n" = f_str("xx", n),
            "Mean (SD)" = f_str("xx.x (xx.xx)", mean, sd)
          )
      ) %>%
      build()
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_desc_custom))
cat("\n")

# ============================================================================
# 4. Shift Layer Functions
# ============================================================================

cat("## 4. Shift Layer Functions\n\n")

# 4.1 Shift layer
cat("### 4.1 Shift Layer\n")
bench_shift <- mark(
  {
    tplyr_table(tplyr_adlb, TRTA) %>%
      add_layer(
        group_shift(vars(row = BNRIND, column = ANRIND), by = PARAMCD)
      ) %>%
      build()
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_shift))
cat("\n")

# ============================================================================
# 5. Complex Multi-Layer Tables
# ============================================================================

cat("## 5. Complex Multi-Layer Tables\n\n")

# 5.1 Multi-layer table
cat("### 5.1 Multi-Layer Table\n")
bench_multi_layer <- mark(
  {
    tplyr_table(tplyr_adsl, TRT01A) %>%
      add_layer(group_count(RACE)) %>%
      add_layer(group_count(SEX)) %>%
      add_layer(group_desc(AGE)) %>%
      build()
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_multi_layer))
cat("\n")

# 5.2 Complex AE table
cat("### 5.2 Complex AE Table\n")
bench_complex_ae <- mark(
  {
    tplyr_table(tplyr_adae, TRTA) %>%
      set_pop_data(tplyr_adsl) %>%
      set_pop_treat_var(TRT01A) %>%
      add_layer(
        group_count(vars(AEBODSYS, AEDECOD)) %>%
          set_distinct_by(USUBJID) %>%
          set_order_count_method("bycount") %>%
          set_ordering_cols("Xanomeline High Dose")
      ) %>%
      build()
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_complex_ae))
cat("\n")

# ============================================================================
# 6. Metadata Generation
# ============================================================================

cat("## 6. Metadata Generation\n\n")

# 6.1 Count layer with metadata
cat("### 6.1 Count Layer with Metadata\n")
bench_metadata_count <- mark(
  {
    tplyr_table(tplyr_adsl, TRT01A) %>%
      add_layer(group_count(RACE)) %>%
      build(metadata = TRUE)
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_metadata_count))
cat("\n")

# 6.2 Desc layer with metadata
cat("### 6.2 Desc Layer with Metadata\n")
bench_metadata_desc <- mark(
  {
    tplyr_table(tplyr_adlb, TRTA) %>%
      add_layer(group_desc(AVAL, by = PARAMCD)) %>%
      build(metadata = TRUE)
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_metadata_desc))
cat("\n")

# ============================================================================
# 7. Sorting Functions
# ============================================================================

cat("## 7. Sorting Functions\n\n")

# 7.1 Sort by count
cat("### 7.1 Sort by Count\n")
bench_sort_count <- mark(
  {
    tplyr_table(tplyr_adsl, TRT01A) %>%
      add_layer(
        group_count(RACE) %>%
          set_order_count_method("bycount") %>%
          set_ordering_cols("Xanomeline High Dose")
      ) %>%
      build()
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_sort_count))
cat("\n")

# 7.2 Sort by variable
cat("### 7.2 Sort by Variable\n")
bench_sort_var <- mark(
  {
    tplyr_table(tplyr_adsl, TRT01A) %>%
      add_layer(
        group_count(RACE) %>%
          set_order_count_method("byvarn")
      ) %>%
      build()
  },
  iterations = 50,
  check = FALSE
)
print(format_bench(bench_sort_var))
cat("\n")

# ============================================================================
# Summary
# ============================================================================

cat("## Summary\n\n")
cat("Baseline performance metrics captured successfully.\n")
cat("These metrics should be compared against post-refactoring performance.\n")
cat("Acceptable performance degradation: < 10%\n\n")

# Save all benchmark results to RDS for later comparison
baseline_results <- list(
  date = Sys.time(),
  r_version = R.version.string,
  tplyr_version = as.character(packageVersion("Tplyr")),
  benchmarks = list(
    treatment_group = bench_treatment_group,
    header_n = bench_header_n,
    count_simple = bench_count_simple,
    count_by = bench_count_by,
    count_nested = bench_count_nested,
    count_distinct = bench_count_distinct,
    count_total = bench_count_total,
    desc_simple = bench_desc_simple,
    desc_custom = bench_desc_custom,
    shift = bench_shift,
    multi_layer = bench_multi_layer,
    complex_ae = bench_complex_ae,
    metadata_count = bench_metadata_count,
    metadata_desc = bench_metadata_desc,
    sort_count = bench_sort_count,
    sort_var = bench_sort_var
  )
)

saveRDS(baseline_results, ".kiro/specs/tplyr-refactor/performance-baseline.rds")
cat("Baseline results saved to: .kiro/specs/tplyr-refactor/performance-baseline.rds\n")
