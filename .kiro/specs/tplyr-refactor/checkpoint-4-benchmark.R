# Checkpoint 4: Performance Benchmark for Table-Level Functions
# This script benchmarks the refactored table-level functions

library(Tplyr)
library(dplyr)
library(bench)

# Load test data
data(tplyr_adsl)
data(tplyr_adae)

cat("=== Checkpoint 4: Table-Level Functions Performance ===\n")
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
# 1. treatment_group_build() Performance
# ============================================================================

cat("## 1. treatment_group_build() Performance\n\n")

cat("### 1.1 Basic table build (triggers treatment_group_build)\n")
bench_treatment_basic <- mark(
  {
    t <- tplyr_table(tplyr_adsl, TRT01A)
    build(t)
  },
  iterations = 100,
  check = FALSE
)
print(format_bench(bench_treatment_basic))
cat("\n")

cat("### 1.2 Table with treatment groups\n")
bench_treatment_groups <- mark(
  {
    t <- tplyr_table(tplyr_adsl, TRT01A) %>%
      add_treat_grps("Xanomeline" = c("Xanomeline High Dose", "Xanomeline Low Dose"))
    build(t)
  },
  iterations = 100,
  check = FALSE
)
print(format_bench(bench_treatment_groups))
cat("\n")

cat("### 1.3 Table with where clause\n")
bench_treatment_where <- mark(
  {
    t <- tplyr_table(tplyr_adsl, TRT01A) %>%
      set_where(SAFFL == "Y")
    build(t)
  },
  iterations = 100,
  check = FALSE
)
print(format_bench(bench_treatment_where))
cat("\n")

# ============================================================================
# 2. build_header_n() Performance
# ============================================================================

cat("## 2. build_header_n() Performance\n\n")

cat("### 2.1 Header N with population data\n")
bench_header_n <- mark(
  {
    t <- tplyr_table(tplyr_adae, TRTA) %>%
      set_pop_data(tplyr_adsl) %>%
      set_pop_treat_var(TRT01A) %>%
      add_layer(group_count(AEDECOD))
    build(t)
  },
  iterations = 100,
  check = FALSE
)
print(format_bench(bench_header_n))
cat("\n")

cat("### 2.2 Header N with column grouping\n")
bench_header_n_cols <- mark(
  {
    t <- tplyr_table(tplyr_adsl, TRT01A, cols = SEX)
    build(t)
  },
  iterations = 100,
  check = FALSE
)
print(format_bench(bench_header_n_cols))
cat("\n")

# ============================================================================
# 3. Combined Table Build
# ============================================================================

cat("## 3. Combined Table Build (Both Functions)\n\n")

cat("### 3.1 Simple table with count layer\n")
bench_combined_simple <- mark(
  {
    tplyr_table(tplyr_adsl, TRT01A) %>%
      add_layer(group_count(RACE)) %>%
      build()
  },
  iterations = 100,
  check = FALSE
)
print(format_bench(bench_combined_simple))
cat("\n")

cat("### 3.2 Complex table with population data and treatment groups\n")
bench_combined_complex <- mark(
  {
    tplyr_table(tplyr_adae, TRTA) %>%
      set_pop_data(tplyr_adsl) %>%
      set_pop_treat_var(TRT01A) %>%
      add_treat_grps("Xanomeline" = c("Xanomeline High Dose", "Xanomeline Low Dose")) %>%
      set_where(SAFFL == "Y") %>%
      add_layer(group_count(AEDECOD)) %>%
      build()
  },
  iterations = 100,
  check = FALSE
)
print(format_bench(bench_combined_complex))
cat("\n")

# ============================================================================
# 4. Load and Compare with Baseline (if available)
# ============================================================================

cat("## 4. Comparison with Baseline\n\n")

baseline_file <- ".kiro/specs/tplyr-refactor/performance-baseline.rds"
if (file.exists(baseline_file)) {
  baseline <- readRDS(baseline_file)
  
  cat("Baseline captured on:", format(baseline$date, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Baseline Tplyr version:", baseline$tplyr_version, "\n\n")
  
  # Compare treatment_group_build
  if (!is.null(baseline$benchmarks$treatment_group)) {
    baseline_median <- summary(baseline$benchmarks$treatment_group)$median
    current_median <- summary(bench_treatment_groups)$median
    ratio <- as.numeric(current_median) / as.numeric(baseline_median)
    pct_change <- (ratio - 1) * 100
    
    cat("### treatment_group_build comparison:\n")
    cat("  Baseline median:", baseline_median, "\n")
    cat("  Current median: ", current_median, "\n")
    cat("  Change:         ", sprintf("%.2f%%", pct_change), "\n")
    
    if (abs(pct_change) < 10) {
      cat("  Status:         ✓ PASS (within 10% threshold)\n\n")
    } else {
      cat("  Status:         ✗ WARNING (exceeds 10% threshold)\n\n")
    }
  }
  
  # Compare build_header_n
  if (!is.null(baseline$benchmarks$header_n)) {
    baseline_median <- summary(baseline$benchmarks$header_n)$median
    current_median <- summary(bench_header_n)$median
    ratio <- as.numeric(current_median) / as.numeric(baseline_median)
    pct_change <- (ratio - 1) * 100
    
    cat("### build_header_n comparison:\n")
    cat("  Baseline median:", baseline_median, "\n")
    cat("  Current median: ", current_median, "\n")
    cat("  Change:         ", sprintf("%.2f%%", pct_change), "\n")
    
    if (abs(pct_change) < 10) {
      cat("  Status:         ✓ PASS (within 10% threshold)\n\n")
    } else {
      cat("  Status:         ✗ WARNING (exceeds 10% threshold)\n\n")
    }
  }
} else {
  cat("No baseline file found. This will serve as the baseline.\n")
  cat("Run performance-baseline.R to establish a pre-refactoring baseline.\n\n")
}

# ============================================================================
# Summary
# ============================================================================

cat("## Summary\n\n")
cat("✓ All table-level function benchmarks completed\n")
cat("✓ Performance metrics captured for:\n")
cat("  - treatment_group_build()\n")
cat("  - build_header_n()\n")
cat("\n")

# Save checkpoint results
checkpoint_results <- list(
  date = Sys.time(),
  r_version = R.version.string,
  tplyr_version = as.character(packageVersion("Tplyr")),
  benchmarks = list(
    treatment_basic = bench_treatment_basic,
    treatment_groups = bench_treatment_groups,
    treatment_where = bench_treatment_where,
    header_n = bench_header_n,
    header_n_cols = bench_header_n_cols,
    combined_simple = bench_combined_simple,
    combined_complex = bench_combined_complex
  )
)

saveRDS(checkpoint_results, ".kiro/specs/tplyr-refactor/checkpoint-4-results.rds")
cat("Checkpoint results saved to: .kiro/specs/tplyr-refactor/checkpoint-4-results.rds\n")
