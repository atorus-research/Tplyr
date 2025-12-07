# Checkpoint 4: Simple Performance Benchmark for Table-Level Functions
# This script benchmarks the refactored table-level functions using base R

library(Tplyr)
library(dplyr)

# Load test data
data(tplyr_adsl)
data(tplyr_adae)

cat("=== Checkpoint 4: Table-Level Functions Performance ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R Version:", R.version.string, "\n")
cat("Tplyr Version:", as.character(packageVersion("Tplyr")), "\n\n")

# Helper function to run benchmark
run_benchmark <- function(expr, name, iterations = 100) {
  cat("Testing:", name, "\n")
  
  # Warm-up
  for (i in 1:5) {
    eval(expr)
  }
  
  # Actual timing
  times <- numeric(iterations)
  for (i in 1:iterations) {
    start <- Sys.time()
    eval(expr)
    end <- Sys.time()
    times[i] <- as.numeric(end - start, units = "secs")
  }
  
  cat("  Iterations:", iterations, "\n")
  cat("  Median:    ", sprintf("%.4f", median(times)), "seconds\n")
  cat("  Mean:      ", sprintf("%.4f", mean(times)), "seconds\n")
  cat("  Min:       ", sprintf("%.4f", min(times)), "seconds\n")
  cat("  Max:       ", sprintf("%.4f", max(times)), "seconds\n")
  cat("  SD:        ", sprintf("%.4f", sd(times)), "seconds\n\n")
  
  return(times)
}

# ============================================================================
# 1. treatment_group_build() Performance
# ============================================================================

cat("## 1. treatment_group_build() Performance\n\n")

times_treatment_basic <- run_benchmark(
  quote({
    t <- tplyr_table(tplyr_adsl, TRT01A) %>%
      add_layer(group_count(RACE))
    build(t)
  }),
  "Basic table build (triggers treatment_group_build)",
  iterations = 50
)

times_treatment_groups <- run_benchmark(
  quote({
    t <- tplyr_table(tplyr_adsl, TRT01A) %>%
      add_treat_grps("Xanomeline" = c("Xanomeline High Dose", "Xanomeline Low Dose")) %>%
      add_layer(group_count(RACE))
    build(t)
  }),
  "Table with treatment groups",
  iterations = 50
)

times_treatment_where <- run_benchmark(
  quote({
    t <- tplyr_table(tplyr_adsl, TRT01A) %>%
      set_where(SAFFL == "Y") %>%
      add_layer(group_count(RACE))
    build(t)
  }),
  "Table with where clause",
  iterations = 50
)

# ============================================================================
# 2. build_header_n() Performance
# ============================================================================

cat("## 2. build_header_n() Performance\n\n")

times_header_n <- run_benchmark(
  quote({
    t <- tplyr_table(tplyr_adae, TRTA) %>%
      set_pop_data(tplyr_adsl) %>%
      set_pop_treat_var(TRT01A) %>%
      add_layer(group_count(AEDECOD))
    build(t)
  }),
  "Header N with population data",
  iterations = 50
)

times_header_n_cols <- run_benchmark(
  quote({
    t <- tplyr_table(tplyr_adsl, TRT01A, cols = SEX) %>%
      add_layer(group_count(RACE))
    build(t)
  }),
  "Header N with column grouping",
  iterations = 50
)

# ============================================================================
# 3. Combined Table Build
# ============================================================================

cat("## 3. Combined Table Build (Both Functions)\n\n")

times_combined_simple <- run_benchmark(
  quote({
    tplyr_table(tplyr_adsl, TRT01A) %>%
      add_layer(group_count(RACE)) %>%
      build()
  }),
  "Simple table with count layer",
  iterations = 50
)

times_combined_complex <- run_benchmark(
  quote({
    tplyr_table(tplyr_adae, TRTA) %>%
      set_pop_data(tplyr_adsl) %>%
      set_pop_treat_var(TRT01A) %>%
      add_treat_grps("Xanomeline" = c("Xanomeline High Dose", "Xanomeline Low Dose")) %>%
      set_where(SAFFL == "Y") %>%
      add_layer(group_count(AEDECOD)) %>%
      build()
  }),
  "Complex table with population data and treatment groups",
  iterations = 50
)

# ============================================================================
# Summary
# ============================================================================

cat("## Summary\n\n")
cat("✓ All table-level function benchmarks completed\n")
cat("✓ Performance metrics captured for:\n")
cat("  - treatment_group_build()\n")
cat("  - build_header_n()\n")
cat("\n")

cat("Key Performance Metrics:\n")
cat("  Basic table build:        ", sprintf("%.4f", median(times_treatment_basic)), "s (median)\n")
cat("  With treatment groups:    ", sprintf("%.4f", median(times_treatment_groups)), "s (median)\n")
cat("  With where clause:        ", sprintf("%.4f", median(times_treatment_where)), "s (median)\n")
cat("  Header N (pop data):      ", sprintf("%.4f", median(times_header_n)), "s (median)\n")
cat("  Header N (cols):          ", sprintf("%.4f", median(times_header_n_cols)), "s (median)\n")
cat("  Simple combined:          ", sprintf("%.4f", median(times_combined_simple)), "s (median)\n")
cat("  Complex combined:         ", sprintf("%.4f", median(times_combined_complex)), "s (median)\n")
cat("\n")

cat("Note: These benchmarks establish the post-refactoring performance baseline.\n")
cat("The refactored functions use the Extract-Process-Bind pattern instead of evalq().\n")
cat("Performance should be comparable to pre-refactoring (within 10%).\n")

# Save results
checkpoint_results <- list(
  date = Sys.time(),
  r_version = R.version.string,
  tplyr_version = as.character(packageVersion("Tplyr")),
  benchmarks = list(
    treatment_basic = times_treatment_basic,
    treatment_groups = times_treatment_groups,
    treatment_where = times_treatment_where,
    header_n = times_header_n,
    header_n_cols = times_header_n_cols,
    combined_simple = times_combined_simple,
    combined_complex = times_combined_complex
  )
)

saveRDS(checkpoint_results, ".kiro/specs/tplyr-refactor/checkpoint-4-results.rds")
cat("\nCheckpoint results saved to: .kiro/specs/tplyr-refactor/checkpoint-4-results.rds\n")
