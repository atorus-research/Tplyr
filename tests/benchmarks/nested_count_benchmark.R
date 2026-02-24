# Nested Count Layer Performance Benchmarks
#
# This script establishes baseline performance metrics for nested count layers.
# Run this before and after the refactor to measure improvement.
#
# Usage:
#   devtools::load_all()  # or library(Tplyr)
#   source("tests/benchmarks/nested_count_benchmark.R")
#   results <- run_nested_benchmarks()
#   print(results)
#
#   # For realistic AE-style benchmarks:
#   ae_results <- run_ae_benchmarks()
#   print(ae_results)

library(dplyr)
library(tidyr)

# Generate synthetic data for benchmarking
generate_benchmark_data <- function(n_rows, n_outer = 10, n_inner_per_outer = 5, n_subjects = NULL) {
  if (is.null(n_subjects)) {
    n_subjects <- ceiling(n_rows / 10)
  }

  # Create outer/inner combinations
  outer_vals <- paste0("OUTER_", sprintf("%02d", seq_len(n_outer)))
  inner_template <- paste0("INNER_", sprintf("%02d", seq_len(n_inner_per_outer)))

  # Each outer value has its own set of inner values

  combinations <- expand.grid(
    outer = outer_vals,
    inner_suffix = seq_len(n_inner_per_outer),
    stringsAsFactors = FALSE
  ) %>%
    mutate(inner = paste0(outer, "_", inner_suffix)) %>%
    select(outer, inner)

  # Generate data

  data <- tibble(
    USUBJID = paste0("SUBJ_", sprintf("%04d", sample(seq_len(n_subjects), n_rows, replace = TRUE))),
    TRT = sample(c("Placebo", "Low Dose", "High Dose"), n_rows, replace = TRUE),
    OUTER_VAR = sample(outer_vals, n_rows, replace = TRUE)
  ) %>%
    rowwise() %>%
    mutate(
      INNER_VAR = {
        valid_inner <- combinations$inner[combinations$outer == OUTER_VAR]
        sample(valid_inner, 1)
      }
    ) %>%
    ungroup() %>%
    mutate(
      SEX = sample(c("M", "F"), n_rows, replace = TRUE),
      AGEGR = sample(c("<65", ">=65"), n_rows, replace = TRUE)
    )

  data
}

# Generate population data
generate_pop_data <- function(benchmark_data) {
  benchmark_data %>%
    distinct(USUBJID, TRT) %>%
    rename(TRT01A = TRT)
}

# Benchmark a single scenario
benchmark_scenario <- function(name, data, pop_data = NULL, n_iterations = 5) {
  times <- numeric(n_iterations)

  for (i in seq_len(n_iterations)) {
    start_time <- Sys.time()

    t <- tplyr_table(data, TRT)

    if (!is.null(pop_data)) {
      t <- t %>%
        set_pop_data(pop_data) %>%
        set_pop_treat_var(TRT01A)
    }

    t <- t %>%
      add_layer(
        group_count(vars(OUTER_VAR, INNER_VAR)) %>%
          set_distinct_by(USUBJID)
      ) %>%
      build()

    end_time <- Sys.time()
    times[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
  }

  list(
    scenario = name,
    mean_time = mean(times),
    median_time = median(times),
    min_time = min(times),
    max_time = max(times),
    sd_time = sd(times),
    n_iterations = n_iterations,
    n_rows = nrow(data)
  )
}

# Benchmark with by variables
benchmark_with_by <- function(name, data, pop_data = NULL, n_iterations = 5) {
  times <- numeric(n_iterations)

  for (i in seq_len(n_iterations)) {
    start_time <- Sys.time()

    t <- tplyr_table(data, TRT)

    if (!is.null(pop_data)) {
      t <- t %>%
        set_pop_data(pop_data) %>%
        set_pop_treat_var(TRT01A)
    }

    t <- t %>%
      add_layer(
        group_count(vars(OUTER_VAR, INNER_VAR), by = SEX) %>%
          set_distinct_by(USUBJID)
      ) %>%
      build()

    end_time <- Sys.time()
    times[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
  }

  list(
    scenario = name,
    mean_time = mean(times),
    median_time = median(times),
    min_time = min(times),
    max_time = max(times),
    sd_time = sd(times),
    n_iterations = n_iterations,
    n_rows = nrow(data)
  )
}

# Benchmark multiple nested layers
benchmark_multiple_layers <- function(name, data, pop_data = NULL, n_iterations = 5) {
  times <- numeric(n_iterations)

  for (i in seq_len(n_iterations)) {
    start_time <- Sys.time()

    t <- tplyr_table(data, TRT)

    if (!is.null(pop_data)) {
      t <- t %>%
        set_pop_data(pop_data) %>%
        set_pop_treat_var(TRT01A)
    }

    t <- t %>%
      add_layer(
        group_count(vars(OUTER_VAR, INNER_VAR)) %>%
          set_distinct_by(USUBJID)
      ) %>%
      add_layer(
        group_count(vars(OUTER_VAR, INNER_VAR), by = SEX) %>%
          set_distinct_by(USUBJID)
      ) %>%
      build()

    end_time <- Sys.time()
    times[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
  }

  list(
    scenario = name,
    mean_time = mean(times),
    median_time = median(times),
    min_time = min(times),
    max_time = max(times),
    sd_time = sd(times),
    n_iterations = n_iterations,
    n_rows = nrow(data)
  )
}

# Compare nested vs non-nested performance
benchmark_nested_vs_flat <- function(data, n_iterations = 5) {
  # Nested benchmark
  nested_times <- numeric(n_iterations)
  for (i in seq_len(n_iterations)) {
    start_time <- Sys.time()
    t <- tplyr_table(data, TRT) %>%
      add_layer(
        group_count(vars(OUTER_VAR, INNER_VAR)) %>%
          set_distinct_by(USUBJID)
      ) %>%
      build()
    nested_times[i] <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  }

  # Flat (single variable) benchmark
  flat_times <- numeric(n_iterations)
  for (i in seq_len(n_iterations)) {
    start_time <- Sys.time()
    t <- tplyr_table(data, TRT) %>%
      add_layer(
        group_count(OUTER_VAR) %>%
          set_distinct_by(USUBJID)
      ) %>%
      build()
    flat_times[i] <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  }

  list(
    nested_mean = mean(nested_times),
    flat_mean = mean(flat_times),
    ratio = mean(nested_times) / mean(flat_times),
    n_rows = nrow(data)
  )
}

# Main benchmark runner
run_nested_benchmarks <- function(verbose = TRUE) {
  results <- list()

  if (verbose) cat("Generating benchmark data...\n")

 # Small data
  if (verbose) cat("  - Small dataset (1K rows)\n")
  small_data <- generate_benchmark_data(1000, n_outer = 5, n_inner_per_outer = 4)
  small_pop <- generate_pop_data(small_data)

  # Medium data
  if (verbose) cat("  - Medium dataset (10K rows)\n")
  medium_data <- generate_benchmark_data(10000, n_outer = 10, n_inner_per_outer = 5)
  medium_pop <- generate_pop_data(medium_data)

  # Large data
  if (verbose) cat("  - Large dataset (100K rows)\n")
  large_data <- generate_benchmark_data(100000, n_outer = 20, n_inner_per_outer = 10)
  large_pop <- generate_pop_data(large_data)

  if (verbose) cat("\nRunning benchmarks...\n")

  # Basic nested count scenarios
  if (verbose) cat("  1. Small data - basic nested\n")
  results$small_basic <- benchmark_scenario("small_basic", small_data, small_pop)

  if (verbose) cat("  2. Medium data - basic nested\n")
  results$medium_basic <- benchmark_scenario("medium_basic", medium_data, medium_pop)

  if (verbose) cat("  3. Large data - basic nested\n")
  results$large_basic <- benchmark_scenario("large_basic", large_data, large_pop)

  # With by variables
  if (verbose) cat("  4. Medium data - with by variable\n")
  results$medium_with_by <- benchmark_with_by("medium_with_by", medium_data, medium_pop)

  if (verbose) cat("  5. Large data - with by variable\n")
  results$large_with_by <- benchmark_with_by("large_with_by", large_data, large_pop)

  # Multiple layers
  if (verbose) cat("  6. Medium data - multiple nested layers\n")
  results$medium_multi <- benchmark_multiple_layers("medium_multi", medium_data, medium_pop)

  # Nested vs flat comparison
  if (verbose) cat("  7. Nested vs flat comparison\n")
  results$nested_vs_flat <- benchmark_nested_vs_flat(medium_data)

  if (verbose) cat("\nBenchmarks complete.\n")

  results
}

# Format results as a data frame for easy viewing
format_benchmark_results <- function(results) {
  # Extract timing results
  timing_results <- results[!names(results) %in% "nested_vs_flat"]

  timing_df <- bind_rows(lapply(timing_results, function(r) {
    tibble(
      scenario = r$scenario,
      n_rows = r$n_rows,
      mean_sec = round(r$mean_time, 4),
      median_sec = round(r$median_time, 4),
      min_sec = round(r$min_time, 4),
      max_sec = round(r$max_time, 4)
    )
  }))

  # Add nested vs flat comparison
  nvf <- results$nested_vs_flat
  comparison <- tibble(
    metric = c("Nested mean (sec)", "Flat mean (sec)", "Nested/Flat ratio"),
    value = c(round(nvf$nested_mean, 4), round(nvf$flat_mean, 4), round(nvf$ratio, 2))
  )

  list(
    timing = timing_df,
    nested_vs_flat = comparison
  )
}

# Save results to file
save_benchmark_results <- function(results, filename = "benchmark_results.rds") {
  results$timestamp <- Sys.time()
  results$r_version <- R.version.string
  results$tplyr_version <- as.character(packageVersion("Tplyr"))

  saveRDS(results, file.path("tests/benchmarks", filename))
  cat("Results saved to:", file.path("tests/benchmarks", filename), "\n")
}

# Compare two benchmark result sets
compare_benchmarks <- function(before, after) {
  before_timing <- format_benchmark_results(before)$timing
  after_timing <- format_benchmark_results(after)$timing

  comparison <- before_timing %>%
    select(scenario, n_rows, before_mean = mean_sec) %>%
    left_join(
      after_timing %>% select(scenario, after_mean = mean_sec),
      by = "scenario"
    ) %>%
    mutate(
      speedup = round(before_mean / after_mean, 2),
      improvement_pct = round((1 - after_mean / before_mean) * 100, 1)
    )

  comparison
}

# =============================================================================
# Realistic AE-Style Benchmarks
# =============================================================================

#' Build multiplied AE data for benchmarking
#'
#' Creates test data by replicating tplyr_adae with unique AEBODSYS/AEDECOD values
#'
#' @param multiplier Number of times to replicate the base data
#' @return A data frame with multiplied AE data
build_ae_benchmark_data <- function(multiplier) {
  do.call(rbind, lapply(seq_len(multiplier), function(i) {
    temp <- Tplyr::tplyr_adae
    temp$AEBODSYS <- paste0(temp$AEBODSYS, "_", i)
    temp$AEDECOD <- paste0(temp$AEDECOD, "_", i)
    temp
  }))
}

#' Run a single AE benchmark scenario
#'
#' @param multiplier Scale multiplier for data
#' @param n_iterations Number of timing iterations
#' @param verbose Print progress
#' @return List with benchmark results
benchmark_ae_scenario <- function(multiplier, n_iterations = 3, verbose = TRUE) {
  adae <- build_ae_benchmark_data(multiplier)

  n_rows <- nrow(adae)
  n_outer <- length(unique(adae$AEBODSYS))
  n_inner <- length(unique(adae$AEDECOD))

  if (verbose) {
    cat(sprintf("  %dx: %d rows, %d outer, %d inner... ",
                multiplier, n_rows, n_outer, n_inner))
  }

  times <- numeric(n_iterations)
  for (i in seq_len(n_iterations)) {
    start <- Sys.time()
    t <- Tplyr::tplyr_table(adae, TRTA) |>
      Tplyr::set_pop_data(Tplyr::tplyr_adsl) |>
      Tplyr::set_pop_treat_var(TRT01A) |>
      Tplyr::add_layer(
        Tplyr::group_count(vars(AEBODSYS, AEDECOD)) |>
          Tplyr::set_nest_count(TRUE) |>
          Tplyr::set_distinct_by(USUBJID) |>
          Tplyr::set_order_count_method("bycount", break_ties = "desc")
      ) |>
      Tplyr::build()
    times[i] <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  }

  if (verbose) {
    cat(sprintf("%.3f sec\n", mean(times)))
  }

  list(
    multiplier = multiplier,
    n_rows = n_rows,
    n_outer = n_outer,
    n_inner = n_inner,
    mean_time = mean(times),
    median_time = median(times),
    min_time = min(times),
    max_time = max(times),
    times = times
  )
}

#' Run full AE-style benchmark suite
#'
#' @param scales Vector of multipliers to test (default: c(10, 25, 50, 75, 100))
#' @param n_iterations Number of iterations per scenario
#' @param verbose Print progress
#' @return List with all benchmark results
run_ae_benchmarks <- function(scales = c(10, 25, 50, 75, 100),
                               n_iterations = 3,
                               verbose = TRUE) {
  if (verbose) cat("=== AE-Style Nested Count Benchmarks ===\n\n")

  results <- list()

  for (mult in scales) {
    results[[paste0("scale_", mult)]] <- benchmark_ae_scenario(
      mult,
      n_iterations = n_iterations,
      verbose = verbose
    )
  }

  if (verbose) cat("\nBenchmarks complete.\n")

  results
}

#' Format AE benchmark results as data frame
#'
#' @param results Output from run_ae_benchmarks()
#' @return Data frame with formatted results
format_ae_results <- function(results) {
  bind_rows(lapply(results, function(r) {
    tibble(
      multiplier = r$multiplier,
      n_rows = r$n_rows,
      n_outer = r$n_outer,
      n_inner = r$n_inner,
      mean_sec = round(r$mean_time, 3),
      median_sec = round(r$median_time, 3)
    )
  }))
}

#' Analyze complexity from benchmark results
#'
#' @param results Output from run_ae_benchmarks()
#' @return List with complexity analysis
analyze_complexity <- function(results) {
  df <- format_ae_results(results)

  # Fit models
  models <- list(
    linear = lm(mean_sec ~ n_outer, data = df),
    quadratic = lm(mean_sec ~ I(n_outer^2), data = df),
    nlogn = lm(mean_sec ~ I(n_outer * log(n_outer)), data = df),
    mixed = lm(mean_sec ~ n_outer + I(n_outer^2), data = df)
  )

  r_squared <- sapply(models, function(m) summary(m)$r.squared)

  # Time per outer value
  df$time_per_outer <- df$mean_sec / df$n_outer

  list(
    data = df,
    r_squared = r_squared,
    best_fit = names(which.max(r_squared)),
    time_per_outer = df$time_per_outer
  )
}

#' Compare AE benchmark results before and after refactor
#'
#' @param before Results from before refactor
#' @param after Results from after refactor
#' @return Data frame with comparison
compare_ae_benchmarks <- function(before, after) {
  before_df <- format_ae_results(before)
  after_df <- format_ae_results(after)

  comparison <- before_df %>%
    select(multiplier, n_rows, n_outer, before_sec = mean_sec) %>%
    left_join(
      after_df %>% select(multiplier, after_sec = mean_sec),
      by = "multiplier"
    ) %>%
    mutate(
      speedup = round(before_sec / after_sec, 2),
      improvement_pct = round((1 - after_sec / before_sec) * 100, 1)
    )

  comparison
}

#' Save AE benchmark results
#'
#' @param results Benchmark results
#' @param filename Filename (saved in tests/benchmarks/)
save_ae_results <- function(results, filename = "ae_benchmark_results.rds") {
  results$timestamp <- Sys.time()
  results$r_version <- R.version.string

  # Try to get Tplyr version
  results$tplyr_version <- tryCatch(
    as.character(packageVersion("Tplyr")),
    error = function(e) "dev"
  )

  filepath <- file.path("tests/benchmarks", filename)
  saveRDS(results, filepath)
  cat("Results saved to:", filepath, "\n")
}
