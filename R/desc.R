#' Process numeric data for a layer of type \code{desc}
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes data in function environment
#' 3. Binds results back to layer environment
#'
#' @param x Layer object
#'
#' @return Nothing
#' @export
#' @noRd
process_summaries.desc_layer <- function(x, ...) {

  # If format strings weren't provided, then grab the defaults
  if (!has_format_strings(x)) {
    # Grab the defaults available at the table or option level
    params <- gather_defaults(x)
    # Place the formats
    x <- do.call('set_format_strings', append(x, params))
  }

  # EXTRACT: Get needed bindings from layer environment (with inheritance from parent)
  built_target <- env_get(x, "built_target", inherit = TRUE)
  target_var <- x$target_var
  where <- x$where
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  by <- x$by
  cols <- env_get(x, "cols", inherit = TRUE)
  format_strings <- x$format_strings
  summary_vars <- x$summary_vars
  trans_vars <- x$trans_vars
  limit_data_by <- x$limit_data_by
  need_prec_table <- x$need_prec_table
  precision_on <- x$precision_on

  # PROCESS: Work in function environment
  # trans_sums is the data that will pass forward to be formatted
  trans_sums <- vector("list", length(target_var))
  # num_sums is the data that will be bound together and returned to provide
  # the numeric internal values
  # num_sums_raw is kept separate to better facililate use for prep of metadata
  num_sums_raw <- vector("list", length(target_var))
  num_sums <- vector("list", length(target_var))

  # Get the row labels out from the format strings list
  row_labels <- name_translator(format_strings)

  # Subset the local built_target based on where
  # Catch errors
  tryCatch({
    built_target <- built_target %>%
      filter(!!where)
  }, error = function(e) {
    abort(paste0("group_desc `where` condition `",
                 as_label(where),
                 "` is invalid. Filter error:\n", e))
  })

  # Extract the list of summaries that need to be performed
  for (i in seq_along(target_var)) {

    # Pull out the target variable being iterated
    cur_var <- target_var[[i]]

    # Get the summaries that need to be performed for this layer
    # Pass the layer environment so get_custom_summaries can find custom_summaries
    summaries <- get_summaries(e = x)[match_exact(summary_vars)]

    # Create the numeric summary data
    cmplt1 <- built_target %>%
      # Rename the current variable to make each iteration use a generic name
      rename(.var = !!cur_var) %>%
      # Group by treatment, provided by variable, and provided column variables
      group_by(!!treat_var, !!!by, !!!cols) %>%
      # Execute the summaries
      summarize(!!!summaries) %>%
      ungroup()

    num_sums_raw[[i]] <- complete_and_limit(cmplt1, treat_var, by, cols, limit_data_by=limit_data_by)

    # Create the transposed summary data to prepare for formatting
    trans_sums[[i]] <- num_sums_raw[[i]] %>%
      # Transpose the summaries that make up the first number in a display string
      # into the the `value` column with labels by `stat`
      pivot_longer(cols = match_exact(trans_vars), names_to = "stat") %>%
      rowwise() %>%
      # Add in the row labels
      mutate(
         row_label = row_labels[[stat]]
      )

    # If precision is required, then create the variable identifier
    if (need_prec_table) {
      trans_sums[[i]] <- trans_sums[[i]] %>%
        mutate(
          precision_on = as_name(precision_on)
        )
    }

    # Numeric data needs the variable names replaced and add summary variable name
    num_sums[[i]] <- replace_by_string_names(num_sums_raw[[i]], by) %>%
      mutate(summary_var = as_name(cur_var)) %>%
      select(summary_var, everything())
  }
  # Note: cur_var, summaries, i are local variables - no cleanup needed

  # Bind the numeric data together within the layer
  numeric_data <- pivot_longer(bind_rows(num_sums), cols = match_exact(summary_vars), names_to = "stat")

  # BIND: Write results back to layer environment
  x$trans_sums <- trans_sums
  x$num_sums_raw <- num_sums_raw
  x$numeric_data <- numeric_data

  invisible(x)
}

#' Format processing for desc layers
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes data in function environment
#' 3. Binds results back to layer environment
#'
#' @param x layer object
#'
#' @return Formatted and processed data
#' @noRd
#' @export
process_formatting.desc_layer <- function(x, ...) {

  # EXTRACT: Get needed bindings from layer environment (with inheritance from parent)
  trans_sums <- x$trans_sums
  target_var <- x$target_var
  need_prec_table <- x$need_prec_table
  format_strings <- x$format_strings
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  by <- x$by
  cols <- env_get(x, "cols", inherit = TRUE)
  # stats_as_columns may not exist, default to FALSE
  stats_as_columns <- if (exists("stats_as_columns", envir = x, inherits = FALSE)) {
    x$stats_as_columns
  } else {
    FALSE
  }

  # Extract precision-related bindings if needed
  if (need_prec_table) {
    built_target <- env_get(x, "built_target", inherit = TRUE)
    precision_by <- x$precision_by
    precision_on <- x$precision_on
    cap <- x$cap
    prec_error <- x$prec_error
    # Check if precision data was manually specified
    has_manual_prec <- exists("prec", envir = x, inherits = FALSE)
    if (has_manual_prec) {
      manual_prec <- x$prec
    }
  }

  # PROCESS: Work in function environment
  # Initialize list for formatted, transposed outputs
  form_sums <- vector("list", length(target_var))

  # Handle precision data if needed
  if (need_prec_table) {
    if (has_manual_prec) {
      # If precision data was manually specified, grab it
      prec <- get_prec_data(built_target, manual_prec, precision_by, precision_on, cap, prec_error)
    } else {
      # Otherwise create it
      prec <- make_prec_data(built_target, precision_by, precision_on, cap)
    }
  }

  for (i in seq_along(trans_sums)) {
    # Format the display strings - this is just applying construct_desc_string to each row of
    # the data.frame

    # Make a local copy to avoid modifying the original
    current_trans_sum <- trans_sums[[i]]

    if (need_prec_table) {
      # Merge the precision data on
      current_trans_sum <- left_join(current_trans_sum, prec, by=c(match_exact(precision_by), 'precision_on'))
    }

    # Format display strings using vectorized function
    current_trans_sum['display_string'] <- construct_desc_string_vec(current_trans_sum, format_strings)

    # Now do one more transpose to split the columns out
    # Default is to use the treatment variable, but if `cols` was provided
    # then also transpose by cols.
    if (stats_as_columns) {
      form_sums[[i]] <- current_trans_sum %>%
        # Select only columns needed for pivot to reduce memory footprint
        select(!!treat_var, match_exact(by), row_label, match_exact(cols), display_string) %>%
        pivot_wider(id_cols=c(!!treat_var, match_exact(by)), # Keep row_label and the by variables
                    names_from = match_exact(vars(row_label, !!!cols)), # Pull the names from treatment and cols argument
                    names_prefix = paste0('var', i, "_"), # Prefix with the name of the target variable
                    values_from = display_string # Use the created display_string variable for values
        )

    } else {
      form_sums[[i]] <- current_trans_sum %>%
        # Select only columns needed for pivot to reduce memory footprint
        select(row_label, match_exact(by), !!treat_var, match_exact(cols), display_string) %>%
        pivot_wider(id_cols=c('row_label', match_exact(by)), # Keep row_label and the by variables
                    names_from = match_exact(vars(!!treat_var, !!!cols)), # Pull the names from treatment and cols argument
                    names_prefix = paste0('var', i, "_"), # Prefix with the name of the target variable
                    values_from = display_string # Use the created display_string variable for values
      )

    }
  }
  # Note: form_sums, i, current_trans_sum, prec (if created) are local variables - no cleanup needed

  # Join the final outputs
  if (stats_as_columns) {
    formatted_data <- reduce(form_sums, full_join, by=c(as_label(treat_var), match_exact(by)))

    # Replace row label names
    formatted_data <- replace_by_string_names(formatted_data, by, treat_var)
  } else {
    formatted_data <- reduce(form_sums, full_join, by=c('row_label', match_exact(by)))

    # Replace row label names
    formatted_data <- replace_by_string_names(formatted_data, by)
  }

  formatted_data <- assign_row_id(formatted_data, 'd')

  # BIND: Write results back to layer environment
  x$formatted_data <- formatted_data

  add_order_columns(x)

  env_get(x, "formatted_data")
}

# Small helper function to help with builtins
inf_to_na <- function(x) if_else(is.infinite(x), NA, x)

#' Get the summaries to be passed forward into \code{dplyr::summarize()}
#'
#' @param e the environment summaries are stored in.
#'
#' @return A list of expressions to be unpacked in \code{dplyr::summarize}
#' @noRd
get_summaries <- function(e = caller_env()) {

  # Define the default list of summaries
  summaries <- exprs(
    n       = n(),
    mean    = mean(.var, na.rm=TRUE),
    sd      = sd(.var, na.rm=TRUE),
    median  = median(.var, na.rm=TRUE),
    var     = var(.var, na.rm=TRUE),
    min     = inf_to_na(min(.var, na.rm=TRUE)),
    max     = inf_to_na(max(.var, na.rm=TRUE)),
    iqr     = IQR(.var, na.rm=TRUE, type=getOption('tplyr.quantile_type')),
    q1      = quantile(.var, na.rm=TRUE, type=getOption('tplyr.quantile_type'))[[2]],
    q3      = quantile(.var, na.rm=TRUE, type=getOption('tplyr.quantile_type'))[[4]],
    missing = sum(is.na(.var))
  )

  append(summaries, get_custom_summaries(e), after=0)
}

#' Vectorized formatting for descriptive statistics display strings
#'
#' This function replaces the pmap_chr(data, construct_desc_string, ...) pattern
#' with a vectorized approach. It processes rows grouped by row_label since
#' each row_label has a potentially different format string.
#'
#' @param data Data frame containing the transposed summary data with columns:
#'   - value: the pivoted statistic value
#'   - row_label: the display label for the statistic
#'   - max_int, max_dec: (optional) auto-precision columns
#'   - Additional columns for keep_vars (e.g., sd when formatting "Mean (SD)")
#' @param format_strings Named list of f_str objects keyed by row_label
#'
#' @return Character vector of formatted display strings
#' @noRd
construct_desc_string_vec <- function(data, format_strings) {

  # Initialize result vector
  result <- character(nrow(data))

  # Get unique row labels to process
  unique_labels <- unique(data$row_label)

  for (label in unique_labels) {
    # Get the format for this row_label
    fmt <- format_strings[[label]]

    if (is.null(fmt)) {
      # Skip if no format found (shouldn't happen in normal use)
      next
    }

    # Get indices for rows with this label
    idx <- which(data$row_label == label)

    if (length(idx) == 0) next

    # Extract the subset of data for this label
    subset_data <- data[idx, , drop = FALSE]

    # Check if all values are NA - return empty string if so
    # Get the first variable value (in 'value' column) and any additional vars
    all_na_mask <- is.na(subset_data$value)

    # Check additional variables too
    for (var_expr in fmt$vars[-1]) {
      var_name <- as_name(var_expr)
      if (var_name %in% names(subset_data)) {
        all_na_mask <- all_na_mask & is.na(subset_data[[var_name]])
      }
    }

    # Handle rows where all values are NA
    if (any(all_na_mask) && '.overall' %in% names(fmt$empty)) {
      result[idx[all_na_mask]] <- fmt$empty['.overall']
    }

    # Process non-all-NA rows
    non_na_idx <- idx[!all_na_mask]

    if (length(non_na_idx) == 0) next

    non_na_data <- data[non_na_idx, , drop = FALSE]

    # Handle auto precision: when precision varies by row, sub-group by precision
    if (fmt$auto_precision && "max_int" %in% names(non_na_data) && "max_dec" %in% names(non_na_data)) {
      # Check if precision varies within this group
      unique_prec <- unique(non_na_data[, c("max_int", "max_dec"), drop = FALSE])

      if (nrow(unique_prec) > 1) {
        # Multiple precision levels - process each sub-group separately
        for (p_row in seq_len(nrow(unique_prec))) {
          p_int <- unique_prec$max_int[p_row]
          p_dec <- unique_prec$max_dec[p_row]

          # Find rows matching this precision
          prec_mask <- non_na_data$max_int == p_int & non_na_data$max_dec == p_dec
          prec_rows <- which(prec_mask)
          prec_idx <- non_na_idx[prec_rows]
          prec_data <- non_na_data[prec_rows, , drop = FALSE]

          # Format this sub-group
          fmt_args <- list(fmt$repl_str)
          fmt_args[[2]] <- num_fmt_vec_auto(prec_data$value, 1, fmt, p_int, p_dec)

          if (length(fmt$vars) > 1) {
            for (j in seq_along(fmt$vars[-1])) {
              var_name <- as_name(fmt$vars[[j + 1]])
              var_vals <- if (var_name %in% names(prec_data)) prec_data[[var_name]] else rep(NA_real_, nrow(prec_data))
              fmt_args[[j + 2]] <- num_fmt_vec_auto(var_vals, j + 1, fmt, p_int, p_dec)
            }
          }

          result[prec_idx] <- do.call(sprintf, fmt_args)
        }
        next  # Skip the default processing below
      } else {
        # Single precision level - use it
        max_int <- unique_prec$max_int[1]
        max_dec <- unique_prec$max_dec[1]
      }
    } else if (fmt$auto_precision) {
      # Auto precision but no precision columns - default to 0
      max_int <- 0
      max_dec <- 0
    } else {
      max_int <- 0
      max_dec <- 0
    }

    # Build format arguments list (single precision case)
    fmt_args <- list(fmt$repl_str)
    fmt_args[[2]] <- num_fmt_vec_auto(non_na_data$value, 1, fmt, max_int, max_dec)

    if (length(fmt$vars) > 1) {
      for (j in seq_along(fmt$vars[-1])) {
        var_name <- as_name(fmt$vars[[j + 1]])
        var_vals <- if (var_name %in% names(non_na_data)) non_na_data[[var_name]] else rep(NA_real_, nrow(non_na_data))
        fmt_args[[j + 2]] <- num_fmt_vec_auto(var_vals, j + 1, fmt, max_int, max_dec)
      }
    }

    result[non_na_idx] <- do.call(sprintf, fmt_args)
  }

  result
}
