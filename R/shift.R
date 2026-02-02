
#' @export
process_summaries.shift_layer <- function(x, ...) {

  # EXTRACT: Get needed bindings from layer environment (with inheritance from parent)
  target_var <- x$target_var
  format_strings <- x$format_strings
  built_target <- env_get(x, "built_target", inherit = TRUE)
  where <- x$where
  
  # PROCESS: Validate and process in function environment
  assert_that(all(names(target_var) %in% c("row", "column")),
              all(c("row", "column") %in% names(target_var)),
              msg = "target_vars passed to a shift layer must be named.")
  
  # Gather defaults if format_strings is NULL
  if(is.null(format_strings)) {
    format_strings <- gather_defaults(x)[[1]]
  }
  
  # Subset the local built_target based on where
  # Save the built target before the where so it can be processed in the denominator
  built_target_pre_where <- built_target
  
  # Apply where filter with error handling
  tryCatch({
    built_target <- built_target %>%
      filter(!!where)
  }, error = function(e) {
    abort(paste0("group_shift `where` condition `",
                 as_label(where),
                 "` is invalid. Filter error:\n", e))
  })
  
  # BIND: Write results back to layer environment
  x$format_strings <- format_strings
  x$built_target <- built_target
  x$built_target_pre_where <- built_target_pre_where
  
  # Call helper functions that will also follow Extract-Process-Bind pattern
  process_shift_denoms(x)
  process_shift_n(x)
  prepare_format_metadata(x)
  
  invisible(x)
}

#' Process shift layer N counts
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes shift count calculations in function environment
#' 3. Binds results back to layer environment
#'
#' @param x A shift_layer object
#' @return The layer invisibly
#' @noRd
process_shift_n <- function(x) {

  # EXTRACT: Get needed bindings from layer environment
  built_target <- x$built_target
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  by <- x$by
  target_var <- x$target_var
  cols <- env_get(x, "cols", inherit = TRUE)
  limit_data_by <- x$limit_data_by
  format_strings <- x$format_strings
  
  # PROCESS: Calculate numeric data in function environment
  numeric_data <- built_target %>%
    # Group by variables including target variables and count them
    group_by(!!treat_var, !!!by, !!!unname(target_var), !!!cols) %>%
    tally(name = "n") %>%
    ungroup() %>%
    # complete all combinations of factors to include combinations that don't exist.
    # add 0 for combinations that don't exist
    # complete(!!treat_var, !!!by, !!!unname(target_var), !!!cols, fill = list(n = 0)) %>%
    complete_and_limit(treat_var, by, cols, unname(target_var),
                       limit_data_by, .fill = list(n = 0)) %>%
    # Change the treat_var and first target_var to characters to resolve any
    # issues if there are total rows and the original column is numeric
    mutate(!!treat_var := as.character(!!treat_var)) %>%
    mutate(!!as_label(target_var$row) := as.character(!!target_var$row)) %>%
    # Rename the row target to summary_var
    rename("summary_var" := !!target_var$row)

  # If there is no values in summary_stat, which can happen depending on where. Return nothing
  if(nrow(numeric_data) == 0) return()

  # BIND: Write results back to layer environment
  x$numeric_data <- numeric_data
  
  # Call process_shift_total if needed
  if("pct" %in% format_strings$vars) process_shift_total(x)
  
  invisible(x)
}

#' Process shift layer totals
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes total calculations in function environment
#' 3. Binds results back to layer environment
#'
#' @param x A shift_layer object
#' @return The layer invisibly
#' @noRd
process_shift_total <- function(x) {

  # EXTRACT: Get needed bindings from layer environment
  numeric_data <- x$numeric_data
  denoms_by <- x$denoms_by
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  by <- x$by
  cols <- env_get(x, "cols", inherit = TRUE)
  denoms_df <- x$denoms_df
  
  # PROCESS: Calculate totals in function environment (vectorized)
  if(is.null(denoms_by)) denoms_by <- c(treat_var, by, cols)

  numeric_data <- numeric_data %>%
    get_denom_total_vectorized(denoms_by, denoms_df)

  # BIND: Write results back to layer environment
  x$numeric_data <- numeric_data
  
  invisible(x)
}

#' @noRd
prepare_format_metadata.shift_layer <- function(x) {

  # EXTRACT: Get needed bindings from layer environment
  numeric_data <- x$numeric_data
  format_strings <- x$format_strings
  
  # PROCESS: Calculate metadata in function environment
  # Pull max character length from counts. Should be at least 1
  n_width <- max(c(nchar(numeric_data$n), 1L))

  # If a layer_width flag is present, edit the formatting string to display the maximum
  # character length
  if(str_detect(format_strings$format_string, "a|A")) {
    # Replace the flag with however many xs
    replaced_string <- str_replace(format_strings$format_string, "a",
                                   paste(rep("x", n_width), collapse = ""))

    replaced_string <- str_replace(replaced_string, "A",
                                   paste(rep("X", n_width), collapse = ""))

    # Make a new f_str and replace the old one
    format_strings <- f_str(replaced_string, !!!format_strings$vars)
  }
  max_length <- format_strings$size
  
  # BIND: Write results back to layer environment
  x$format_strings <- format_strings
  x$max_length <- max_length
  
  invisible(x)

}

#' @export
#' @keywords internal
process_formatting.shift_layer <- function(x, ...) {

  # EXTRACT: Get needed bindings from layer environment (with inheritance from parent)
  numeric_data <- x$numeric_data
  format_strings <- x$format_strings
  max_layer_length <- env_get(x, "max_layer_length", inherit = TRUE)
  max_n_width <- env_get(x, "max_n_width", inherit = TRUE)
  by <- x$by
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  target_var <- x$target_var
  cols <- env_get(x, "cols", inherit = TRUE)
  
  # PROCESS: Format data in function environment
  formatted_data <- numeric_data %>%
    # Mutate value based on if there is a distinct_by
    mutate(n = construct_shift_string(.n=n, .total = total,
                                      shift_fmt=format_strings,
                                      max_layer_length=max_layer_length,
                                      max_n_width=max_n_width)) %>%
    # Select only columns needed for pivot to reduce memory footprint
    select(match_exact(by), summary_var, !!treat_var, !!target_var$column, match_exact(cols), n) %>%
    # Pivot table
    pivot_wider(id_cols = c(match_exact(by), "summary_var"),
                names_from = c( !!treat_var, !!target_var$column, match_exact(cols)),
                values_from = n,
                names_prefix = "var1_") %>%
    replace_by_string_names(quos(!!!by, summary_var))

  formatted_data <- assign_row_id(formatted_data, 's')
  
  # BIND: Write results back to layer environment
  x$formatted_data <- formatted_data
  
  add_order_columns(x)

  env_get(x, "formatted_data")

}

#' Vectorized shift string formatting
#'
#' Formats shift layer count strings using vectorized operations instead of
#' row-by-row map_chr calls. Uses num_fmt_vec for efficient bulk formatting.
#'
#' @param .n Vector of counts
#' @param .total Vector of totals (for percentage calculation)
#' @param shift_fmt The f_str object used to display the string
#' @param max_layer_length The maximum character length in the table
#' @param max_n_width The maximum count length in the table
#'
#' @return Character vector of formatted shift strings
#' @noRd
construct_shift_string <- function(.n, .total, shift_fmt, max_layer_length, max_n_width) {

  vars_ord <- map_chr(shift_fmt$vars, as_name)

  # Build argument list for sprintf using vectorized formatting
  args <- list(shift_fmt$repl_str)

  for (i in seq_along(vars_ord)) {
    var_name <- vars_ord[i]

    formatted <- switch(var_name,
      "n" = num_fmt_vec(.n, i, shift_fmt),
      "pct" = {
        # Calculate percentages, replacing NA with 0
        pcts <- replace(.n / .total, is.na(.n / .total), 0)
        num_fmt_vec(pcts * 100, i, shift_fmt)
      },
      "total" = num_fmt_vec(.total, i, shift_fmt)
    )

    args[[i + 1]] <- formatted
  }

  # Vectorized sprintf
  string_ <- do.call(sprintf, args[!map_lgl(args, is.null)])

  # Apply padding
  string_ <- pad_formatted_data(string_, 0, max_n_width)

  string_
}

#' @noRd
#' @param x The layer object
#'
#' Process shift layer denominators
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes denominator calculations in function environment
#' 3. Binds results back to layer environment
#'
#' This creates the `denoms_df` object that contains the counts of the combinations
#' of the layer and table parameters
#'
#' @param x A shift_layer object
#' @return The layer invisibly
#' @noRd
process_shift_denoms <- function(x) {

  # EXTRACT: Get needed bindings from layer environment
  built_target_pre_where <- x$built_target_pre_where
  denom_where <- x$denom_where
  where <- x$where
  target_var <- x$target_var
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  by <- x$by
  cols <- env_get(x, "cols", inherit = TRUE)
  
  # PROCESS: Calculate denominators in function environment
  if(is.null(denom_where)) denom_where <- where

  denoms_df <- built_target_pre_where %>%
    filter(!!denom_where) %>%
    group_by(!!!unname(target_var), !!treat_var, !!!by, !!!cols) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    complete(!!!unname(target_var), !!treat_var, !!!by, !!!cols) %>%
    # The rows will duplicate for some reason so this removes that
    distinct() %>%
    rename("summary_var" := !!target_var$row)

  # BIND: Write results back to layer environment
  x$denoms_df <- denoms_df
  
  invisible(x)
}
