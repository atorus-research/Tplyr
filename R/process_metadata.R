#' Process metadata for a layer of type \code{desc}
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes metadata in function environment
#' 3. Binds results back to layer environment
#'
#' @param x Layer object
#'
#' @return Nothing
#' @export
#' @noRd
process_metadata.desc_layer <- function(x, ...) {

  # EXTRACT: Get needed bindings from layer environment
  target_var <- x$target_var
  num_sums_raw <- x$num_sums_raw
  trans_sums <- x$trans_sums
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  by <- x$by
  cols <- env_get(x, "cols", inherit = TRUE)
  table_where <- env_get(x, "table_where", inherit = TRUE)
  where <- x$where
  treat_grps <- env_get(x, "treat_grps", inherit = TRUE)
  
  # stats_as_columns may not exist, default to FALSE
  stats_as_columns <- if (exists("stats_as_columns", envir = x, inherits = FALSE)) {
    x$stats_as_columns
  } else {
    FALSE
  }

  # PROCESS: Generate metadata in function environment
  # meta_sums store the metadata table built alongside trans_sums
  meta_sums <- vector("list", length(target_var))
  form_meta <- vector("list", length(target_var))

  for (i in seq_along(target_var)) {
    cur_var <- target_var[[i]]

    # Prepare metadata table
    meta_sum <- num_sums_raw[[i]] %>%
      group_by(!!treat_var, !!!by, !!!cols) %>%
      group_keys() %>%
      # rowwise() %>%
      mutate(
        meta = build_desc_meta(cur_var, table_where, where, treat_grps, !!treat_var, !!!by, !!!cols)
      )


    # Join meta table with the transposed summaries ready for formatting
    meta_sums[[i]] <- trans_sums[[i]] %>%
      select(!!treat_var, match_exact(by), !!!cols, row_label) %>%
      left_join(meta_sum, by=c(as_label(treat_var), match_exact(by), match_exact(cols)))

    if (stats_as_columns) {
      # Transpose the metadata identical to the summary
      form_meta[[i]] <- meta_sums[[i]] %>%
        pivot_wider(id_cols=c(!!treat_var, match_exact(by)),
                    names_from = match_exact(vars(row_label, !!!cols)),
                    names_prefix = paste0('var', i, "_"),
                    values_from = meta
        )
    } else {
      form_meta[[i]] <- meta_sums[[i]] %>%
        pivot_wider(id_cols=c('row_label', match_exact(by)),
                    names_from = match_exact(vars(!!treat_var, !!!cols)),
                    names_prefix = paste0('var', i, "_"),
                    values_from = meta
        )
    }

  }

  if (stats_as_columns) {
    formatted_meta <- reduce(form_meta, full_join, by=c(as_label(treat_var), match_exact(by)))
    formatted_meta <- replace_by_string_names(formatted_meta, by, treat_var)
  } else {
    formatted_meta <- reduce(form_meta, full_join, by=c('row_label', match_exact(by)))
    formatted_meta <- replace_by_string_names(formatted_meta, by)
  }

  formatted_meta <- assign_row_id(formatted_meta, 'd')

  # BIND: Explicitly bind result to layer environment
  x$formatted_meta <- formatted_meta

  env_get(x, "formatted_meta")

}

#' Process metadata for a layer of type \code{count}
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes metadata in function environment
#' 3. Binds results back to layer environment
#'
#' @param x Layer object
#' @param ... Pass through parameters
#'
#' @return Formatted count metadata
#' @export
#' @noRd
process_metadata.count_layer <- function(x, ...) {

  # EXTRACT: Get needed bindings from layer environment
  numeric_data <- x$numeric_data
  table_where <- env_get(x, "table_where", inherit = TRUE)
  where <- x$where
  treat_grps <- env_get(x, "treat_grps", inherit = TRUE)
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  by <- x$by
  cols <- env_get(x, "cols", inherit = TRUE)
  target_var <- x$target_var
  is_built_nest <- env_get(x, "is_built_nest", default = FALSE, inherit = TRUE)
  stats <- env_get(x, "stats", default = list())
  row_labels <- env_get(x, "row_labels", default = NULL)
  
  # PROCESS: Generate metadata in function environment
  # Build up the metadata for the count layer
  meta_sum <- numeric_data %>%
    mutate(
      meta = build_count_meta(
        x,  # Pass layer environment for build_count_meta
        table_where,
        where,
        treat_grps,
        summary_var,
        !!treat_var,
        !!!by,
        !!!cols
        )
    )

  # Pivot the meta table
  formatted_meta <- meta_sum %>%
    pivot_wider(id_cols = c(match_exact(by), "summary_var"),
                names_from = c(!!treat_var, match_exact(cols)), values_from = meta,
                names_prefix = "var1_") %>%
    replace_by_string_names(quos(!!!by, summary_var))

  if (is_built_nest) {
    row_labels_meta <- vars_select(names(formatted_meta), starts_with("row_label"))
    formatted_meta[is.na(formatted_meta[[1]]), 1] <- formatted_meta[is.na(formatted_meta[[1]]),
                                                                    tail(row_labels, 1)]
  }

  if (!is_empty(stats)) {
    formatted_stats_metadata <- map(stats, process_metadata) %>%
      reduce(full_join, by = c('summary_var', match_exact(c(by, head(target_var, -1))))) %>%
      # Replace the by variables and target variable names with `row_label<n>`
      replace_by_string_names(quos(!!!by, summary_var))

    formatted_meta <- full_join(formatted_meta, formatted_stats_metadata,
                                by = vars_select(names(formatted_meta), starts_with("row_label")))

  }

  # Attach the row identifier
  formatted_meta <- assign_row_id(formatted_meta, 'c')

  # BIND: Explicitly bind result to layer environment
  x$formatted_meta <- formatted_meta
  
  env_get(x, "formatted_meta")

}

#' Process metadata for a tplyr_statistic object
#'
#' This is a generic dispatcher that calls the appropriate method based on
#' the second class of the statistic object (e.g., tplyr_riskdiff)
#'
#' @param x Statistic object
#' @param ... Additional arguments
#'
#' @return Formatted statistic metadata
#' @noRd
process_metadata.tplyr_statistic <- function(x, ...) {
  # Get the second class (the specific statistic type)
  stat_class <- class(x)[2]
  
  # Dispatch to the specific method
  if (stat_class == "tplyr_riskdiff") {
    process_metadata.tplyr_riskdiff(x, ...)
  } else {
    abort(paste0("No process_metadata method for statistic class: ", stat_class))
  }
}

#' Process metadata for risk difference statistics
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from statistic environment
#' 2. Processes metadata in function environment
#' 3. Binds results back to statistic environment
#'
#' @param x A tplyr_riskdiff statistic object
#' @param ... Pass through parameters
#'
#' @return Formatted risk difference metadata
#' @noRd
process_metadata.tplyr_riskdiff <- function(x, ...) {

  # EXTRACT: Get what we need from the statistic environment
  comparisons <- x$comparisons
  
  # Get these from the parent layer environment
  meta_sum <- env_get(x, "meta_sum", default = NULL, inherit = TRUE)
  treat_var <- env_get(x, "treat_var", default = NULL, inherit = TRUE)
  by <- env_get(x, "by", default = NULL, inherit = TRUE)
  cols <- env_get(x, "cols", default = NULL, inherit = TRUE)
  target_var <- env_get(x, "target_var", default = NULL, inherit = TRUE)
  is_built_nest <- env_get(x, "is_built_nest", default = FALSE, inherit = TRUE)
  
  # PROCESS: Work in function environment
  stats_meta <- vector('list', length(comparisons))

  for (i in seq_along(comparisons)) {

    # Weird looking, but this will give me just the variables needed
    stats_meta[[i]] <- meta_sum %>%
      select(-!!treat_var, -any_of(c('n', 'distinct_n', 'distinct_total', 'total'))) %>%
      mutate(
        meta = build_rdiff_meta(meta, treat_var, comparisons[[i]])
      )

    # Rename the meta variable
    names(stats_meta[[i]])[ncol(stats_meta[[i]])] <- paste(c("rdiff", comparisons[[i]]), collapse = "_")

  }

  # Join the rdiff columns together
  formatted_stats_meta <- reduce(stats_meta,
                                 full_join,
                                 by=c(match_exact(c(by, cols, head(target_var, -1))),  'summary_var')) %>%
    distinct()

  if (length(cols) > 0) {

    # If only one comparison was made, the columns won't prefix with the transposed variable name
    # So trick it by introducing a column I can drop later. Not great, but functional
    formatted_stats_meta['rdiffx'] <- ''

    # Pivot by column
    formatted_stats_meta <- formatted_stats_meta %>%
      pivot_wider(id_cols=c(match_exact(c(by, head(target_var, -1))),  'summary_var'),
                  names_from = match_exact(cols),
                  names_sep = "_",
                  values_from=starts_with('rdiff'))

    # Drop the dummied columns
    formatted_stats_meta <- formatted_stats_meta %>% select(-starts_with('rdiffx'))

  }

  # Handle the outer layer being NA for the outer layer
  if (is_built_nest) {
    formatted_stats_meta <- formatted_stats_meta %>%
      mutate(
        !!by[[1]] := if_else(is.na(!!by[[1]]), summary_var, as.character(!!by[[1]]))
      )
  }

  # BIND: Write results back to statistic environment
  x$formatted_stats_meta <- formatted_stats_meta
  
  formatted_stats_meta
}

#' Process metadata for a layer of type \code{shift}
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes metadata in function environment
#' 3. Binds results back to layer environment
#'
#' @param x Layer object
#'
#' @return Nothing
#' @export
#' @noRd
process_metadata.shift_layer <- function(x, ...) {

  # EXTRACT: Get needed bindings from layer environment
  numeric_data <- x$numeric_data
  target_var <- x$target_var
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  by <- x$by
  cols <- env_get(x, "cols", inherit = TRUE)
  table_where <- env_get(x, "table_where", inherit = TRUE)
  where <- x$where
  treat_grps <- env_get(x, "treat_grps", inherit = TRUE)
  
  # PROCESS: Generate metadata in function environment
  # Note: build_shift_meta() requires the layer environment for match.call()
  # metaprogramming, so we pass the layer object directly
  
  # Build up the metadata for the shift layer
  formatted_meta <- numeric_data %>%
    mutate(
      meta = build_shift_meta(
        x,
        table_where,
        where,
        treat_grps,
        summary_var,
        !!treat_var,
        !!!by,
        !!!cols,
        !!target_var$column
      )
    ) %>%
    # Pivot table
    pivot_wider(id_cols = c(match_exact(by), "summary_var"),
                names_from = c( !!treat_var, !!target_var$column, match_exact(cols)),
                values_from = meta,
                names_prefix = "var1_") %>%
    replace_by_string_names(quos(!!!by, summary_var))

  # Attach the row identifier
  formatted_meta <- assign_row_id(formatted_meta, 's')

  # BIND: Explicitly bind result to layer environment
  x$formatted_meta <- formatted_meta

  env_get(x, "formatted_meta")
}
