#' Use available metadata to build the tplyr_meta object
#'
#' This is the main driver function, and layer specific variants
#' adapt on top of this function
#'
#' @param table_where Table level where filter
#' @param layer_where Layer level where filter
#' @param treat_grps Treatment groups from the tplyr_table parent environment
#' @param ... All grouping variables
#'
#' @return tplyr_meta object
#' @noRd
build_meta <- function(table_where, layer_where, treat_grps, variables, values) {

  # Make an assumption that the treatment variable was the first variable provided
  values[[1]] <- translate_treat_grps(values[[1]], treat_grps)

  filters <- make_parsed_strings(variables, values)

  meta <- new_tplyr_meta(
    names = variables,
    filters = filters
  )

  meta <- meta %>%
    add_filters_(layer_where) %>%
    add_variables_(get_vars_from_filter(layer_where)) %>%
    add_filters_(table_where) %>%
    add_variables_(get_vars_from_filter(table_where))

  meta
}

#' Build metadata for desc_layers
#'
#' @param target Target variable currently being summarized
#' @param table_where Table level where filter
#' @param layer_where Layer level where filter
#' @param treat_grps Treatment groups from the tplyr_table parent environment
#' @param ... All grouping variables
#'
#' @return tplyr_meta object
#' @noRd
build_desc_meta <- function(target, table_where, layer_where, treat_grps, ...) {

  variables <- call_args(match.call())

  # Don't want any of the named parameters here
  variables <- variables[which(names(variables)=='')]
  values <- list2(...)

  # Get rid of text provided by variables
  inds <- which(map_lgl(unname(variables), ~ quo_class(.) == "name"))
  variables <- variables[inds]
  values <- values[inds]

  # Output vector
  meta <- vector('list', length(values[[1]]))

  # Vectorize across the input data
  for (i in seq_along(values[[1]])) {
    # Pull out the current row's values
    cur_values <- map(values, ~ .x[i])
    # Build the tplyr_meta object
    meta[[i]] <- build_meta(table_where, layer_where, treat_grps, variables, cur_values) %>%
      add_variables_(target)
  }

  meta
}

#' Build metadata for count_layers
#'
#' @param target Target variable currently being summarized
#' @param table_where Table level where filter
#' @param layer_where Layer level where filter
#' @param treat_grps Treatment groups from the tplyr_table parent environment
#' @param ... All grouping variables
#'
#' @return tplyr_meta object
#' @noRd
build_count_meta <- function(layer, table_where, layer_where, treat_grps, summary_var, ...) {

  variables <- call_args(match.call())

  # Don't want any of the named parameters here
  variables <- variables[which(names(variables)=='')]
  values <- list2(...)

  # Get rid of text provided by variables
  inds <- which(map_lgl(unname(variables), ~ quo_class(.) == "name"))
  variables <- variables[inds]
  values <- values[inds]

  # The total row label may not pass through, so set it
  total_row_label <- ifelse(is.null(layer$total_row_label), 'Total', layer$total_row_label)
  missing_subjects_row_label <- ifelse(is.null(layer$total_row_label), 'Missing', layer$missing_subjects_row_label)
  count_missings <- ifelse(is.null(layer$count_missings), FALSE, layer$count_missings)
  mlist <- layer$missing_count_list

  # If the outer layer was provided as a text variable, get value
  character_outer <- get_character_outer(layer)
  unnested_character <- is_unnested_character(layer)

  # Pull out table object to use later
  tbl <- env_parent(layer)

  meta <- vector('list', length(values[[1]]))

  # Vectorize across the input data
  for (i in seq_along(values[[1]])) {

    if (!unnested_character) {
      add_vars <- layer$target_var
    } else {
      add_vars <- quos()
    }

    row_filter <- list()
    aj <- NULL

    # Pull out the current row's values
    cur_values <- map(values, ~ .x[i])

    # The outer layer will currently be NA for the outer layer summaries, so adjust the filter appropriately
    if (any(is.na(cur_values))) {

      # Total row or outer layer
      na_var <- variables[which(is.na(cur_values))]

      # work around outer letter being NA
      filter_variables <- variables[which(!is.na(cur_values))]
      filter_values <- cur_values[which(!is.na(cur_values))]

      if (summary_var[i] == total_row_label && !count_missings) {
        # Filter out the missing counts if the total row should exclude missings
        row_filter <- make_parsed_strings(layer$target_var, list(mlist), negate=TRUE)
      } else if (summary_var[i] == missing_subjects_row_label) {
        # Special handling for missing subject rows
        # Make a meta object for the pop data
        pop_filt_inds <- which(filter_variables %in% unlist(list(tbl$treat_var, tbl$cols)))
        pop_filt_vars <- filter_variables[pop_filt_inds]
        pop_filt_vals <- filter_values[pop_filt_inds]
        pop_meta <- build_meta(tbl$pop_where, quo(TRUE), treat_grps, pop_filt_vars, pop_filt_vals)
        aj <- new_anti_join(join_meta=pop_meta, on=layer$distinct_by)
      } else if (summary_var[i] %in% names(mlist)) {
        # Get the values for the missing row
        miss_val <- mlist[which(names(mlist) == summary_var[i])]
        row_filter <- make_parsed_strings(layer$target_var, list(miss_val))
      } else if (summary_var[i] != total_row_label) {
        # Subset to outer layer value
        row_filter <- make_parsed_strings(na_var, summary_var[i])
      }

      add_vars <- append(add_vars, na_var)

    } else {
      # Inside the nested layer
      filter_variables <- variables
      filter_values <- cur_values

      # Toss out the indentation
      if (!is.null(layer$indentation) && str_starts(summary_var[i], layer$indentation)) {
        summary_var[i] <- str_sub(summary_var[i], layer$indentation_length+1)
      }

      if (summary_var[i] %in% names(mlist)) {
        # Get the values for the missing row
        miss_val <- mlist[which(names(mlist) == summary_var[i])]
        row_filter <- make_parsed_strings(layer$target_var, list(miss_val))
      }
      else if (summary_var[i] == total_row_label && !count_missings) {
        # Filter out the missing counts if the total row should exclude missings
        row_filter <- make_parsed_strings(layer$target_var, list(mlist), negate=TRUE)
      } else if (summary_var[i] == missing_subjects_row_label) {
        # Special handling for missing subject rows
        # Make a meta object for the pop data
        pop_filt_inds <- which(filter_variables %in% unlist(list(tbl$treat_var, tbl$cols)))
        pop_filt_vars <- filter_variables[pop_filt_inds]
        pop_filt_vals <- filter_values[pop_filt_inds]
        # Reset to the pop treat value
        pop_filt_vars[[
          which(map_chr(pop_filt_vars, as_label) == as_label(tbl$treat_var))
          ]] <- tbl$pop_treat_var
        pop_meta <- build_meta(tbl$pop_where, quo(TRUE), treat_grps, pop_filt_vars, pop_filt_vals)
        aj <- new_anti_join(join_meta=pop_meta, on=layer$distinct_by)
      }
      else if (!is.na(character_outer) && summary_var[i] == character_outer) {
        # If the outer layer is a character string then don't provide a filter
        row_filter <- list()
      }
      else if (summary_var[i] != total_row_label && !unnested_character) {
        # If we're not in a total row, build the filter
        row_filter <- make_parsed_strings(layer$target_var, summary_var[i])
      }
    }

    # Make the meta object
    meta[[i]] <- build_meta(table_where, layer_where, treat_grps, filter_variables, filter_values) %>%
      add_filters_(row_filter) %>%
      add_variables_(add_vars) %>%
      add_anti_join_(aj)
  }

  meta
}

#' Build metadata for risk difference comparisons
#'
#' @param meta A tplyr_metadata object
#' @param treat_var the treatment variable
#' @param comp The current rdiff comparison
#'
#' @return tplyr_meta object
#' @noRd
build_rdiff_meta <- function(meta, treat_var, comp){

  for (i in seq_along(meta)) {
    # Make a new filter that contains the current comparison being made
    filt <- make_parsed_strings(list(treat_var), list(comp))[[1]]
    # Add the filter in the spot where the treatment groups are held,
    # which is always the first element (in a count layer)
    meta[[i]]$filters[[1]] <- filt
  }

  meta
}

#' Build metadata for shift_layers (vectorized)
#'
#' Builds metadata for shift layers using vectorized string operations
#' instead of row-by-row loops. Pre-computes common elements and builds
#' all filter strings at once.
#'
#' @param layer The shift layer object
#' @param table_where Table level where filter
#' @param layer_where Layer level where filter
#' @param treat_grps Treatment groups from the tplyr_table parent environment
#' @param summary_var Vector of summary variable values
#' @param ... All grouping variables
#'
#' @return List of tplyr_meta objects
#' @noRd
build_shift_meta <- function(layer, table_where, layer_where, treat_grps, summary_var, ...) {

  variables <- call_args(match.call())

  # Don't want any of the named parameters here
  variables <- variables[which(names(variables)=='')]
  values <- list2(...)

  # Get rid of text provided by variables
  inds <- which(map_lgl(unname(variables), ~ quo_class(.) == "name"))
  variables <- variables[inds]
  values <- values[inds]

  n_rows <- length(values[[1]])

  # Pre-compute common elements (same for all rows)
  common_names <- append(variables, layer$target_var$row)
  layer_where_vars <- get_vars_from_filter(layer_where)
  table_where_vars <- get_vars_from_filter(table_where)

  # Pre-compute variable labels for filter string building
  var_labels <- map_chr(variables, as_label)
  row_var_label <- as_label(layer$target_var$row)

  # Translate treatment groups once per unique value (first column is treat var)
  unique_treat <- unique(values[[1]])
  treat_translation <- setNames(
    map_chr(unique_treat, ~ translate_treat_grps(.x, treat_grps)),
    unique_treat
  )
  translated_treat <- treat_translation[as.character(values[[1]])]

  # Build all filter strings at once using vectorized operations
  # For each variable, create filter strings for all rows
  filter_strings <- vector("list", length(variables) + 1)

  for (j in seq_along(variables)) {
    vals <- if (j == 1) translated_treat else values[[j]]
    vname <- var_labels[j]

    # Vectorized filter string construction
    filter_strings[[j]] <- ifelse(
      is.na(vals),
      paste0("is.na(", vname, ")"),
      ifelse(
        sapply(vals, function(v) class(v) %in% c('character', 'factor')),
        paste0(vname, ' == "', vals, '"'),
        paste0(vname, " == ", vals)
      )
    )
  }

  # Summary var filter (row variable)
  filter_strings[[length(variables) + 1]] <- ifelse(
    is.na(summary_var),
    paste0("is.na(", row_var_label, ")"),
    ifelse(
      sapply(summary_var, function(v) class(v) %in% c('character', 'factor')),
      paste0(row_var_label, ' == "', summary_var, '"'),
      paste0(row_var_label, " == ", summary_var)
    )
  )

  # Parse all filter strings at once
  parsed_filters <- lapply(filter_strings, function(fs) {
    lapply(fs, str2lang)
  })

  # Build list of tplyr_meta objects
  meta <- vector('list', n_rows)

  for (i in seq_len(n_rows)) {
    # Collect filters for this row
    row_filters <- lapply(seq_along(parsed_filters), function(j) parsed_filters[[j]][[i]])

    meta[[i]] <- new_tplyr_meta(
      names = common_names,
      filters = row_filters
    )

    # Add layer and table where filters/vars (same for all)
    meta[[i]] <- meta[[i]] %>%
      add_filters_(layer_where) %>%
      add_variables_(layer_where_vars) %>%
      add_filters_(table_where) %>%
      add_variables_(table_where_vars)
  }

  meta
}
