
#' @noRd
#' @export
process_summaries.count_layer <- function(x, ...) {

  if (env_get(x, "is_built_nest", default = FALSE)) {
    refresh_nest(x)
  }

  # EXTRACT: Get needed bindings from layer environment (with inheritance from parent)
  built_target <- env_get(x, "built_target", inherit = TRUE)
  target <- env_get(x, "target", inherit = TRUE)
  target_var <- env_get(x, "target_var", inherit = TRUE)
  where <- env_get(x, "where", inherit = TRUE)
  kept_levels <- env_get(x, "kept_levels", inherit = TRUE)
  levels_to_keep <- env_get(x, "levels_to_keep", inherit = TRUE, default = NULL)
  include_total_row <- env_get(x, "include_total_row", inherit = TRUE, default = NULL)
  total_row_label <- env_get(x, "total_row_label", inherit = TRUE, default = NULL)
  include_missing_subjects_row <- env_get(x, "include_missing_subjects_row", inherit = TRUE, default = NULL)
  missing_subjects_row_label <- env_get(x, "missing_subjects_row_label", inherit = TRUE, default = NULL)
  built_target_pre_where <- env_get(x, "built_target_pre_where", inherit = TRUE, default = NULL)

  # PROCESS: Subset the local built_target based on where
  # Catch errors
  tryCatch({
    # Check 'kept_levels' and stop if they're not in the target dataset
    #Logic to check for keep_levels
    # If this is not a built nest
    if (!("tplyr_layer" %in% class(env_parent(x)))) {
      keep_levels_logic <- expr(!is.null(levels_to_keep))
      # If this is a built nest and we're begining to process
    } else if ("tplyr_layer" %in% class(env_parent(x)) && length(target_var) == 2) {
      keep_levels_logic <- expr(!is.null(levels_to_keep) && quo_is_symbol(target_var[[1]]))
      # If this is a built nest and we are processing the "sub" layers
    } else {
      keep_levels_logic <- expr(FALSE)
    }

    # Check that all values in 'keep levels' are present in the data
    if (eval_tidy(keep_levels_logic)) {
      if (is.factor(target[[as_name(tail(target_var, 1)[[1]])]])) {
        target_levels <- levels(target[[as_name(tail(target_var, 1)[[1]])]])
      } else {
        target_levels <- unique(target[[as_name(tail(target_var, 1)[[1]])]])
      }
      kept_levels_found <- unlist(levels_to_keep) %in% target_levels
      assert_that(all(kept_levels_found),
                  msg = paste0("level passed to `kept_levels` not found: ",
                               paste0(levels_to_keep[!kept_levels_found],
                                      collapse = "",
                                      sep = " ")))
    }

    # Do this here to make sure that defaults are available everywhere else
    # Downstream
    if (is.null(include_total_row)) include_total_row <- FALSE
    if (is.null(total_row_label)) total_row_label <- "Total"
    if (is.null(include_missing_subjects_row)) include_missing_subjects_row <- FALSE
    if (is.null(missing_subjects_row_label)) missing_subjects_row_label <- "Missing"

    # Save this for the denominator where, but only if it hasn't been saved yet.
    if (is.null(built_target_pre_where)) built_target_pre_where <- built_target

    built_target <- built_target %>%
      filter(!!where) %>%
      filter(!!kept_levels)

    ## Drop levels if target var is factor and kept levels used
    if (eval_tidy(keep_levels_logic) &&
        is.factor(built_target[[as_name(tail(target_var, 1)[[1]])]])) {
      # Pull out the levels that weren't in keep levels.
      target_levels <- levels(built_target[[as_name(tail(target_var, 1)[[1]])]])
      drop_levels_ind <- !(target_levels %in% levels_to_keep)
      drop_these_levels <- target_levels[drop_levels_ind]
      # Use forcats to remove the levels that weren't in the "keep levels"
      built_target <- built_target %>%
        mutate(!!tail(target_var,1)[[1]] := fct_drop(!!tail(target_var,1)[[1]], only = drop_these_levels))
    }

  }, error = function(e) {
    abort(paste0("group_count `where` condition `",
                 as_label(where),
                 "` is invalid. Filter error:\n", e))
  })

  if (!quo_is_symbol(target_var[[1]]) && as_name(target_var[[1]]) %in% names(target)) {
    warning(paste0("The first target variable has been coerced into a symbol.",
                   " You should pass variable names unquoted."), immediate. = TRUE)

    target_var[[1]] <- quo(!!sym(as_name(target_var[[1]])))
  }

  if (length(target_var) == 2 && !quo_is_symbol(target_var[[2]]) &&
      as_name(target_var[[2]]) %in% names(target)) {
    warning(paste0("The second target variable has been coerced into a symbol.",
                   "You should pass variable names unquoted."), immediate. = TRUE)

    target_var[[2]] <- quo(!!sym(as_name(target_var[[2]])))
  }

  # BIND: Write processed results back to layer environment
  x$built_target <- built_target
  x$include_total_row <- include_total_row
  x$total_row_label <- total_row_label
  x$include_missing_subjects_row <- include_missing_subjects_row
  x$missing_subjects_row_label <- missing_subjects_row_label
  x$built_target_pre_where <- built_target_pre_where
  x$target_var <- target_var

  rename_missing_values(x)

  # Preprocssing in the case of two target_variables
  if (length(env_get(x, "target_var")) > 2) abort("Only up too two target_variables can be used in a count_layer")

  else if (length(env_get(x, "target_var")) == 2) {

    # Change treat_var to factor so all combinations appear in nest
    factor_treat_var(x)

    # If the nest_sort_index isn't null, reset it
    # This happens if the layer is reloaded
    if (!is.null(env_get(x, "nest_sort_index", default = NULL))) env_bind(x, nest_sort_index = NULL)

    process_nested_count_target(x)

  } else {

    process_count_denoms(x)

    process_single_count_target(x)

  }

  prepare_format_metadata(x)

  # Trigger any derivation of additional statistics
  map(x$stats, process_statistic_data)

  x
}

#' @param x A count layer with a single target_var
#'
#' This function uses dplyr to filter out the where call, pull out the distinct
#' rows if applicable, and tallies the different target_var values.
#'
#' If include_total_row is true a row will be added with a total row labeled
#' with total_row_label.
#'
#' Complete is used to complete the combinations of by, treat_var, and target_var
#'
#' @noRd
process_single_count_target <- function(x) {
  # EXTRACT: Get needed bindings from layer environment
  include_total_row <- env_get(x, "include_total_row")
  include_missing_subjects_row <- env_get(x, "include_missing_subjects_row")
  count_row_prefix <- env_get(x, "count_row_prefix", default = NULL)
  denoms_by <- env_get(x, "denoms_by", default = NULL, inherit=TRUE)
  target_var <- env_get(x, "target_var")
  format_strings <- env_get(x, "format_strings", default = NULL, inherit=TRUE)
  count_missings <- env_get(x, "count_missings", default = FALSE, inherit=TRUE)
  denom_ignore <- env_get(x, "denom_ignore", default = NULL, inherit=TRUE)
  total_count_format <- env_get(x, "total_count_format", default = NULL)
  count_fmt <- env_get(x, "count_fmt", default = NULL, inherit=TRUE)

  # PROCESS: Execute in function environment
  # The current environment should be the layer itself
  process_count_n(x)

  if (include_total_row) {
    process_count_total_row(x)

    # Used to temporarily check formats
    if (is.null(format_strings)) tmp_fmt <- gather_defaults.count_layer(x)
    if (count_missings && !(is.null(denom_ignore) || length(denom_ignore) == 0) &&
        (("pct" %in% total_count_format$vars || "distinct_pct" %in% total_count_format$vars) ||
         # Logic if no total_count format
         (is.null(total_count_format) && is.null(format_strings) && ("pct" %in% tmp_fmt$n_counts$vars || "distinct_pct" %in% tmp_fmt$n_counts$vars)) ||
         (is.null(total_count_format) && ("pct" %in% count_fmt$n_counts$vars || "distinct_pct" %in% count_fmt$n_counts$vars))
        )
    ) {
      warning("Your total row is ignoring certain values. The 'pct' in this row may not be 100%",
              immediate. = TRUE)
    }
  }

  if (include_missing_subjects_row) {
    process_missing_subjects_row(x)
  }

  if (is.null(count_row_prefix)) count_row_prefix <- ""

  # Extract summary_stat, total_stat, missing_subjects_stat after processing
  # Also re-extract denoms_by as it may have been modified by process_count_n
  summary_stat <- env_get(x, "summary_stat")
  total_stat <- env_get(x, "total_stat", default = NULL)
  missing_subjects_stat <- env_get(x, "missing_subjects_stat", default = NULL, inherit=TRUE)
  denoms_df <- env_get(x, "denoms_df", inherit=TRUE)
  denoms_by <- env_get(x, "denoms_by", default = NULL, inherit=TRUE)
  
  # Note: We don't return early for empty summary_stat because we still need to
  # process it through get_denom_total() to add the required columns (total, distinct_total, etc.)

  # If a denoms variable is factor then it should be character for the denoms calculations
  denoms_df_prep <- denoms_df %>%
    mutate(
      across(dplyr::where(is.factor), ~as.character(.))
    )

  # Nested counts might have summary var come through as numeric
  if ('summary_var' %in% map_chr(denoms_by, as_name) && is.numeric(denoms_df_prep$summary_var)) {
    denoms_df_prep$summary_var <- as.character(denoms_df_prep$summary_var)
  }

  # But if a summary_stat variable is factor, then the denoms needs to match this
  # This happens if sorting was triggered for the variable as a factor
  # fct_cols will be a named logical vector of the variable names, where TRUE
  # is the summary_stat variables that are factors
  fct_cols <- map_lgl(summary_stat, is.factor)

  if (any(fct_cols)) {
    # From the bool vector of fct_cols, grab the names of the ones that
    # are TRUE
    # Create a regular expression like var1|var2|var3
    fct_cols_ns <- paste(names(fct_cols[fct_cols]), collapse="|")

    # Reset each factor variable to have the appropriate levels for the denom
    # so that 0 filling can happen appropriately later on
    denoms_df_prep <- denoms_df_prep %>%
      mutate(
        across(matches(fct_cols_ns), ~ factor(., levels=levels(summary_stat[[cur_column()]])))
      )
  }

  # rbind tables together and join denominator totals (vectorized)
  numeric_data <- bind_rows(summary_stat, total_stat, missing_subjects_stat) %>%
    rename("summary_var" = !!target_var[[1]]) %>%
    get_denom_total_vectorized(denoms_by, denoms_df_prep, "n") %>%
    mutate(
      # Inline paste0 call (paste0 also handles NA to "NA" conversion)
      summary_var = paste0(count_row_prefix, summary_var),
      # Calculate percentages here rather than in formatting step
      pct = replace(n / total, is.na(n / total), 0),
      distinct_pct = replace(distinct_n / distinct_total, is.na(distinct_n / distinct_total), 0)
    )

  # BIND: Write results back to layer environment
  x$numeric_data <- numeric_data

  invisible(x)
}

#' Process the n count data and put into summary_stat
#' Process the n count data and put into summary_stat
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes count calculations in function environment
#' 3. Binds results back to layer environment
#'
#' @param x Count layer
#' @return The layer invisibly
#' @noRd
process_count_n <- function(x) {
  # EXTRACT: Get needed bindings from layer environment (with inheritance from parent)
  denoms_by <- env_get(x, "denoms_by", default = NULL, inherit=TRUE)
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  cols <- env_get(x, "cols", inherit = TRUE)
  target_var <- env_get(x, "target_var")
  built_target <- env_get(x, "built_target")
  by <- env_get(x, "by")
  distinct_by <- env_get(x, "distinct_by", default = NULL, inherit=TRUE)
  missing_count_string <- env_get(x, "missing_count_string", default = NULL, inherit=TRUE)
  missing_count_list <- env_get(x, "missing_count_list", default = NULL, inherit=TRUE)
  limit_data_by <- env_get(x, "limit_data_by", default = NULL)
  outer_ <- env_get(x, "outer_", default = FALSE)

  # PROCESS: Execute in function environment
  if (is.null(denoms_by)) denoms_by <- c(treat_var, cols)
  denoms_by_ <- map(denoms_by, function(x) {
    if (as_name(x) == "summary_var") quo(!!target_var[[1]])
    else x
  })

  summary_stat <- built_target %>%
    mutate(
      across(
        .cols = any_of(map_chr(c(denoms_by, target_var, by), ~as_name(.))),
        .fns = function(x) if (is.factor(x)) x else as.factor(x)
      )
    ) %>%
    # Group by variables including target variables and count them
    group_by(!!treat_var, !!!by, !!!target_var, !!!cols) %>%
    summarize(
      n = n(),
      distinct_n = n_distinct(!!!distinct_by, !!treat_var, !!!target_var)
    ) %>%
    mutate(
      n = as.double(n),
      distinct_n = as.double(distinct_n)
    ) %>%
    ungroup()

  # If there is a missing_count_string, but its not in the dataset
  if (!is.null(missing_count_string) &&

      !((any(unname(unlist(missing_count_list)) %in% unique(built_target[, as_name(target_var[[1]])]))) ||
        any(is.na(built_target[, as_name(target_var[[1]])])))) {
    # This adds the missing string as a factor to the tallies. This is needed
    # to make sure the missing row is added even if there are no missing values.
    summary_stat <- summary_stat %>%
      mutate(!!target_var[[1]] := fct_expand(.data[[as_name(target_var[[1]])]],
                                             names(missing_count_list)))
  }

  complete_levels <- summary_stat %>%
    complete_and_limit(treat_var, by, cols, target_var, limit_data_by,
                       .fill = list(n = 0, total = 0, distinct_n = 0, distinct_total = 0),
                       outer=outer_)

  summary_stat <- complete_levels %>%
    # Change the treat_var and first target_var to characters to resolve any
    # issues if there are total rows and the original column is numeric
    mutate(!!treat_var := as.character(!!treat_var)) %>%
    mutate(!!as_name(target_var[[1]]) := as.character(!!target_var[[1]])) %>%
    group_by(!!!denoms_by_) %>%
    ungroup()

  # BIND: Write results back to layer environment (even if empty)
  # This ensures summary_stat exists for downstream processing
  x$summary_stat <- summary_stat
  x$denoms_by <- denoms_by

  invisible(x)
}


#' Get Logical vector that is used to remove the treat_var and cols
#'
#' In total row and missing subject counts, denoms_by needs to be stripped of
#' cols and treat_var variables, otherwise it will error out in the group_by
#'
#' @param denoms_by The layer denoms by
#' @param treat_var table treat var
#' @param cols tables cols vars
#'
#' @return list of quosures
#' @noRd
get_needed_denoms_by <- function(denoms_by, treat_var, cols) {
  map_lgl(denoms_by, function(x, treat_var, cols) {
    all(as_name(x) != as_name(treat_var),
        as_name(x) != map_chr(cols, as_name))
  }, treat_var, cols)
}

#' Process the amounts for a total row
#'
#' Process the amounts for a total row
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes total row calculations in function environment
#' 3. Binds results back to layer environment
#'
#' @param x A Count layer
#' @return The layer invisibly
#' @noRd
process_count_total_row <- function(x) {
  # EXTRACT: Get needed bindings from layer environment (with inheritance from parent)
  include_total_row <- env_get(x, "include_total_row", default = NULL)
  denoms_by <- env_get(x, "denoms_by", default = NULL)
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  cols <- env_get(x, "cols", inherit = TRUE)
  by <- env_get(x, "by")
  count_missings <- env_get(x, "count_missings", default = FALSE)
  target_var <- env_get(x, "target_var")
  missing_count_list <- env_get(x, "missing_count_list", default = NULL)
  summary_stat <- env_get(x, "summary_stat")
  total_row_label <- env_get(x, "total_row_label")

  # PROCESS: Execute in function environment
  # Check if denoms_by wasn't passed and by was passed.
  if (!is.null(include_total_row) && include_total_row &&
      identical(denoms_by, c(treat_var, cols)) && any(map_lgl(by, quo_is_symbol)) > 0) {
    warning("A total row was added in addition to non-text by variables, but
no denoms_by variable was set. This may cause unexpected results. If you wish to
change this behavior, use `set_denoms_by()`.", immediate. = TRUE)
  }

  # Logical vector that is used to remove the treat_var and cols
  needed_denoms_by <- get_needed_denoms_by(denoms_by, treat_var, cols)

  #Create an expression to evaluate filter
  if (!count_missings) {
    filter_logic <- expr(!(!!target_var[[1]] %in% names(missing_count_list)))
  } else {
    filter_logic <- expr(TRUE)
  }

  # create a data.frame to create total counts
  total_stat <- summary_stat %>%
    #Filter out any ignored denoms
    filter(!!filter_logic) %>%
    # Use distinct if this is a distinct total row
    # Group by all column variables
    group_by(!!treat_var, !!!cols, !!!denoms_by[needed_denoms_by]) %>%
    summarize(
      n = sum(n),
      distinct_n = sum(distinct_n)
    ) %>%
    ungroup() %>%
    # Create a variable to label the totals when it is merged in.
    mutate(!!as_name(target_var[[1]]) := total_row_label) %>%
    # Create variables to carry forward 'by'. Only pull out the ones that
    # aren't symbols
    group_by(!!!extract_character_from_quo(by)) %>%
    # ungroup right away to make sure the complete works
    ungroup()

  # BIND: Write results back to layer environment
  x$total_stat <- total_stat

  invisible(x)
}

#' Process the amounts for a missing subjects row
#'
#' @param x A Count layer
#' @noRd
process_missing_subjects_row <- function(x) {
  # EXTRACT: Get needed bindings from layer environment (with inheritance from parent)
  denoms_by <- env_get(x, "denoms_by", default = NULL)
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  cols <- env_get(x, "cols", inherit = TRUE)
  pop_treat_var <- env_get(x, "pop_treat_var", inherit = TRUE)
  built_target <- env_get(x, "built_target")
  by <- env_get(x, "by")
  distinct_by <- env_get(x, "distinct_by", default = NULL)
  header_n <- env_get(x, "header_n", inherit = TRUE)
  target_var <- env_get(x, "target_var")
  missing_subjects_row_label <- env_get(x, "missing_subjects_row_label")

  # PROCESS: Execute in function environment
  # Logical vector that is used to remove the treat_var and cols
  needed_denoms_by <- get_needed_denoms_by(denoms_by, treat_var, cols)

  # Create the merge variables to join the header_n data
  mrg_vars <- map_chr(c(pop_treat_var, cols, denoms_by[needed_denoms_by]), as_name)
  names(mrg_vars)[1] <- as_name(treat_var)
  # create a data.frame to create total counts
  missing_subjects_stat <- built_target %>%
    # Use distinct if this is a distinct total row
    # Group by all column variables
    distinct(!!treat_var, !!!cols, !!!by, !!!distinct_by) %>%
    ungroup() %>%
    count(!!treat_var, !!!cols, !!!by, name="n_present") %>%
    # complete based on missing groupings
    complete(!!treat_var, !!!cols, !!!by, fill = list(n_present = 0)) %>%
    left_join(
      header_n %>% rename(header_tots = n), by = mrg_vars
    ) %>%
    # Create a variable to label the totals when it is merged in.
    mutate(
      !!as_name(target_var[[1]]) := missing_subjects_row_label,
      distinct_n = header_tots - n_present
    ) %>%
    # Create variables to carry forward 'by'. Only pull out the ones that
    # aren't symbols
    group_by(!!!extract_character_from_quo(by)) %>%
    # ungroup right away to make sure the complete works
    ungroup() %>%
    select(-c(n_present, header_tots))

  # BIND: Write results back to layer environment
  x$missing_subjects_stat <- missing_subjects_stat

  invisible(x)
}

#' Prepare metadata for table
#'
#' @param x count_layer object
#' @noRd
prepare_format_metadata.count_layer <- function(x) {
  
  # EXTRACT: Get needed bindings from layer environment
  format_strings <- x$format_strings
  distinct_by <- x$distinct_by
  numeric_data <- x$numeric_data
  
  # PROCESS: Calculate metadata in function environment
  # Get formatting metadata prepared
  if (is.null(format_strings)) {
    format_strings <- gather_defaults(x)
  } else if (!'n_counts' %in% names(format_strings)) {
    format_strings[['n_counts']] <- gather_defaults(x)[['n_counts']]
  }

  # If there is both n & distinct, or pct and distinct_pct there has to be a
  # distinct_by
  # If both distinct and n
  if (((("distinct_n" %in% map(format_strings$n_counts$vars, as_name) &
         "n" %in% map(format_strings$n_counts$vars, as_name)) |
        # or both distinct_pct and pct
        ("distinct_pct" %in% map(format_strings$n_counts$vars, as_name) &
         "pct" %in% map(format_strings$n_counts$vars, as_name))) &
       # AND distinct_by is null
       is.null(distinct_by))) {
    stop("You can't use distinct and non-distinct parameters without specifying a distinct_by")
  }

  # If distinct_by isn't there, change distinct and distinct_pct
  if (is.null(distinct_by) & "distinct_n" %in% map(format_strings$n_counts$vars, as_name)) {
    distinct_ind <- which(map(format_strings$n_counts$vars, as_name) %in% "distinct_n")
    format_strings$n_counts$vars[[distinct_ind]] <- expr(n)
  }
  if (is.null(distinct_by) & "distinct_pct" %in% map(format_strings$n_counts$vars, as_name)) {
    distinct_ind <- which(map(format_strings$n_counts$vars, as_name) %in% "distinct_pct")
    format_strings$n_counts$vars[[distinct_ind]] <- expr(pct)
  }

  # Pull max character length from counts. Should be at least 1
  # Optimization: Only calculate nchar for unique values to reduce computation
  n_width <- max(c(nchar(unique(numeric_data$n)), 1L), na.rm = TRUE)

  # If a layer_width flag is present, edit the formatting string to display the maximum
  # character length
  if (str_detect(format_strings[['n_counts']]$format_string, "a|A")) {
    # Replace 'a' with appropriate 'x'
    replaced_string <- str_replace(format_strings[['n_counts']]$format_string, "a",
                                   paste(rep("x", n_width), collapse = ""))
    # Replace 'A' with appropriate 'X'
    replaced_string <- str_replace(replaced_string, "A",
                                   paste(rep("X", n_width), collapse = ""))

    # Make a new f_str and replace the old one
    format_strings[['n_counts']] <- f_str(replaced_string, !!!format_strings$n_counts$vars)
  }
  max_length <- format_strings[['n_counts']]$size
  
  # BIND: Write results back to layer environment
  x$format_strings <- format_strings
  x$n_width <- n_width
  x$max_length <- max_length
  
  invisible(x)
}

#' @noRd
#' @export
process_formatting.count_layer <- function(x, ...) {
  # EXTRACT: Get needed bindings from layer environment (with inheritance from parent table)
  numeric_data <- env_get(x, "numeric_data")
  indentation <- env_get(x, "indentation", default = NULL)
  numeric_cutoff <- env_get(x, "numeric_cutoff", default = NULL)
  numeric_cutoff_stat <- env_get(x, "numeric_cutoff_stat", default = NULL)
  numeric_cutoff_column <- env_get(x, "numeric_cutoff_column", default = NULL)
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  format_strings <- env_get(x, "format_strings")
  max_layer_length <- env_get(x, "max_layer_length", default = NULL)
  max_n_width <- env_get(x, "max_n_width", default = NULL)
  missing_string <- env_get(x, "missing_string", default = NULL)
  missing_count_string <- env_get(x, "missing_count_string", default = NULL)
  total_count_format <- env_get(x, "total_count_format", default = NULL)
  missing_subjects_count_format <- env_get(x, "missing_subjects_count_format", default = NULL)
  total_row_label <- env_get(x, "total_row_label", default = NULL)
  missing_subjects_row_label <- env_get(x, "missing_subjects_row_label", default = NULL)
  has_missing_count <- env_get(x, "has_missing_count", default = FALSE)
  by <- env_get(x, "by")
  cols <- env_get(x, "cols", inherit = TRUE, default = NULL)
  is_built_nest <- env_get(x, "is_built_nest", default = FALSE)
  stats <- env_get(x, "stats", default = list())
  target_var <- env_get(x, "target_var")

  # PROCESS: Execute in function environment
  # Calculate the indentation length. This is needed if there are missing
  # values in a nested count layer. Length is sent to string construction and
  # used to split the string.
  indentation_length <- ifelse(is.null(indentation), 0, nchar(encodeString(indentation)))

  # For nested layers, filtering was already applied in process_nested_count_target
  # Skip redundant filter_numeric call to avoid duplicate work
  formatted_data <- numeric_data %>%
    {if (!is_built_nest) filter_numeric(., numeric_cutoff,
                                         numeric_cutoff_stat,
                                         numeric_cutoff_column,
                                         treat_var) else .} %>%
    # Mutate value based on if there is a distinct_by
    mutate(n = {
      construct_count_string(.n = n, .total = total,
                             .distinct_n = distinct_n,
                             .distinct_total = distinct_total,
                             .pct = pct,
                             .distinct_pct = distinct_pct,
                             count_fmt = format_strings[['n_counts']],
                             max_layer_length = max_layer_length,
                             max_n_width = max_n_width,
                             missing_string = missing_string,
                             missing_f_str = missing_count_string,
                             summary_var = summary_var,
                             indentation_length = indentation_length,
                             total_count_format = total_count_format,
                             missing_subjects_count_format = missing_subjects_count_format,
                             total_row_label = total_row_label,
                             missing_subjects_row_label = missing_subjects_row_label,
                             has_missing_count = has_missing_count)
    }) %>%
    # Select only columns needed for pivot to reduce memory footprint
    select(match_exact(by), summary_var, !!treat_var, match_exact(cols), n) %>%
    # Pivot table
    pivot_wider(id_cols = c(match_exact(by), "summary_var"),
                names_from = c(!!treat_var, match_exact(cols)), values_from = n,
                names_prefix = "var1_") %>%
    # Replace the by variables and target variable names with `row_label<n>`
    replace_by_string_names(quos(!!!by, summary_var))

  if (is_built_nest) {
    # I had trouble doing this in a 'tidy' way so I just did it here.
    # First column is always the outer target variable.
    # Last row label is always the inner target variable
    row_labels <- vars_select(names(formatted_data), starts_with("row_label"))
    # Replace the missing 'outer' with the original target
    # The indexing looks weird but the idea is to get rid of the matrix with the '[, 1]'
    formatted_data[is.na(formatted_data[[1]]), 1] <- formatted_data[is.na(formatted_data[[1]]),
                                                                    tail(row_labels, 1)]
    # Bind row_labels to layer environment for use by process_metadata
    x$row_labels <- row_labels
  }

  if (!is_empty(stats)) {
    # Process the statistical data formatting
    formatted_stats_data <- map(stats, process_statistic_formatting) %>%
      reduce(full_join, by = c('summary_var', match_exact(c(by, head(target_var, -1))))) %>%
      # Replace the by variables and target variable names with `row_label<n>`
      replace_by_string_names(quos(!!!by, summary_var))

    formatted_data <- full_join(formatted_data, formatted_stats_data,
                                by = vars_select(names(formatted_data), starts_with("row_label")))
  }

  # Attach the row identifier
  formatted_data <- assign_row_id(formatted_data, 'c')

  # BIND: Write results back to layer environment
  x$formatted_data <- formatted_data

  add_order_columns(x)

  env_get(x, "formatted_data")

}

#' Build vectorized format arguments for count strings
#'
#' Creates the argument list for sprintf using vectorized num_fmt_vec() instead
#' of row-by-row map_chr(values, num_fmt, ...) calls.
#'
#' @param vars_ord Character vector of variable names in order (e.g., c("n", "pct"))
#' @param count_fmt f_str object
#' @param n Numeric vector of counts
#' @param pct Numeric vector of percentages (pre-calculated, 0-1 scale)
#' @param distinct_n Numeric vector of distinct counts
#' @param distinct_pct Numeric vector of distinct percentages (pre-calculated, 0-1 scale)
#' @param total Numeric vector of totals
#' @param distinct_total Numeric vector of distinct totals
#'
#' @return List suitable for do.call(sprintf, ...)
#' @noRd
build_count_format_args <- function(vars_ord, count_fmt, n, pct, distinct_n,
                                    distinct_pct, total, distinct_total) {

  args <- list(count_fmt$repl_str)

  for (i in seq_along(vars_ord)) {
    var_name <- vars_ord[i]

    formatted <- switch(var_name,
      "n" = num_fmt_vec(n, i, count_fmt),
      "pct" = num_fmt_vec(pct * 100, i, count_fmt),
      "distinct_n" = num_fmt_vec(distinct_n, i, count_fmt),
      "distinct_pct" = num_fmt_vec(distinct_pct * 100, i, count_fmt),
      "total" = num_fmt_vec(total, i, count_fmt),
      "distinct_total" = num_fmt_vec(distinct_total, i, count_fmt)
    )

    args[[i + 1]] <- formatted
  }

  args
}

#' Format n counts for display in count_layer
#'
#' Vectorized implementation that formats all count strings in batch operations
#' rather than row-by-row. Uses pre-calculated pct and distinct_pct columns.
#'
#' left padding = (maximum_n_width - this_n_width)
#' right padding = (maximum_layer_width - this_layer_width[after left padding])
#'
#' @param .n Vector of counts for each cell
#' @param .total  Vector of totals. Should be the same length as .n and be the
#'   denominator that column is based off of.
#' @param .distinct_n Vector of distinct counts
#' @param .distinct_total Vector of total counts for distinct
#' @param .pct Vector of pre-calculated percentages (0-1 scale). If NULL, calculated on the fly.
#' @param .distinct_pct Vector of pre-calculated distinct percentages (0-1 scale). If NULL, calculated on the fly.
#' @param count_fmt The f_str object the strings are formatted around.
#' @param max_layer_length The maximum layer length of the whole table
#' @param max_n_width The maximum length of the actual numeric counts
#' @param missing_string The value of the string used to note missing. Usually NA
#' @param missing_f_str The f_str object used to display missing values
#' @param summary_var The summary_var values that contain the values of the
#'   target variable.
#' @param indentation_length If this is a nested count layer. The row prefixes
#'   must be removed
#' @param total_count_format f_str for total counts
#' @param missing_subjects_count_format f_str for missing subjects
#' @param total_row_label Label string for total rows
#' @param missing_subjects_row_label Label string for missing subjects
#' @param has_missing_count Boolean for if missing counts are present
#'
#' @return Character vector of formatted count strings
#' @noRd
construct_count_string <- function(.n, .total, .distinct_n = NULL, .distinct_total = NULL,
                                   .pct = NULL, .distinct_pct = NULL,
                                   count_fmt = NULL, max_layer_length, max_n_width, missing_string,
                                   missing_f_str, summary_var, indentation_length, total_count_format,
                                   missing_subjects_count_format, total_row_label, missing_subjects_row_label,
                                   has_missing_count) {

  # Handle defaults for nested count layers where these may not be processed yet
  if (is.null(max_layer_length)) max_layer_length <- 0
  if (is.null(max_n_width)) max_n_width <- 0

  # Use passed percentages or calculate if not provided (backwards compatibility)
  if (is.null(.pct)) {
    .pct <- replace(.n / .total, is.na(.n / .total), 0)
  }
  if (is.null(.distinct_pct)) {
    .distinct_pct <- replace(.distinct_n / .distinct_total, is.na(.distinct_n / .distinct_total), 0)
  }

  # Initialize row type masks
  missing_rows <- rep(FALSE, length(.n))
  total_rows <- rep(FALSE, length(.n))
  missing_subject_rows <- rep(FALSE, length(.n))

  # Add in the missing format if its null and there are missing counts
  if (has_missing_count && is.null(missing_f_str)) {
    missing_f_str <- count_fmt
  }

  # For nested count layers, strip indentation when checking row types
  # The 'outer' values will be cut off but they will never be "missing"
  summary_var_check <- str_sub(summary_var, indentation_length + 1)

  # Identify missing rows
  if (!is.null(missing_f_str) && !is.null(missing_string)) {
    missing_rows <- summary_var_check %in% missing_string
  }

  # Identify total rows
  if (!is.null(total_count_format) && !is.null(total_row_label)) {
    total_rows <- summary_var_check %in% total_row_label
  }

  # Identify missing subject rows
  if (!is.null(missing_subjects_count_format) && !is.null(missing_subjects_row_label)) {
    missing_subject_rows <- summary_var_check %in% missing_subjects_row_label
  }

  # Regular rows are everything else
  regular_rows <- !missing_rows & !total_rows & !missing_subject_rows

  # Initialize result vector
  result <- character(length(.n))

  # Format regular rows (vectorized)
  if (any(regular_rows)) {
    vars_ord <- map_chr(count_fmt$vars, as_name)
    args <- build_count_format_args(
      vars_ord, count_fmt,
      .n[regular_rows], .pct[regular_rows],
      .distinct_n[regular_rows], .distinct_pct[regular_rows],
      .total[regular_rows], .distinct_total[regular_rows]
    )
    result[regular_rows] <- do.call(sprintf, args)
  }

  # Format missing rows (vectorized)
  if (any(missing_rows)) {
    vars_ord <- map_chr(missing_f_str$vars, as_name)
    args <- build_count_format_args(
      vars_ord, missing_f_str,
      .n[missing_rows], .pct[missing_rows],
      .distinct_n[missing_rows], .distinct_pct[missing_rows],
      .total[missing_rows], .distinct_total[missing_rows]
    )
    result[missing_rows] <- do.call(sprintf, args)
  }

  # Format total rows (vectorized)
  if (any(total_rows)) {
    vars_ord <- map_chr(total_count_format$vars, as_name)
    args <- build_count_format_args(
      vars_ord, total_count_format,
      .n[total_rows], .pct[total_rows],
      .distinct_n[total_rows], .distinct_pct[total_rows],
      .total[total_rows], .distinct_total[total_rows]
    )
    result[total_rows] <- do.call(sprintf, args)
  }

  # Format missing subject rows (vectorized)
  if (any(missing_subject_rows)) {
    vars_ord <- map_chr(missing_subjects_count_format$vars, as_name)
    args <- build_count_format_args(
      vars_ord, missing_subjects_count_format,
      .n[missing_subject_rows], .pct[missing_subject_rows],
      .distinct_n[missing_subject_rows], .distinct_pct[missing_subject_rows],
      .total[missing_subject_rows], .distinct_total[missing_subject_rows]
    )
    result[missing_subject_rows] <- do.call(sprintf, args)
  }

  # Apply padding (vectorized)
  # Left pad set to 0 meaning it won't pad to the left at all
  # Right pad is set to the maximum n count in the table
  pad_formatted_data(result, 0, max_n_width)
}

#' Switch statement helper used in formatting (retained for shift layer compatibility)
#'
#' This function is used by construct_shift_string() and provides row-by-row
#' formatting. For count layers, the vectorized build_count_format_args() is
#' preferred for better performance.
#'
#' @param x Current parameter to format
#' @param count_fmt f_str object used to format
#' @param .n values used in 'n'
#' @param .total values used in pct calculations
#' @param .distinct_n values used in 'distinct_n'
#' @param .distinct_total values used in distinct pct
#' @param vars_ord variable order
#'
#' @noRd
count_string_switch_help <- function(x, count_fmt, .n, .total,
                                     .distinct_n, .distinct_total, vars_ord){

  switch(x,
         "n" = map_chr(.n, num_fmt, which(vars_ord == "n"), fmt = count_fmt),
         "pct" = {
           # Make a vector of ratios between n and total. Replace na values with 0
           pcts <- replace(.n/.total, is.na(.n/.total), 0)
           # Make a vector of percentages
           map_chr(pcts*100, num_fmt, which(vars_ord == "pct"), fmt = count_fmt)
         },
         "distinct_n" =  map_chr(.distinct_n, num_fmt, which(vars_ord == "distinct_n"), fmt = count_fmt),
         "distinct_pct" = {
           # Same as pct
           pcts <- replace(.distinct_n/.distinct_total, is.na(.distinct_n/.distinct_total), 0)

           map_chr(pcts*100, num_fmt, which(vars_ord == "distinct_pct"), fmt = count_fmt)
         },
         "total" = {
           map_chr(.total, num_fmt, which(vars_ord == "total"), fmt = count_fmt)
         },
         "distinct_total" = {
           map_chr(.distinct_total, num_fmt, which(vars_ord == "distinct_total"), fmt = count_fmt)
         }
  )
}

#' @param x Count Layer
#'
#' When nesting a count layer in some cases a treatment group will not apear in one of the
#' groups so this will turn the variable into a factor to force it to complete in the
#' complete logic
#' Convert treatment variable to factor
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes factor conversion in function environment
#' 3. Binds results back to layer environment
#'
#' @param x A count_layer object
#' @return The layer invisibly
#' @noRd
factor_treat_var <- function(x) {
  # EXTRACT: Get needed bindings from layer environment (parent for nested layers)
  parent_env <- env_parent(x)
  built_target <- env_get(parent_env, "built_target")
  treat_var <- env_get(parent_env, "treat_var")

  # PROCESS: Execute in function environment
  built_target[, as_name(treat_var)] <- as.factor(unlist(built_target[, as_name(treat_var)]))

  # BIND: Write results back to parent layer environment
  env_bind(parent_env, built_target = built_target)

  invisible(x)
}



#' Process count denominators
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes denominator calculations in function environment
#' 3. Binds results back to layer environment
#'
#' @param x A count_layer object
#' @return The layer invisibly
#' @noRd
process_count_denoms <- function(x) {
  # EXTRACT: Get needed bindings from layer environment (with inheritance from parent)
  target_var <- env_get(x, "target_var")
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  by <- env_get(x, "by")
  cols <- env_get(x, "cols", inherit = TRUE)
  target <- env_get(x, "target", inherit = TRUE)
  denom_ignore <- env_get(x, "denom_ignore", default = NULL, inherit=TRUE)
  missing_count_string <- env_get(x, "missing_count_string", default = NULL, inherit=TRUE)
  denom_where <- env_get(x, "denom_where", default = NULL, inherit=TRUE)
  pop_data <- env_get(x, "pop_data", inherit = TRUE)
  where <- env_get(x, "where", inherit=TRUE)
  missing_count_list <- env_get(x, "missing_count_list", default = NULL, inherit=TRUE)
  built_target_pre_where <- env_get(x, "built_target_pre_where")
  built_pop_data <- env_get(x, "built_pop_data", inherit = TRUE)
  pop_treat_var <- env_get(x, "pop_treat_var", inherit = TRUE)
  distinct_by <- env_get(x, "distinct_by", default = NULL, inherit=TRUE)
  denoms_by <- env_get(x, "denoms_by", default = NULL)

  # PROCESS: Execute in function environment
  # This used in case there is a character passed to the layer
  layer_params <- c(target_var, treat_var, by, cols)
  # Logical vector indicating if the param appears in the target dataset.
  param_apears <- map_lgl(layer_params, function(x) {
    as_name(x) %in% names(target)
  })

  # Raise errors if a denom is ignored but there isn't a missing count string
  if (!is.null(denom_ignore) && is.null(missing_count_string)) {
    abort("A value(s) were set with 'denom_ignore' but no missing count was set. Your percentages/totals may not have meaning.")
  }

  # Logic to determine how to subset target for denominators.
  if (is.null(denom_where)) {
    # If a pop_data was passed change the denom_where to the pop_data_where
    if (!isTRUE(try(identical(pop_data, target)))) {
      denom_where <- quo(TRUE)
    } else {
      # Otherwise make denom_where equal to table where
      denom_where <- where
    }
  }

  # Because the missing strings haven't replaced the missing strings, it has to happen here.
  # Expand denoms contains the
  local_denom_ignore <- denom_ignore
  if (!is.null(missing_count_list)) {
    expand_denoms <- names(missing_count_list) %in% unlist(denom_ignore)
    local_denom_ignore <- c(denom_ignore, unname(missing_count_list[expand_denoms]))
  }


  # Subset the local built_target based on where
  # Catch errors - combine filter conditions to reduce vec_slice overhead
  tryCatch({
    denom_target <- built_target_pre_where %>%
      filter(!!denom_where & !(!!target_var[[1]] %in% unlist(local_denom_ignore)))
  }, error = function(e) {
    abort(paste0("group_count `where` condition `",
                 as_label(denom_where),
                 "` is invalid. Filter error:\n", e))
  })

  # For distinct counts, we want to defer back to the
  # population dataset. Trigger this by identifying that
  # the population dataset was overridden
  if (!isTRUE(try(identical(pop_data, target)))) {
    # If the denom_where doesn't match the where AND the denom_where isn't true
    # than the user passed a custom denom_where
    if (deparse(denom_where) != deparse(where) && !isTRUE(quo_get_expr(denom_where))) {
      warning(paste0("A `denom_where` has been set with a pop_data. The `denom_where` has been ignored.",
                     "You should use `set_pop_where` instead of `set_denom_where`.", sep = "\n"),
              immediate. = TRUE)
    }
  }

  denoms_df_n <- denom_target %>%
    group_by(!!!layer_params[param_apears]) %>%
    summarize(
      n = n()
    ) %>%
    ungroup()

  # If user specified treatment var as a denom by then remove it
  # and if inside a nested layer, rename summary_var in the denoms_by
  # for building this table
  if (is.null(denoms_by)) denoms_by <- c(treat_var, cols)
  dist_grp <- denoms_by
  which_is_treatvar <- which(
    map_lgl(denoms_by, ~ as_name(.) %in% c(as_name(pop_treat_var), as_name(treat_var)))
  )
  if (length(which_is_treatvar) > 0) {
    dist_grp <- dist_grp[-which_is_treatvar]
  }
  is_svar <- map_lgl(dist_grp, ~as_name(.) == "summary_var")
  if (any(is_svar)) {
    dist_grp[[which(is_svar)]] <- layer_params[[1]]
  }

  # Issue in here somewhere. 
  denoms_df_dist <- built_pop_data %>%
    filter(!!denom_where) %>%
    group_by(!!pop_treat_var, !!!dist_grp) %>%
    summarize(
      distinct_n = n_distinct(!!!distinct_by, !!pop_treat_var)
    ) %>%
    ungroup()

  # Create merge variables to get the denoms dataframe merged correctly
  by_join <- map_chr(append(dist_grp, pop_treat_var, after=0), as_name)
  names(by_join) <- map_chr(append(dist_grp, treat_var, after=0), as_name)


  denoms_df <- denoms_df_n %>%
    left_join(denoms_df_dist, by = by_join)

  if (as_name(target_var[[1]]) %in% names(target)) {
    denoms_df <- denoms_df %>%
      rename("summary_var" := !!target_var[[1]])
  }

  # BIND: Write results back to layer environment
  x$denoms_df <- denoms_df
  x$denoms_by <- denoms_by

  invisible(x)
}

#' Rename missing values in count layer
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes missing value renaming in function environment
#' 3. Binds results back to layer environment
#'
#' @param x A count_layer object
#' @return The layer invisibly
#' @noRd
rename_missing_values <- function(x) {
  # EXTRACT: Get needed bindings from layer environment
  missing_count_list <- env_get(x, "missing_count_list", default = NULL, inherit=TRUE)
  built_target <- env_get(x, "built_target")
  target_var <- env_get(x, "target_var")

  # PROCESS: Execute in function environment
  # Rename missing values
  if (!is.null(missing_count_list)) {
    missing_count_list_ <- missing_count_list
    # If the target variable isn't a character or a factor. Coerse it as a
    # character. This can happen if the target var is numeric
    if (!(class(built_target[, as_name(target_var[[1]])][[1]]) %in% c("factor", "character"))) {
      built_target <- built_target %>%
        mutate(!!target_var[[1]] := as.character(!!target_var[[1]]))
    }
    # Collapse the factors that were missing.
    for (idx in seq_along(missing_count_list)) {

      # Logic if the missing_count_list contains an implicit NA
      if (any(is.nan(missing_count_list[[idx]]))) {
        ## Repalce the NA in the missing_count list with an explicit value
        missing_count_list_[[idx]] <- ifelse(missing_count_list[[idx]] == "NaN", "(Missing_NAN)", as.character(missing_count_list[[idx]]))
        # Replace the implicit values in built_target
        built_target <- built_target %>%
          mutate(!!target_var[[1]] := fct_expand(!!target_var[[1]], "(Missing_NAN)")) %>%
          mutate(!!target_var[[1]] := ifelse(is.nan(!!target_var[[1]]), "(Missing_NAN)", as.character(!!target_var[[1]])))

      } else if (any(is.na(missing_count_list[[idx]]))) {
        ## Repalce the NA in the missing_count list with an explicit value
        missing_count_list_[[idx]] <- ifelse(is.na(as.character(missing_count_list[[idx]])) , "(Missing)", as.character(missing_count_list[[idx]]))
        # Replace the implicit values in built_target
        built_target <- built_target %>%
          mutate(!!target_var[[1]] := fct_expand(!!target_var[[1]], "(Missing)")) %>%
          mutate(!!target_var[[1]] := fct_na_value_to_level(!!target_var[[1]], level="(Missing)"))

      }
      built_target <- built_target %>%
        mutate(
          # Warnings suppressed here. They can happen if something is called missing
          # That isn't in the data, that isn't something to warn about in this context
          !!target_var[[1]] := suppressWarnings(fct_collapse(!!target_var[[1]], !!names(missing_count_list)[idx] := missing_count_list_[[idx]]))
        )
    }

    # BIND: Write results back to layer environment
    x$built_target <- built_target
  }

  invisible(x)
}

filter_numeric <- function(.data,
                           numeric_cutoff,
                           numeric_cutoff_stat,
                           numeric_cutoff_column,
                           treat_var,
                           by = NULL) {

  if (is.null(numeric_cutoff)) {
    return(.data)
  }

  # Build combined filter condition to reduce filter() calls
  filter_cond <- if (is.null(numeric_cutoff_column)) {
    expr(!!sym(numeric_cutoff_stat) >= !!numeric_cutoff)
  } else {
    expr(!!treat_var == numeric_cutoff_column & !!sym(numeric_cutoff_stat) >= !!numeric_cutoff)
  }

  vals <- .data %>%
    filter(!!filter_cond) %>%
    pull(summary_var)

  .data %>%
    filter(summary_var %in% vals)


}

