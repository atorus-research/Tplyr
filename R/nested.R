#' Process nested count target
#'
#' This function follows the Extract-Process-Bind pattern:
#' 1. Extracts needed bindings from layer environment
#' 2. Processes nested count logic in function environment
#' 3. Binds results back to layer environment
#'
#' @param x A count_layer object with nested target variables
#' @return The layer invisibly
#' @noRd
process_nested_count_target <- function(x) {

  # EXTRACT: Get what we need from layer environment
  indentation <- x$indentation
  count_row_prefix <- env_get(x, "count_row_prefix", default = NULL)
  target_var <- x$target_var
  target <- env_get(x, "target", inherit = TRUE)
  built_target <- env_get(x, "built_target", inherit = TRUE)
  include_total_row <- x$include_total_row
  denoms_by <- x$denoms_by
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  cols <- env_get(x, "cols", inherit = TRUE)
  by <- x$by
  where <- x$where
  numeric_cutoff <- x$numeric_cutoff
  numeric_cutoff_stat <- x$numeric_cutoff_stat
  numeric_cutoff_column <- x$numeric_cutoff_column
  total_row_label <- x$total_row_label
  missing_subjects_row_label <- x$missing_subjects_row_label

  # PROCESS: Work in function environment
  # Use count_row_prefix if indentation is not set
  if(is.null(indentation)) {
    indentation <- if(!is.null(count_row_prefix)) count_row_prefix else "   "
  }

  assert_that(quo_is_symbol(target_var[[2]]),
              msg = "Inner layers must be data driven variables")

  if(is.factor(built_target[[as_name(target_var[[1]])]])) {
    warning(paste0("Factors are not currently supported in nested count layers",
                   " that have two data driven variables. Factors will be coerced into character vectors"),
            immediate. = TRUE)
  }
  if(is.factor(built_target[[as_name(target_var[[2]])]]) && quo_is_symbol(target_var[[1]])) {
    warning(paste0("Factors are not currently supported in nested count layers",
                   " that have two data driven variables. Factors will be coerced into character vectors"),
            immediate. = TRUE)
  }

  if (isTRUE(include_total_row)) {
    abort("You can't include total rows in nested counts. Instead, add a seperate layer for total counts.")
  }

  if (!is.null(denoms_by)) {
    change_denom_ind <- map_chr(denoms_by, as_name) %in% "summary_var"
    second_denoms_by <- denoms_by
    second_denoms_by[change_denom_ind] <- quos(!!target_var[[1]])
  } else {
    denoms_by <- c(treat_var, cols)
    second_denoms_by <- denoms_by
  }

  # Missing subject counts should not occur in the outer layer
  fl <- group_count(x, target_var = !!target_var[[1]],
                    by = vars(!!!by), where = !!where)
  env_bind(fl, denoms_by = denoms_by)
  fl$include_missing_subjects_row <- FALSE
  x$outer_ <- TRUE
  first_layer <- process_summaries(fl)

  x$outer_ <- FALSE
  second_layer <- process_summaries(group_count(x, target_var = !!target_var[[2]],
                                                by = vars(!!target_var[[1]], !!!by), where = !!where) %>%
                                      set_count_row_prefix(indentation) %>%
                                      set_denoms_by(!!!second_denoms_by))

  first_layer_final <- first_layer$numeric_data

  # Apply numeric cutoff filter first
  second_layer_filtered <- second_layer$numeric_data %>%
    filter_numeric(
      numeric_cutoff = numeric_cutoff,
      numeric_cutoff_stat = numeric_cutoff_stat,
      numeric_cutoff_column = numeric_cutoff_column,
      treat_var = treat_var,
      by = by
    )

  # Use vectorized filtering instead of do()
  second_layer_final <- filter_nested_inner_layer_vectorized(
    second_layer_filtered,
    target,
    target_var[[1]],
    target_var[[2]],
    indentation,
    missing_subjects_row_label
  )

  ignored_filter_rows <- ifelse(include_total_row,
                                ifelse(is.null(total_row_label),
                                       "Total",
                                       total_row_label),
                                character(0))

  # Bind the numeric data together
  numeric_data <- bind_rows(first_layer_final, second_layer_final) %>%
    filter_nested_numeric(
      numeric_cutoff,
      numeric_cutoff_stat,
      numeric_cutoff_column,
      treat_var,
      target_var,
      ignored_filter_rows
    )

  # Save the original by and target_vars incase the layer is rebuilt
  by_saved <- by
  target_var_saved <- target_var
  is_built_nest <- TRUE

  by_new <- vars(!!target_var[[1]], !!!by)
  target_var_new <- vars(!!target_var[[2]])

  # BIND: Write results back to layer environment
  x$numeric_data <- numeric_data
  x$by_saved <- by_saved
  x$target_var_saved <- target_var_saved
  x$is_built_nest <- is_built_nest
  x$by <- by_new
  x$target_var <- target_var_new
  x$indentation <- indentation

  invisible(x)
}

#' Vectorized filtering of nested inner layer values
#'
#' This function replaces the do() + filter_nested_inner_layer() pattern
#' with a fully vectorized approach. Instead of processing each outer group
#' separately, it builds a lookup of valid outer-inner combinations and
#' filters in one pass.
#'
#' @param .data The data frame to filter (second layer numeric data)
#' @param target The target dataset
#' @param outer_name Quosure for outer variable name
#' @param inner_name Quosure for inner variable name
#' @param indentation The indentation prefix for inner values
#' @param missing_subjects_row_label Label for missing subjects row
#' @return Filtered data frame
#' @noRd
filter_nested_inner_layer_vectorized <- function(.data, target, outer_name, inner_name,
                                                  indentation, missing_subjects_row_label) {

  # Is outer variable text? If it is, all inner values are valid
  text_outer <- !quo_is_symbol(outer_name)
  outer_name_chr <- as_name(outer_name)
  inner_name_chr <- as_name(inner_name)

  if (text_outer) {
    # For text outer, all inner values in target are valid (plus factor levels)
    lvs <- levels(target[[inner_name_chr]])
    all_valid_inner <- target %>%
      select(any_of(inner_name_chr)) %>%
      unlist() %>%
      c(lvs) %>%
      unique() %>%
      paste0(indentation, .)

    # Add missing subjects label
    all_valid_inner <- c(all_valid_inner, paste0(indentation, missing_subjects_row_label))

    # Simple filter - all rows with valid summary_var values
    return(.data %>% filter(summary_var %in% all_valid_inner))
  }

  # For symbol outer: build a lookup table of valid (outer, inner) combinations
  # This is the vectorized replacement for the per-group filtering

  # Get all unique outer-inner combinations from the target data
  valid_combinations <- target %>%
    select(all_of(c(outer_name_chr, inner_name_chr))) %>%
    distinct() %>%
    mutate(
      # Add indentation to inner values to match summary_var format
      valid_summary_var = paste0(indentation, .data[[inner_name_chr]]),
      # Convert outer to character to match .data which may have factor/character conversion
      !!outer_name_chr := as.character(.data[[outer_name_chr]])
    ) %>%
    select(all_of(outer_name_chr), valid_summary_var)

  # Also allow missing subjects row for each outer value
  outer_values <- unique(valid_combinations[[outer_name_chr]])
  missing_rows <- tibble(
    !!outer_name_chr := outer_values,
    valid_summary_var = paste0(indentation, missing_subjects_row_label)
  )

  valid_combinations <- bind_rows(valid_combinations, missing_rows)

  # Ensure .data's outer column is also character for joining
  .data <- .data %>%
    mutate(!!outer_name_chr := as.character(.data[[outer_name_chr]]))

  # Now filter .data by joining with valid combinations
  # A row is valid if its (outer_value, summary_var) pair exists in valid_combinations
  .data %>%
    inner_join(
      valid_combinations,
      by = setNames("valid_summary_var", "summary_var") %>%
        c(setNames(outer_name_chr, outer_name_chr))
    )
}

#' This function resets the variables for a nested layer after it was built
#' @noRd
refresh_nest <- function(x) {
  env_bind(x, by = env_get(x, "by_saved"))
  env_bind(x, target_var = env_get(x, "target_var_saved"))
}


filter_nested_numeric <- function(.data,
                                  numeric_cutoff,
                                  numeric_cutoff_stat,
                                  numeric_cutoff_column,
                                  treat_var,
                                  target_var,
                                  ignored_rows) {

  if (is.null(numeric_cutoff)) {
    return(.data)
  }

  # All of the non NA values in target_var[[1]] are what we want to keep here.
  # Because those are the 'inner' values that passed the filter
  vals <- .data %>%
    {if (is.null(numeric_cutoff_column)) . else filter(., !!treat_var == numeric_cutoff_column)} %>%
    filter(!is.na(!!target_var[[1]])) %>%
    extract2(as_name(target_var[[1]])) %>%
    as.character()

  .data %>%
    filter(!is.na(!!target_var[[1]]) | summary_var %in% c(vals, ignored_rows))


}
