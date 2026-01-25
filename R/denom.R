

#' Get the value of the header_n value in a grouped df
#'
#' This is intended to be called in a dplyr context using `do`. This function
#' returns the value of the header_n for the current group. `do` is currently
#' labeled as questioning but its replacement, group_map, is experimental
#'
#' @param .data A tibble that has been grouped
#' @param header_n The header_n binding from a \code{tplyr_table} object.
#'
#' @return A single numeric value from the header_n binding that is pulled
#'  from the groups
#'
#' @examples
#' library(dplyr)
#'
#' t <- tplyr_table(mtcars, gear)
#'
#' mtcars %>%
#'   group_by(gear, vs) %>%
#'   do(this_denom(., header_n(t), treat_var(t)))
#'
#' @noRd
this_denom <- function(.data, header_n, treat_var) {
  # Rename the first column of the header_n to the treat_var, is saved as the
  # pop_treat_var at first
  names(header_n)[1] <- as_name(treat_var)

  # Get names of header_n execpt last one, last should be the count
  header_n_grp_names <- names(header_n)[1:ncol(header_n) - 1]

  # Pull out each unique filter requirement. Each name for header_n is stored
  # on the LHS and its unique value in the function is on the RHS.
  # Examples
  # gear == 3
  # am == 1
  # These are stored in a list that is evaluated below
  filter_logic <- map(header_n_grp_names, function(x) {
    expr(!!as.symbol(x) == !!unique(.data[, x])[[1]])
  })

  # Evaluate the filter above and pull out the 'n' column
  totals <- header_n %>%
    filter(!!!filter_logic) %>%
    select(n) %>%
    sum()

  # Bind the totals to the .data
  .data$total <- totals

  .data
}


#' Get the header_n value of a certain
#'
#' @param x A tplyr_table or data.frame object
#' @param ... Parameters to filter the header_n. Should be in the order of
#'   variables passed in the tplyr_table
#'
#' @return The sum of the subset of the header_n values after filtering
#' @noRd
#'
#' @examples
#' library(dplyr)
#'
#' t <- tplyr_table(mtcars, gear, cols = vars(cyl, am))
#'
#' get_header_n_value(t, 3, 6, 0)
#' # Returns the number of cars that have 3 gears, 6 cylinders, and auto transmission
get_header_n_value <- function(x, ...) {
  UseMethod("get_header_n_value")
}

#' @noRd
get_header_n_value.tplyr_table <- function(x, ...) {
  # Arguments passed
  #dots <- enquos(...)

  header_names <- names(header_n(x))

  # You can use all columns in the header_n except the last, find the minimum
  # of the number of dots passed and the columns you can subset on
  param_num <- min(length(header_names) - 1, length(dots))

  # Just pull out the names you are selecting for
  dots_names <- header_names[1:param_num]

  ## I tried this in a map but I had trouble with the names being stripped out
  filter_logic <- list()
  for (i in seq_along(dots)) {
    filter_logic <- append(filter_logic, expr(!!as.symbol(dots_names[i]) == !!dots[[i]]))
  }

  # filter_logic <- map(dots, function(x) {
  #   print(x)
  #   print(class(x))
  #   print(str(x))
  #   expr(!!as.symbol(names(x)) == !!x)
  #   })

  header_n(x) %>%
    filter(!!!filter_logic) %>%
    select(n) %>%
    sum()
}

#' @noRd
get_header_n_value.data.frame <- function(x, ...) {
  # Arguments passed
  #dots <- enquos(...)

  header_names <- names(x)

  # You can use all columns in the header_n except the last, find the minimum
  # of the number of dots passed and the columns you can subset on
  param_num <- min(length(header_names) - 1, length(...))

  # Just pull out the names you are selecting for
  dots_names <- header_names[1:param_num]

  ## I tried this in a map but I had trouble with the names being stripped out
  filter_logic <- list()
  for (i in seq_along(...)) {
    rhs_logic <- as.character(...[[i]])
    filter_logic <- append(filter_logic, expr(!!as.symbol(dots_names[i]) == !!rhs_logic))
  }

  x %>%
    filter(!!!filter_logic) %>%
    select(n) %>%
    sum()

}

#' Get the denominator used in shift pcts
#'
#' This is meant to be called in a dplyr grouped context
#'
#' @param .data A data.frame that has been grouped
#' @param denoms_by The variables used to get the denoms from
#' @param denoms_df The denoms_df that is created during layer processing.
#'   Contains the unique combinations of all layer parameters and their counts.
#' @param denoms_distinct_df The values calculated that represent the distinct
#'   denominator values used in a pct.
#' @param total_extract Either 'n' or distinct_n
#'
#' @return A data.frame with the
#' @noRd
get_denom_total <- function(.data, denoms_by, denoms_df,
                            total_extract = "n") {

  # Filter denoms dataset
  vars_in_denoms <- denoms_by[map_lgl(denoms_by, ~ as_name(.) %in% names(denoms_df))]
  filter_logic <- map(vars_in_denoms, function(x) {
    if (nrow(.data) > 0) {
      expr(!!sym(as_name(x)) == !!unique(.data[, as_name(x)])[[1]])
    } else {
      FALSE
    }
  })

  sums <-  denoms_df %>%
    filter(!!!filter_logic) %>%
    group_by(!!!vars_in_denoms)

    .data$total <- ifelse(nrow(sums) > 0, sum(sums[["n"]], na.rm = TRUE), 0)
    # distinct_n is present for all count layers, but not shift layers, so
    # dont' do this for shift layers
    if ("distinct_n" %in% names(sums)) {

      merge_vars <- names(sums)[!(names(sums) %in% c('n', 'distinct_n'))]
      dist_tot <- sums %>%
        select(everything(), -n, distinct_total = distinct_n)

      # summary_var may be used for grouping denoms so only toss it if
      # it's not in denoms_by
      if (!('summary_var' %in% map_chr(vars_in_denoms, as_name)) & 'summary_var' %in% names(sums)) {
        merge_vars <- merge_vars[merge_vars != 'summary_var']
        dist_tot <- dist_tot %>%
          select(-summary_var)
      }

      dist_tot <- dist_tot %>% distinct()

      .data <- .data %>%
        left_join(
          dist_tot, by = merge_vars
        )
    }

  .data

}

#' Vectorized version of get_denom_total
#'
#' This function replaces the do(get_denom_total()) pattern with a single
#' vectorized join operation. Instead of filtering denoms_df per-group,
#' it pre-aggregates the totals and joins them in one pass.
#'
#' @param .data A data frame (not grouped)
#' @param denoms_by Quosures specifying the denominator grouping variables
#' @param denoms_df The denominator data frame with n and optionally distinct_n
#' @param total_extract Column to use for total (default "n", kept for API compatibility)
#' @return Data frame with total and optionally distinct_total columns added
#' @noRd
get_denom_total_vectorized <- function(.data, denoms_by, denoms_df,
                                        total_extract = "n") {

  if (nrow(.data) == 0) {
    .data$total <- numeric(0)
    if ("distinct_n" %in% names(denoms_df)) {
      .data$distinct_total <- numeric(0)
    }
    return(.data)
  }

  # Find which denoms_by variables exist in denoms_df
  vars_in_denoms <- denoms_by[map_lgl(denoms_by, ~ as_name(.) %in% names(denoms_df))]
  join_vars <- map_chr(vars_in_denoms, as_name)

  if (length(join_vars) == 0) {
    # No join variables - use total sum for all rows
    .data$total <- sum(denoms_df[["n"]], na.rm = TRUE)
    if ("distinct_n" %in% names(denoms_df)) {
      .data$distinct_total <- sum(denoms_df[["distinct_n"]], na.rm = TRUE)
    }
    return(.data)
  }

  # Coerce join columns to character in both data frames to avoid type mismatches
  # This matches the behavior of the original do() approach which performed
  # value-based filtering that was type-agnostic
  denoms_df_coerced <- denoms_df
  for (jv in join_vars) {
    if (jv %in% names(.data) && jv %in% names(denoms_df_coerced)) {
      .data[[jv]] <- as.character(.data[[jv]])
      denoms_df_coerced[[jv]] <- as.character(denoms_df_coerced[[jv]])
    }
  }

  # Pre-aggregate totals by the join variables
  totals_df <- denoms_df_coerced %>%
    group_by(across(all_of(join_vars))) %>%
    summarize(total = sum(n, na.rm = TRUE), .groups = "drop")

  # Handle distinct_n if present (count layers have it, shift layers don't)
  if ("distinct_n" %in% names(denoms_df_coerced)) {
    # Determine merge variables for distinct totals
    # Exclude 'n' and 'distinct_n' from merge vars
    merge_vars <- join_vars

    # summary_var may be used for grouping denoms so only toss it if
    # it's not in denoms_by
    denoms_by_names <- map_chr(denoms_by, as_name)
    if (!('summary_var' %in% denoms_by_names) & 'summary_var' %in% names(denoms_df_coerced)) {
      merge_vars <- merge_vars[merge_vars != 'summary_var']
    }

    # Get distinct totals
    distinct_df <- denoms_df_coerced %>%
      select(all_of(c(merge_vars, "distinct_n"))) %>%
      rename(distinct_total = distinct_n) %>%
      distinct()

    # Join totals first, then distinct totals
    .data <- .data %>%
      left_join(totals_df, by = join_vars) %>%
      left_join(distinct_df, by = merge_vars)
  } else {
    # Shift layers - just join totals
    .data <- .data %>%
      left_join(totals_df, by = join_vars)
  }

  # Replace NA totals with 0 (matches original behavior)
  .data$total <- ifelse(is.na(.data$total), 0, .data$total)

  # Arrange by denoms_by variables to match the grouping order from original do() approach
  # The original do() function sorted output by the grouping variables
  denoms_by_names <- map_chr(denoms_by, as_name)
  arrange_vars <- denoms_by_names[denoms_by_names %in% names(.data)]
  if (length(arrange_vars) > 0) {
    .data <- .data %>%
      arrange(across(all_of(arrange_vars)))
  }

  .data
}
