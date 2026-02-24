

#### Helpers ####

#' Find row indices for special rows (total, missing, missing subjects)
#'
#' Locates the positions of total, missing, and missing subjects rows within
#' formatted data by matching labels in the last row_label column.
#'
#' @param formatted_data The formatted data frame
#' @param missing_string String identifying missing rows
#' @param total_row_label Label for the total row
#' @param missing_subjects_row_label Label for missing subjects row
#'
#' @return A named list with elements `missing`, `total`, and
#'   `missing_subjects`, each NULL or an integer vector of row indices
#' @noRd
find_special_row_indices <- function(formatted_data,
                                     missing_string = NULL,
                                     total_row_label = NULL,
                                     missing_subjects_row_label = NULL) {
  label_row_ind <- which(names(formatted_data) %in%
                           tail(vars_select(names(formatted_data),
                                            starts_with("row_label")), 1))
  label_values <- unlist(formatted_data[, label_row_ind])

  list(
    missing = if (!is.null(missing_string)) which(label_values %in% missing_string),
    total = if (!is.null(total_row_label)) which(label_values %in% total_row_label),
    missing_subjects = if (!is.null(missing_subjects_row_label))
      which(label_values %in% missing_subjects_row_label)
  )
}

#' Check for VARN variable
#'
#' For needed by variables, looks for a <by>N. e.g. VISITN
#'
#' @param .data The target dataset
#' @param a_by A character or quosure vector
#'
#' @return A logical vector indicating if there is a <VAR>N variable for the
#'   indicated <VAR>
#'
#' @noRd
has_varn <- function(.data, a_by) {

  # Vectorized function - Accepts quosure or character
  has_n <- map_lgl(a_by, function(x) {
    # as_name will return character for quosure or character
      a_by_n <- paste0(as_name(x), "N")
      a_by_n %in% names(.data)
  })

  # The map is naming this for some reason so here I'm unnaming it
  unname(has_n)
}

#' Get the value of the by variable in one column and its 'N' in the other
#'
#' @param .data The target dataset
#' @param a_by A single variable name as a character or quosure
#'
#' @return A two column data.frame with the unique combinations of <VAR> and
#'   <VAR>N.
#' @noRd
get_varn_values <- function(.data, a_by) {

  # A data.frame with only the by variable and its <by>N counterpart
  varn_df <- unique(.data[, c(as_name(a_by), paste0(as_name(a_by), "N"))])

  # For the <VAR>N to be well formed there should be one and only one N for each value.
  # Therefore the number unique indices should be the number of total rows.
  assert_that(nrow(unique(varn_df)) == nrow(varn_df),
              msg = "Bad indices were pulled when ordering on the by variable")

  varn_df
}

add_layer_index <- function(layer_output, index) {
  layer_output$`ord_layer_index` <- index

  layer_output
}

#' Add columns for ordering the table after output
#'
#' @param x A \code{tplyr_layer} object
#'
#' @return Nothing, the ord_ columns are added to the environment silently
#' @noRd
add_order_columns <- function(x) {
  UseMethod("add_order_columns")
}

#' @noRd
add_order_columns.count_layer <- function(x) {
  # Counting has the most complex sorting methods. Here is an attempt and
  # showing the flow.
    # 1) A few defaults are set if they are not set yet. result_order_var,
    # ordering_cols, order_count_method
    # 2) If the count layer is nested:
      # a. The by labels are added the same way they are for all layers.
      # b. The grouping and grouped variables are pulled out and passed to the
      #    add_data_order_nested function, which is very large.
      # c. The row_labels are collapsed if the flag is set for that.
    # 3) If the count layer isn't nested:
      # a. The by variables are calculated.
      # b. The final ordering column is passed to get_data_order where it is
      #    dispatched based on the counting flag.

  # EXTRACT: Pull necessary bindings from layer environment

  formatted_data <- env_get(x, "formatted_data")
  break_ties <- env_get(x, "break_ties", default = NULL)
  result_order_var <- env_get(x, "result_order_var", default = NULL)
  ordering_cols <- env_get(x, "ordering_cols", default = NULL)
  order_count_method <- env_get(x, "order_count_method", default = NULL)
  pop_data <- env_get(x, "pop_data", inherit = TRUE)
  pop_treat_var <- env_get(x, "pop_treat_var", inherit = TRUE)
  is_built_nest <- env_get(x, "is_built_nest", default = FALSE)
  by <- env_get(x, "by")
  target <- env_get(x, "target", inherit = TRUE)
  indentation <- env_get(x, "indentation", default = NULL)
  treat_var <- env_get(x, "treat_var", inherit = TRUE)
  cols <- env_get(x, "cols", inherit = TRUE)
  numeric_data <- env_get(x, "numeric_data")
  target_var <- env_get(x, "target_var")
  outer_inf <- env_get(x, "outer_inf", default = NULL)
  numeric_cutoff <- env_get(x, "numeric_cutoff", default = NULL)
  numeric_cutoff_stat <- env_get(x, "numeric_cutoff_stat", default = NULL)
  numeric_cutoff_column <- env_get(x, "numeric_cutoff_column", default = NULL)
  missing_subjects_row_label <- env_get(x, "missing_subjects_row_label", default = NULL)
  missing_subjects_sort_value <- env_get(x, "missing_subjects_sort_value", default = NULL)
  total_row_sort_value <- env_get(x, "total_row_sort_value", default = NULL)
  nest_count <- env_get(x, "nest_count", default = NULL)
  include_total_row <- env_get(x, "include_total_row", default = FALSE)
  total_row_label <- env_get(x, "total_row_label", default = NULL)
  missing_string <- env_get(x, "missing_string", default = NULL)
  missing_sort_value <- env_get(x, "missing_sort_value", default = NULL)
  missing_count_list <- env_get(x, "missing_count_list", default = NULL)

  # PROCESS: Conduct the data processing
  if (nrow(formatted_data) == 0) return(formatted_data)

  # Set all defaults for ordering
  if (is.null(result_order_var)) result_order_var <- quo(n)
  # A lot of weird stripping of the object is done here to make sure its the
  # right class
  if (is.null(ordering_cols)) ordering_cols <- quos(!!unname(unlist(as.character(
    head(unique(pop_data[, as_name(pop_treat_var)]), 1)
    ))))
  if (is.null(order_count_method)) order_count_method <- "byfactor"

  # If it is a nested count_layer
  if (is_built_nest) {

    # Number of sorting columns needed, number of bys plus one for the target_var
    formatted_col_index <- length(by) + 1

    for (by_i in seq_along(by)) {
      a_by <- by[[by_i]]
      # If a_by is a character, add the index itself
      if (!is.name(quo_get_expr(a_by))) {
        formatted_data[, paste0("ord_layer_", by_i)] <- by_i
      } else {
        # Otherwise determine data order
        formatted_data[, paste0("ord_layer_", by_i)] <- get_by_order(formatted_data, target, by_i, a_by)
      }
    }

    # Used to remove the prefix. String is encoded to get controlled characters i.e. \t
    indentation_length <- ifelse(!is.null(indentation), nchar(encodeString(indentation)), 3)

    # Only the outer columns
    filter_logic <- map2(c(treat_var, cols), ordering_cols, function(x, y) {
      expr(!!sym(as_name(x)) == !!as_name(y))
    })

    # Identify the outer layer and attach it to the filter logic
    filter_logic <- append(filter_logic, ifelse(
      quo_is_symbol(by[[1]]), # Is the outside variable character or a symbol?
      exprs(is.na(!!by[[1]])), # For symbols, the outer var will be NA
      exprs(summary_var == !!by[[1]]) # For character, it will match summary_var
    ))

    all_outer <- numeric_data %>%
      filter(!!!filter_logic)

    # Add the ordering of the pieces in the layer using vectorized approach
    formatted_data <- add_data_order_nested_vectorized(
      formatted_data, formatted_col_index - 1, numeric_data,
      indentation_length = indentation_length,
      ordering_cols = ordering_cols,
      treat_var = treat_var, by = by, cols = cols,
      result_order_var = result_order_var,
      target_var = target_var,
      order_count_method = order_count_method,
      target = target, all_outer = all_outer,
      filter_logic = filter_logic,
      indentation = indentation,
      outer_inf = outer_inf,
      break_ties = break_ties,
      numeric_cutoff = numeric_cutoff,
      numeric_cutoff_stat = numeric_cutoff_stat,
      numeric_cutoff_column = numeric_cutoff_column,
      missing_subjects_row_label = missing_subjects_row_label,
      missing_subjects_sort_value = missing_subjects_sort_value,
      total_row_sort_value = total_row_sort_value
    )

    if (!is.null(nest_count) && nest_count) {
      # If the table nest should be collapsed into one row.
      row_label_names <- vars_select(names(formatted_data), starts_with("row_label"))
      # Remove first row
      formatted_data[, 1] <- NULL
      # Rename row labels
      names(formatted_data)[length(by)] <- head(row_label_names, -1)
    }

  # If it isn't a nested count layer
  } else {

    # Number of sorting columns needed, number of bys plus one for the target_var
    formatted_col_index <- length(by) + 1

    # This adds a column for each by variable and the target variable.
    for (by_i in seq_along(by)) {
      a_by <- by[[by_i]]
      # If a_by is a character, add the index itself
      if (!is.name(quo_get_expr(a_by))) {
        formatted_data[, paste0("ord_layer_", by_i)] <- by_i
      } else {
        # Otherwise determine data order
        formatted_data[, paste0("ord_layer_", by_i)] <- get_by_order(formatted_data, target, by_i, a_by)
      }
    }

    # This is used to permute any missing ordering values for total rows
    na_ordered_row <- NA
    if (include_total_row) {
      ord_cols <- vars_select(names(formatted_data), starts_with("ord"))
      na_ordered_row <- unique(map_int(ord_cols, function(col) {
        isna_ <- which(is.na(formatted_data[, col]) & formatted_data[, formatted_col_index] == total_row_label)
        if (length(isna_) == 0) return(NA)
        else return(isna_)
      }))
    }

    formatted_data[, paste0("ord_layer_", formatted_col_index)] <- get_data_order_count(
      formatted_data, formatted_col_index,
      order_count_method = order_count_method,
      numeric_data = numeric_data,
      ordering_cols = ordering_cols,
      treat_var = treat_var,
      by = by,
      cols = cols,
      result_order_var = result_order_var,
      target_var = target_var,
      target = target,
      missing_string = missing_string,
      total_row_label = total_row_label,
      missing_subjects_row_label = missing_subjects_row_label,
      missing_sort_value = missing_sort_value,
      total_row_sort_value = total_row_sort_value,
      missing_subjects_sort_value = missing_subjects_sort_value,
      break_ties = break_ties,
      numeric_cutoff = numeric_cutoff,
      numeric_cutoff_stat = numeric_cutoff_stat,
      numeric_cutoff_column = numeric_cutoff_column,
      missing_count_list = missing_count_list
    )

    # If there is a total row that is missing some ord values, they should fall
    # back from the last one.
    if (length(na_ordered_row) == 0) na_ordered_row <- NA
    if (include_total_row && !is.na(na_ordered_row)) {
      # This is the ord columns that are NA
      na_ord_cols <- which(is.na(formatted_data[na_ordered_row, ord_cols]))
      # Change those columns to the last value in the formatted table.
      formatted_data[na_ordered_row, ord_cols[na_ord_cols]] <-
        formatted_data[na_ordered_row, ncol(formatted_data)][[1]]
    }
  }

  # BIND: Bind necessary variables back into layer
  x$formatted_data <- formatted_data
  x$result_order_var <- result_order_var
  x$ordering_cols <- ordering_cols
  x$order_count_method <- order_count_method
  # For nested count layers, indentation_length is used by meta-builders
  if (is_built_nest) {
    x$indentation_length <- indentation_length
  }

  invisible(x)
}

#' @noRd
add_order_columns.desc_layer <- function(x) {

  # EXTRACT: Pull necessary bindings from layer environment
  formatted_data <- env_get(x, "formatted_data")
  by <- env_get(x, "by")
  target <- env_get(x, "target", inherit = TRUE)

  # PROCESS: Conduct the data processing

  # This adds a column for each by variable and the target variable.
  for (by_i in seq_along(by)) {
    a_by <- by[[by_i]]
    # If a_by is a character, add the index itself
    if (!is.name(quo_get_expr(a_by))) {
      formatted_data[, paste0("ord_layer_", by_i)] <- by_i
    } else {
      # Otherwise determine data order
      formatted_data[, paste0("ord_layer_", by_i)] <- get_by_order(formatted_data, target, by_i, a_by)
    }
  }

  # Number of sorting columns needed, number of bys plus one for the target_var
  formatted_col_index <- length(by) + 1

  if (formatted_col_index > 1) {
    formatted_data <- formatted_data %>%
      group_by(!!sym(paste0("ord_layer_", formatted_col_index - 1))) %>%
      mutate(!!sym(paste0("ord_layer_", formatted_col_index)) := row_number()) %>%
      ungroup()
  } else {
    formatted_data[, paste0("ord_layer_", formatted_col_index)] <- seq(nrow(formatted_data))
  }

  # BIND: Bind necessary variables back into layer
  x$formatted_data <- formatted_data

  invisible(x)
}

#' @noRd
add_order_columns.shift_layer <- function(x) {

  # EXTRACT: Pull necessary bindings from layer environment
  formatted_data <- env_get(x, "formatted_data")
  by <- env_get(x, "by")
  target <- env_get(x, "target", inherit = TRUE)
  target_var <- env_get(x, "target_var")
  total_row_sort_value <- env_get(x, "total_row_sort_value", default = NULL)
  missing_subjects_sort_value <- env_get(x, "missing_subjects_sort_value", default = NULL)

  # PROCESS: Conduct the data processing

  # This adds a column for each by variable and the target variable.
  for (by_i in seq_along(by)) {
    a_by <- by[[by_i]]
    # If a_by is a character, add the index itself
    if (!is.name(quo_get_expr(a_by))) {
      formatted_data[, paste0("ord_layer_", by_i)] <- by_i
    } else {
      # Otherwise determine data order
      formatted_data[, paste0("ord_layer_", by_i)] <- get_by_order(formatted_data, target, by_i, a_by)
    }
  }

  # Number of sorting columns needed, number of bys plus one for the target_var
  formatted_col_index <- length(by) + 1

  #### The Factor data order method.
  target_data <- target[, as_name(target_var$row)]

  # Pull levels from target variable
  target_fact <- levels(target_data[[1]])

  # If the levels are null, the target was not a factor. So turn it into a factor
  if (is.null(target_fact)) {

    # Change target variable into a factor
    target_fact <- as.factor(unlist(target_data))

    # Create data.frame with levels and index
    fact_df <- tibble(
      !!target_var$row := unique(target_fact),
      factor_index := unclass(unique(target_fact))
    )

  } else {
    # Create data.frame with levels and index
    fact_df <- tibble(
      !!target_var$row := target_fact,
      factor_index := seq_along(target_fact)
    )
  }

  # Find explicit indices of special rows
  total_row_label <- env_get(x, "total_row_label", default = NULL)
  missing_subjects_row_label <- env_get(x, "missing_subjects_row_label", default = NULL)
  row_inds <- find_special_row_indices(formatted_data,
                                       total_row_label = total_row_label,
                                       missing_subjects_row_label = missing_subjects_row_label)

  # The logic is the same now for a byvarn so reuse that function
  formatted_data[, paste0("ord_layer_", formatted_col_index)] <-
    get_data_order_byvarn(formatted_data, fact_df, as_name(target_var$row),
                          formatted_col_index,
                          total_index = row_inds$total,
                          total_row_sort_value = total_row_sort_value,
                          missing_subjects_index = row_inds$missing_subjects,
                          missing_subjects_sort_value = missing_subjects_sort_value)

  # BIND: Bind necessary variables back into layer
  x$formatted_data <- formatted_data

  invisible(x)
}

#' Return the indicies of the rows based on the by variables
#'
#'
#' @param formatted_data The formatted_data object from the layer
#' @param target The target dataset in the tplyr_table
#' @param i The index of the by variable i.e. the order it was passed in the
#'   \code{tplyr_layer}. This is also the column the by variable is output in
#'   the formatted_data.
#' @param var The by variable as a quosure
#'
#' @noRd
get_by_order <- function(formatted_data, target, i, var) {

  # The levels of the factor in the target data.frame. Will be null if its
  # not a factor
  levels_i <- levels(target[[as_name(var)]])

  # If variable is factor
  if (!is.null(levels_i)) {


    # Unlist to pull it out of the tibble, order it based on the orders in the target
    # data.frame, unclass it to pull out the index
    as.numeric(unclass(ordered(unlist(formatted_data[, i]), levels_i)))

    # If variable is varn
  } else if (has_varn(target, as_name(var))) {

    varn_df <- get_varn_values(target, as_name(var))

    as.numeric(get_data_order_byvarn(formatted_data, varn_df, as_name(var), i))

    # If it is a factor, just use levels to sort
  } else {

    # Unlist to get out of tibble, turn into factor which will order it alphabeticlly
    # unclass it to get it as a number
    as.numeric(unclass(as.factor(unlist(formatted_data[, i]))))
  }
}

#' Add the order for the target variable (count layer version)
#'
#' Refactored version that takes explicit parameters instead of using evalq.
#' Handles 'bycount', 'byvarn', and 'byfactor' sorting methods.
#'
#' @param formatted_data The formatted_data data frame
#' @param formatted_col_index The column index of the target variable data
#' @param order_count_method The sorting method ("bycount", "byvarn", or "byfactor")
#' @param numeric_data The numeric_data data frame
#' @param ordering_cols Columns used for ordering
#' @param treat_var Treatment variable
#' @param by By variables
#' @param cols Column variables
#' @param result_order_var Variable used for result ordering
#' @param target_var Target variable(s)
#' @param target Target dataset
#' @param missing_string String identifying missing values
#' @param total_row_label Label for total row
#' @param missing_subjects_row_label Label for missing subjects row
#' @param missing_sort_value Sort value for missing
#' @param total_row_sort_value Sort value for total row
#' @param missing_subjects_sort_value Sort value for missing subjects
#' @param break_ties How to break ties
#' @param numeric_cutoff Numeric cutoff value
#' @param numeric_cutoff_stat Statistic for numeric cutoff
#' @param numeric_cutoff_column Column for numeric cutoff
#' @param missing_count_list List of missing count values
#'
#' @return Returns the index the variables should be ordered in
#' @noRd
get_data_order_count <- function(formatted_data, formatted_col_index,
                                  order_count_method, numeric_data,
                                  ordering_cols, treat_var, by, cols,
                                  result_order_var, target_var, target,
                                  missing_string = NULL, total_row_label = NULL,
                                  missing_subjects_row_label = NULL,
                                  missing_sort_value = NULL,
                                  total_row_sort_value = NULL,
                                  missing_subjects_sort_value = NULL,
                                  break_ties = NULL, numeric_cutoff = NULL,
                                  numeric_cutoff_stat = NULL,
                                  numeric_cutoff_column = NULL,
                                  missing_count_list = NULL) {

  # Switch for the sorting method
  if (order_count_method == "bycount") {

    # Get the indices of special rows (missing, total, missing subjects)
    row_inds <- find_special_row_indices(formatted_data,
                                         missing_string = missing_string,
                                         total_row_label = total_row_label,
                                         missing_subjects_row_label = missing_subjects_row_label)

    # No processing is needed here just pass in the needed info
    get_data_order_bycount(numeric_data, ordering_cols,
                           treat_var, by, cols, result_order_var, target_var,
                           row_inds$missing, missing_sort_value,
                           row_inds$total, total_row_sort_value,
                           row_inds$missing_subjects, missing_subjects_sort_value,
                           break_ties = break_ties,
                           numeric_cutoff = numeric_cutoff,
                           numeric_cutoff_stat = numeric_cutoff_stat,
                           numeric_cutoff_column = numeric_cutoff_column)

  } else if (order_count_method == "byvarn") {

    assert_that(has_varn(target, as_name(target_var[[1]])), msg = "No VAR<N> variables present")

    varn_df <- get_varn_values(target, as_name(target_var[[1]]))

    if (!is.null(missing_count_list)) {
      varN_name <- names(varn_df)[2]
      varn_df[[1]] <- as.character(varn_df[[1]])
      if (is.null(missing_sort_value)) {
        varn_df <- varn_df %>%
          bind_rows(tibble(
            !!target_var[[1]] := names(missing_count_list),
            !!varN_name := seq_along(missing_count_list) + max(varn_df[, 2])
          ))
      } else {
        varn_df <- varn_df %>%
          bind_rows(tibble(
            !!target_var[[1]] := names(missing_count_list),
            !!varN_name := seq_along(missing_count_list) + missing_sort_value - 1
          ))
      }
    }

    # Find the explicit indices of special rows
    row_inds <- find_special_row_indices(formatted_data,
                                         total_row_label = total_row_label,
                                         missing_subjects_row_label = missing_subjects_row_label)

    get_data_order_byvarn(formatted_data, varn_df, as_name(target_var[[1]]),
                          formatted_col_index,
                          total_index = row_inds$total,
                          total_row_sort_value = total_row_sort_value,
                          missing_subjects_index = row_inds$missing_subjects,
                          missing_subjects_sort_value = missing_subjects_sort_value)

  # Here it is 'byfactor'
  } else {

    # If the target_var is a character, no order is needed
    if (is.character(quo_get_expr(target_var[[1]]))) return(NA)

    target_data <- target[, as_name(target_var[[1]])]

    # Pull levels from target variable
    target_levels <- levels(target_data[[1]])

    # If the levels are null, the target was not a factor. So turn it into a factor
    if (is.null(target_levels)) {

      # Change target variable into a factor
      target_fact <- as.factor(unlist(target_data))

      # Create data.frame with levels and index
      fact_df <- tibble(
        !!target_var[[1]] := unique(sort(target_fact)),
        factor_index := unclass(unique(sort(target_fact)))
      )

    } else {

      fact_df <- tibble(
        !!target_var[[1]] := target_levels,
        factor_index := seq_along(target_levels)
      )

    }

    if (!is.null(missing_count_list)) {
      if (is.null(missing_sort_value)) {
        fact_df <- fact_df %>%
          bind_rows(tibble(
            !!target_var[[1]] := names(missing_count_list),
            factor_index = seq_along(missing_count_list) + max(fact_df$factor_index)
          )) %>%
          distinct(!!target_var[[1]], .keep_all = TRUE)
      } else {
        fact_df <- fact_df %>%
          bind_rows(tibble(
            !!target_var[[1]] := names(missing_count_list),
            factor_index = seq_along(missing_count_list) + missing_sort_value
          )) %>%
          distinct(!!target_var[[1]], .keep_all = TRUE)
      }
    }

    # Find the explicit indices of special rows
    row_inds <- find_special_row_indices(formatted_data,
                                         total_row_label = total_row_label,
                                         missing_subjects_row_label = missing_subjects_row_label)

    # The logic is the same now for a byvarn so reuse that function
    get_data_order_byvarn(formatted_data, fact_df, as_name(target_var[[1]]),
                          formatted_col_index,
                          total_index = row_inds$total,
                          total_row_sort_value = total_row_sort_value,
                          missing_subjects_index = row_inds$missing_subjects,
                          missing_subjects_sort_value = missing_subjects_sort_value)
  }
}

#' Helper method for get_data_order_count
#' @noRd
get_data_order_bycount <- function(numeric_data, ordering_cols,
                       treat_var, by, cols, result_order_var, target_var,
                       missing_index = NULL, missing_sort_value = NULL,
                       total_index = NULL, total_row_sort_value = NULL,
                       missing_subjects_index = NULL,
                       missing_subjects_sort_value = NULL,
                       break_ties, numeric_cutoff, numeric_cutoff_stat,
                       numeric_cutoff_column, nested = FALSE) {

  if (nrow(numeric_data) == 0) return(numeric())

  # Make sure that if distinct_n is selected by set_result_order_var, that
  # there's a distinct variable in the numeric dataset
  if (as_name(result_order_var) %in% c("distinct_n", "distinct_pct")) {
    assert_that("distinct_n" %in% names(numeric_data),
                msg = paste0("`result_order_var` is set to `", as_name(result_order_var),
                             "` but no `distinct_by` is set. If you wish to sort by a distinct variable, ",
                             "you must use `set_distinct_by()` to choose a variable to calculate distinct counts."))
  }

  # Pull out each unique filter requirement. Each name for header_n is stored
  # on the LHS and its unique value in the function is on the RHS.
  # Examples
  # gear == 3
  # am == 1
  # These are stored in a list that is evaluated below
  filter_logic <- map2(c(treat_var, cols), ordering_cols, function(x, y) {
    expr(!!sym(as_name(x)) == !!as_name(y))
  })


  # pct and distinct_pct are now pre-calculated in numeric_data during summarization
  # No need to calculate them here anymore

  # What will become the column name of the tibble below. It nests the names of the ordering cols and
  # separates them with an underscore
  result_column <- paste(map_chr(ordering_cols, as_name), collapse = "_")

  # Filter and extract sort values directly without pivot_wider
  # After filtering to specific treatment/column, we just need the result_order_var values
  numeric_ordering_data <- numeric_data %>%
    {if (nested) . else filter_numeric(.,
                                      numeric_cutoff,
                                      numeric_cutoff_stat,
                                      numeric_cutoff_column,
                                      treat_var)} %>%
    filter(!!!filter_logic) %>%
    ungroup() %>%
    # Select only the sort value column and rename to expected output format
    select(!!result_order_var) %>%
    rename(!!result_column := !!result_order_var)

  if(!is.null(missing_index) && !is.null(missing_sort_value)) {
    numeric_ordering_data[missing_index,] <- seq_along(missing_index) - 1 + missing_sort_value
  }

  if(!is.null(total_index) && !is.null(total_row_sort_value)) {
    numeric_ordering_data[total_index,] <-  total_row_sort_value
  }

  if(!is.null(missing_subjects_index) && !is.null(missing_subjects_sort_value)) {
    numeric_ordering_data[missing_subjects_index,] <- missing_subjects_sort_value
  }

  # This is the numeric index that the numeric data is in. radix was chosen because
  # its the only method that gives indicies as far as I can tell
  # x are the values
  # ix are the indicies
  numeric_ordering_index <- sort(numeric_ordering_data[[ncol(numeric_ordering_data)]], method = "radix" ,
                                 index.return = TRUE, decreasing = TRUE, na.last = TRUE)


  # Order the vector based on the sort and return
  res <- numeric_ordering_index$x[order(numeric_ordering_index$ix)]

  if(isTRUE(break_ties == "asc") && length(target_var) == 2) {
    # dec_level is used to find the highest power of 10 for use in creating the
    # sorted deciaml. If there are 3 values dec_level will be 10, if there are
    # 25 it will be 100 and so forth.
    dec_level <- 10 ^ ceiling(log(nrow(numeric_ordering_data) + 1, base = 10))
    unlist(res + (1:nrow(numeric_ordering_data))/dec_level)
  } else if(isTRUE(break_ties == "desc") && length(target_var) == 2) {
    dec_level <- 10 ^ ceiling(log(nrow(numeric_ordering_data) + 1, base = 10))
    unlist(res + (nrow(numeric_ordering_data):1)/dec_level)
  } else {
    res
  }
}

get_data_order_byvarn <- function(formatted_data, by_varn_df, by_var, by_column_index,
                                  indentation = "",
                                  total_index = NULL, total_row_sort_value = NULL,
                                  missing_subjects_index = NULL,
                                  missing_subjects_sort_value = NULL) {

  # Pull out the by values in the formatted data.
  by_values <- unlist(formatted_data[, by_column_index])

  # Look up the VARN value for each row. Unmatched rows get a default
  # value that places them at the end.
  default_sort <- max(unlist(by_varn_df[, 2])) + 1
  varns <- map_dbl(by_values, function(a_by) {

    # Row containing the index and the value
    ind_row <- by_varn_df %>%
      filter(.data[[as_name(by_var)]] == a_by)

    if (nrow(ind_row) == 0) {
      default_sort
    } else {
      as.double(unlist(ind_row[, 2]))
    }
  })

  # Apply sort values to specific total and missing subjects rows by index
  if (!is.null(total_index) && !is.null(total_row_sort_value)) {
    varns[total_index] <- total_row_sort_value
  }

  if (!is.null(missing_subjects_index) && !is.null(missing_subjects_sort_value)) {
    varns[missing_subjects_index] <- missing_subjects_sort_value
  }

  # Remove the names so its just an unnamed numeric vector
  unname(varns)

}

#' Vectorized nested sorting
#'
#' This function computes sort orders for nested count layers without
#' using the deprecated do() pattern. It processes all outer values
#' and their inner values in a vectorized manner.
#'
#' @param formatted_data The formatted data frame to add ordering to
#' @param final_col The index for the final column (length(by))
#' @param numeric_data The numeric data with counts
#' @param indentation_length Length of indentation prefix
#' @param ordering_cols Columns to use for ordering
#' @param treat_var Treatment variable
#' @param by By variables (first element is outer nest variable)
#' @param cols Column variables
#' @param result_order_var Variable to use for ordering results
#' @param target_var Target variables
#' @param order_count_method Method for ordering ("bycount", "byvarn", or "byfactor")
#' @param target Original target data
#' @param all_outer Filtered numeric data for outer values only
#' @param filter_logic Filter expressions for ordering columns
#' @param indentation Indentation string
#' @param outer_inf Whether outer values should sort to Inf
#' @param break_ties How to break ties ("asc" or "desc")
#' @param numeric_cutoff Numeric cutoff value
#' @param numeric_cutoff_stat Statistic for numeric cutoff
#' @param numeric_cutoff_column Column for numeric cutoff
#' @param missing_subjects_row_label Label for missing subjects row
#' @param missing_subjects_sort_value Sort value for missing subjects
#' @param total_row_sort_value Sort value for total row
#' @return formatted_data with ord_layer columns added
#' @noRd
add_data_order_nested_vectorized <- function(formatted_data, final_col, numeric_data,
                                              indentation_length, ordering_cols,
                                              treat_var, by, cols, result_order_var,
                                              target_var, order_count_method, target,
                                              all_outer, filter_logic, indentation,
                                              outer_inf, break_ties, numeric_cutoff,
                                              numeric_cutoff_stat, numeric_cutoff_column,
                                              missing_subjects_row_label,
                                              missing_subjects_sort_value,
                                              total_row_sort_value = NULL) {

  row_label_vec <- vars_select(names(formatted_data), starts_with("row_label"))
  last_row_label <- tail(row_label_vec, 1)
  mrg_by <- paste0("row_label", seq_along(by))[-1]

  # Identify outer vs inner rows

  # Outer rows: where the last row_label equals row_label1 (no indentation)
  # Inner rows: where the last row_label starts with indentation
  # Handle NA values: NA becomes FALSE (not an outer row), which is safe since
  # NA rows shouldn't exist in well-formed data
  # Handle empty indentation: when indentation is "", all rows start with it,
  # so we need a different approach - use row_label1 == last_row_label
  if (nchar(indentation) == 0) {
    # No indentation - outer rows have the same value in row_label1 and last_row_label
    is_outer_row <- formatted_data$row_label1 == formatted_data[[last_row_label]]
    # Handle NAs
    is_outer_row[is.na(is_outer_row)] <- FALSE
  } else {
    is_outer_row <- str_starts(formatted_data[[last_row_label]], fixed(indentation)) %in% FALSE
  }

  # ========== OUTER LAYER SORTING ==========
  # Compute sort order for outer values (ord_layer_1)

  if (order_count_method[1] == "byvarn") {
    varn_df <- get_varn_values(target, as_name(by[[1]]))
    # For byvarn, we can compute order directly from varn values
    # Note: After replace_by_string_names(c(by, quo(summary_var))):
    #   - by[[1]] becomes row_label1 (contains NA for outer rows)
    #   - summary_var becomes row_label{length(by)+1} (contains actual outer values)
    # So we need to look at column final_col + 1, not final_col
    renamed_outer <- all_outer %>%
      replace_by_string_names(c(by, quo(summary_var)))
    # No total/missing subjects rows expected at the outer nesting level,
    # so no indices are passed
    all_outer$..outer_order <-
      get_data_order_byvarn(renamed_outer, varn_df, by[[1]], final_col + 1)

  } else if (order_count_method[1] == "bycount") {
    all_outer$..outer_order <- get_data_order_bycount(
      all_outer, ordering_cols, treat_var, vars(!!!head(by, -1)), cols,
      result_order_var, vars(!!by[[1]], !!target_var),
      break_ties = break_ties,
      numeric_cutoff = numeric_cutoff,
      numeric_cutoff_stat = numeric_cutoff_stat,
      numeric_cutoff_column = numeric_cutoff_column,
      nested = TRUE
    )
  } else {
    # byfactor - ord_layer_1 is already set by get_by_order() in the caller
    # We don't need to compute or overwrite it
    all_outer$..outer_order <- NULL
  }

  # For bycount and byvarn, we need to join the computed order back to formatted_data
  # For byfactor, ord_layer_1 is already set by get_by_order() - skip this section
  if (order_count_method[1] %in% c("bycount", "byvarn")) {
    # Note: In all_outer, the outer values are in 'summary_var', not in by[[1]] (which is NA)
    # We need to join on summary_var -> row_label1 in formatted_data
    #
    # For the simple case (no extra by vars), just match summary_var to row_label1.
    # For the complex case (extra by vars), we need to:
    # 1. Keep summary_var as is (don't rename it)
    # 2. Rename other by vars to match formatted_data's row_label2, row_label3, etc.
    # 3. Join on summary_var -> row_label1 plus the mrg_by columns

    if (length(mrg_by) == 0) {
      # Simple case: just match summary_var to row_label1
      outer_order_lookup <- all_outer %>%
        select(summary_var, ..outer_order) %>%
        distinct()

      formatted_data <- formatted_data %>%
        left_join(outer_order_lookup, by = c("row_label1" = "summary_var"))
    } else {
      # Complex case: need to include other by variables in the join
      # Use replace_by_string_names like the original code, then select row_label columns
      # This handles both symbol and text by variables correctly

      outer_order_lookup <- all_outer %>%
        replace_by_string_names(c(by, quo(summary_var))) %>%
        # Select row_label columns (excluding row_label1 which is NA from the outer var)
        # and the order column
        select(starts_with("row_label"), ..outer_order, -row_label1) %>%
        distinct()

      # The last row_label column contains the outer values (from summary_var)
      # We need to rename it to match row_label1 in formatted_data
      last_row_label <- paste0("row_label", length(by) + 1)
      if (last_row_label %in% names(outer_order_lookup)) {
        outer_order_lookup <- rename(outer_order_lookup, ..outer_val = !!sym(last_row_label))
      }

      # Join on the outer value and any additional by vars
      formatted_data <- formatted_data %>%
        left_join(outer_order_lookup,
                  by = c(setNames("..outer_val", "row_label1"), setNames(mrg_by, mrg_by)))
    }

    formatted_data$ord_layer_1 <- formatted_data$..outer_order
    formatted_data$..outer_order <- NULL
  }

  # ========== INNER LAYER SORTING ==========
  # For inner rows, compute sort order within each outer group

  # The outer value (Inf or -Inf for outer rows)
  outer_sort_value <- ifelse(is.null(outer_inf) || outer_inf, Inf, -Inf)

  # Initialize ord_layer_(final_col+1)
  ord_col_name <- paste0("ord_layer_", final_col + 1)
  formatted_data[[ord_col_name]] <- NA_real_

  # Outer rows get Inf/-Inf
  formatted_data[is_outer_row, ord_col_name] <- outer_sort_value

  # For inner rows, we need to compute sort order
  inner_formatted <- formatted_data[!is_outer_row, ]

  if (nrow(inner_formatted) > 0) {
    # Remove indentation prefix from inner row labels for matching
    inner_formatted$..inner_label_clean <- str_sub(inner_formatted[[last_row_label]],
                                                    indentation_length + 1)

    # Get the outer value for each inner row (from row_label1)
    # Also prepare numeric_data for inner sorting

    if (tail(order_count_method, 1) == "bycount") {
      # For bycount, compute order based on counts within each outer group
      # We need to process inner values grouped by outer value (and any other by vars)

      # Determine additional by variables (beyond the outer nest var)
      # by[1] is the outer nest var, by[2:length(by)] are other by vars (if any)
      other_by_vars <- if (length(by) > 1) by[2:length(by)] else list()

      # Prepare numeric data: filter to inner values and remove prefix
      inner_numeric <- numeric_data %>%
        filter(str_starts(summary_var, fixed(indentation))) %>%
        mutate(
          ..outer_val = !!by[[1]],
          summary_var = str_sub(summary_var, indentation_length + 1)
        )

      # Build grouping expressions: outer_val plus any additional by vars
      group_vars <- c(quo(..outer_val), other_by_vars)

      # Compute inner order for each outer group (and other by var combination)
      # VECTORIZED: Instead of group_modify, compute order values directly
      inner_by_for_order <- vars(!!by[[1]])

      # Build filter logic for ordering columns
      filter_logic <- map2(c(treat_var, cols), ordering_cols, function(x, y) {
        expr(!!sym(as_name(x)) == !!as_name(y))
      })

      # Get the result variable name
      result_var_name <- as_name(result_order_var)

      # Compute pct if needed (same logic as get_data_order_bycount)
      inner_numeric_prepared <- inner_numeric
      if (result_var_name == "pct") {
        inner_numeric_prepared <- inner_numeric_prepared %>%
          mutate(pct = replace_na(n / total, 0))
      } else if (result_var_name == "distinct_pct") {
        inner_numeric_prepared <- inner_numeric_prepared %>%
          mutate(distinct_pct = replace_na(distinct_n / distinct_total, 0))
      }

      # Filter to the ordering column and get the sort value
      inner_for_ordering <- inner_numeric_prepared %>%
        filter(!!!filter_logic) %>%
        select(..outer_val, summary_var, !!!other_by_vars, sort_val = !!result_order_var) %>%
        distinct()

      # Compute order value within each outer group (and other by vars)
      # The order value is the actual sort_val, with optional tie-breaking decimals
      inner_orders <- inner_for_ordering %>%
        group_by(..outer_val, !!!other_by_vars) %>%
        mutate(
          ..inner_order = {
            # Start with the actual sort values (this matches original behavior)
            order_vals <- sort_val

            # Handle break_ties if specified - add decimal for tie-breaking
            if (!is.null(break_ties) && break_ties == "asc" && length(target_var) == 2) {
              n_vals <- n()
              if (n_vals > 0) {
                dec_level <- 10^ceiling(log10(n_vals + 1))
                # Add decimal based on row position (alphabetical by summary_var)
                alpha_order <- order(summary_var)
                decimal_part <- match(seq_len(n_vals), alpha_order) / dec_level
                order_vals <- order_vals + decimal_part
              }
            } else if (!is.null(break_ties) && break_ties == "desc" && length(target_var) == 2) {
              n_vals <- n()
              if (n_vals > 0) {
                dec_level <- 10^ceiling(log10(n_vals + 1))
                # Add decimal based on reverse row position
                alpha_order <- order(summary_var, decreasing = TRUE)
                decimal_part <- match(seq_len(n_vals), alpha_order) / dec_level
                order_vals <- order_vals + decimal_part
              }
            }
            order_vals
          }
        ) %>%
        ungroup() %>%
        select(..outer_val, summary_var, !!!other_by_vars, ..inner_order)

      # Handle missing subjects
      if (!is.null(missing_subjects_row_label) && !is.null(missing_subjects_sort_value)) {
        inner_orders <- inner_orders %>%
          mutate(..inner_order = ifelse(summary_var == missing_subjects_row_label,
                                        missing_subjects_sort_value, ..inner_order))
      }

      # Build the join specification
      # row_label1 = ..outer_val, row_label2 = by[[2]], ..., ..inner_label_clean = summary_var
      # other_by_vars maps to row_label2, row_label3, etc.
      join_by <- c("row_label1" = "..outer_val", "..inner_label_clean" = "summary_var")
      if (length(other_by_vars) > 0) {
        for (..idx in seq_along(other_by_vars)) {
          join_by <- c(join_by, setNames(as_name(other_by_vars[[..idx]]),
                                          paste0("row_label", ..idx + 1)))
        }
      }

      # Select columns for join
      select_cols <- c("..outer_val", "summary_var", "..inner_order",
                       map_chr(other_by_vars, as_name))

      # Join back to inner_formatted
      inner_formatted <- inner_formatted %>%
        left_join(
          inner_orders %>% select(all_of(select_cols)),
          by = join_by
        )

      inner_formatted[[ord_col_name]] <- inner_formatted$..inner_order

    } else if (tail(order_count_method, 1) == "byvarn") {
      varn_df <- get_varn_values(target, target_var[[1]])
      varn_col <- names(varn_df)[2]  # The column with numeric order values

      # For byvarn, order based on varn values
      inner_formatted <- inner_formatted %>%
        left_join(varn_df, by = setNames(names(varn_df)[1], "..inner_label_clean"))

      inner_formatted[[ord_col_name]] <- inner_formatted[[varn_col]]
      # Remove the extra column added by the join
      inner_formatted[[varn_col]] <- NULL

      # Handle missing subjects
      if (!is.null(missing_subjects_row_label) && !is.null(missing_subjects_sort_value)) {
        missing_mask <- inner_formatted$..inner_label_clean == missing_subjects_row_label
        inner_formatted[missing_mask, ord_col_name] <- missing_subjects_sort_value
      }

    } else {
      # byfactor - use sequential order within each group
      # Use as.numeric to ensure double type for consistency with missing_subjects_sort_value
      inner_formatted <- inner_formatted %>%
        group_by(row_label1) %>%
        mutate(!!ord_col_name := as.numeric(row_number())) %>%
        ungroup()

      # Handle missing subjects
      if (!is.null(missing_subjects_row_label) && !is.null(missing_subjects_sort_value)) {
        missing_mask <- inner_formatted$..inner_label_clean == missing_subjects_row_label
        inner_formatted[missing_mask, ord_col_name] <- missing_subjects_sort_value
      }
    }

    # Clean up temp columns
    inner_formatted$..inner_label_clean <- NULL
    inner_formatted$..inner_order <- NULL

    # Put inner rows back into formatted_data
    formatted_data[!is_outer_row, ] <- inner_formatted
  }

  # Restore the grouped row order: outer row followed by its inner rows for each group
  # This matches the original do() behavior which processed each group separately
  # Recompute is_outer_row since formatted_data may have been modified
  # Use desc() so outer rows (TRUE) come before inner rows (FALSE)
  if (nchar(indentation) == 0) {
    # No indentation - outer rows have the same value in row_label1 and last_row_label
    is_outer_final <- formatted_data$row_label1 == formatted_data[[last_row_label]]
    is_outer_final[is.na(is_outer_final)] <- FALSE
  } else {
    is_outer_final <- str_starts(formatted_data[[last_row_label]], fixed(indentation)) %in% FALSE
  }
  formatted_data$..is_outer <- is_outer_final
  formatted_data <- formatted_data %>%
    arrange(row_label1, desc(..is_outer)) %>%
    select(-..is_outer)

  formatted_data
}

