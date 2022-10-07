

#' Set the sorting of rows/columns in a `tplyr_table` and `tplyr_layer` object
#'
#' Named this way to prevent conflicts
#'
#' @param x A \code{data.frame} or \code{tplyr_layer} object
#' @param ... Parameters passed to disptach that are used in sorting
#'
#'
#' @return An ordered data.frame if a data.frame was passed. Or nothing in the
#'   case of a tplyr_layer. These are adjusted silently and will be ordered
#'   when the table is output.
#'
#' @examples
#' library(dplyr)
#'
#'
#' #### Sorting output of a table ####
#' t <- tplyr_table(mtcars, gear) %>%
#'   add_total_group() %>%
#'   add_layer(
#'     group_count(cyl)
#'   )
#' build(t) %>%
#'   tplyr_order(4, 5, Total)
#' # This orders the column as 4, 5, Total, 3
#' # The 3 appears at the end because it wasn't passed.
#'
#' @noRd
tplyr_order <- function(x, ...) {
  UseMethod("tplyr_order")
}

#' Method for data.frame
#'
#' I added this as S3 for extensibility in case we want to add one for the table
#' for any reason.
#'
#' @noRd
tplyr_order.data.frame <- function(x, ...) {

  dots <- enquos(...)

  assert_inherits_class(dots, "quosures")

  # Make sure the table is well formed
  assert_that(length(vars_select(x,
                                 starts_with("row_label") | starts_with("var1_") | starts_with("ord_"))) == names(x),
              msg = "All of the column headers must start with row_label, var1_, or ord_.
              Is this output from a tplyr_table object?")

  dots_chr <- match_exact(dots)

  # Variables passed prefixed with var1_
  dots_chr <- paste0("var1_", dots_chr)

  # Names of columns that have data.
  var1_names <- names(x) %>%
    vars_select(starts_with("var1_"))

  # The number of row_labels/leading columns
  leading_col_num <- length(names(x)) - length(var1_names)

  # The data.frame with row_labels and ords
  leading_df <- x[,1:leading_col_num]

  # The trailing columns that aren't row labels
  x <- x[, (leading_col_num + 1):ncol(x)]

  # The order that the columsn were passed
  dots_order <- map_dbl(dots_chr, function(y) {
    which(y == names(x))
  })

  # Data.frame in order that its passed and all other rows
  ordered_var_df <- x[,dots_order]
  trailing_df <- x[, -dots_order]

  # First the row_labels, then the order that was passed, then what wasn't passed
  bind_cols(leading_df, ordered_var_df, trailing_df)
}

#### Helpers ####

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

  evalq({

    if(nrow(formatted_data) == 0) return(formatted_data)
    if(!exists("break_ties")) break_ties <- NULL

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

      walk2(by, seq_along(by), function(a_by, by_i) {
        # If a_by is a character, add the index itself
        if (!is.name(quo_get_expr(a_by))) formatted_data[, paste0("ord_layer_", by_i)] <<- by_i
        # Otherwise determine data order
        else formatted_data[, paste0("ord_layer_", by_i)] <<- get_by_order(formatted_data, target, by_i, a_by)
      })

      # Used to remove the prefix. String is encoded to get controlled characters i.e. \t
      indentation_length <- ifelse(!is.null(indentation), nchar(encodeString(indentation)), 3)

      # Only the outer columns
      filter_logic <- map2(c(treat_var, cols), ordering_cols, function(x, y) {
        expr(!!sym(as_name(x)) == !!as_name(y))
      })

      # Get the number of unique outer values, that is the number of rows to pull out.
      # If its text, it is just 1 to pull out
      outer_number <- ifelse(quo_is_symbol(by[[1]]),
                             # Use built_target here to take the 'where' logic into account
                             length(unlist(unique(built_target[, as_name(by[[1]])]))),
                             1)

      all_outer <- numeric_data %>%
        filter(!!!filter_logic) %>%
        extract(1:min(nrow(.), outer_number), )

      # Add the ordering of the pieces in the layer
      formatted_data <- formatted_data %>%
        group_by(.data[[paste0("ord_layer_", formatted_col_index - 1)]]) %>%
        do(add_data_order_nested(., formatted_col_index - 1, numeric_data,
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
                                 numeric_cutoff_column = numeric_cutoff_column)) %>%
        ungroup()

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
      walk2(by, seq_along(by), function(a_by, by_i) {
        # If a_by is a character, add the index itself
        if (!is.name(quo_get_expr(a_by))) formatted_data[, paste0("ord_layer_", by_i)] <<- by_i
        # Otherwise determine data order
        else formatted_data[, paste0("ord_layer_", by_i)] <<- get_by_order(formatted_data, target, by_i, a_by)
      })

      #This is used to permute any missing ordering values for total rows
      if(include_total_row) {
        ord_cols <- vars_select(names(formatted_data), starts_with("ord"))
        na_ordered_row <- unique(map_int(ord_cols, function(x) {
          isna_ <- which(is.na(formatted_data[, x]) & formatted_data[, formatted_col_index] == total_row_label)
          if(length(isna_) == 0) return(NA)
          else return(isna_)
        }))
      }

      formatted_data[, paste0("ord_layer_", formatted_col_index)] <- get_data_order(current_env(), formatted_col_index)

      # If there is a total row that is missing some ord values, they should fall
      # back from the last one.
      if(exists("na_ordered_row") && length(na_ordered_row) == 0) na_ordered_row <- NA
      if(include_total_row && !is.na(na_ordered_row)) {
        #This is the ord columns that are NA
        na_ord_cols <- which(is.na(formatted_data[na_ordered_row, ord_cols]))
        # Change those columns to the last value in the formatted table.
        formatted_data[na_ordered_row, ord_cols[na_ord_cols]] <-
          formatted_data[na_ordered_row, ncol(formatted_data)][[1]]
      }
    }

    rm(formatted_col_index)

  }, envir = x)
}

add_order_columns.desc_layer <- function(x) {

  evalq({

    # This adds a column for each by variable and the target variable.
    walk2(by, seq_along(by), function(a_by, by_i) {
      # If a_by is a character, add the index itself
      if (!is.name(quo_get_expr(a_by))) formatted_data[, paste0("ord_layer_", by_i)] <<- by_i
      # Otherwise determine data order
      else formatted_data[, paste0("ord_layer_", by_i)] <<- get_by_order(formatted_data, target, by_i, a_by)
    })

    # Number of sorting columns needed, number of bys plus one for the target_var
    formatted_col_index <- length(by) + 1

    if (formatted_col_index > 1) {
      formatted_data <- formatted_data %>%
        group_by(!! sym(paste0("ord_layer_", formatted_col_index - 1))) %>%
        mutate(!!sym(paste0("ord_layer_", formatted_col_index)) := row_number())
    } else {
      formatted_data[, paste0("ord_layer_", formatted_col_index)] <- seq(nrow(formatted_data))
    }

    rm(formatted_col_index)

  }, envir = x)
}

add_order_columns.shift_layer <- function(x) {
  evalq({

    # This adds a column for each by variable and the target variable.
    walk2(by, seq_along(by), function(a_by, by_i) {
      # If a_by is a character, add the index itself
      if (!is.name(quo_get_expr(a_by))) formatted_data[, paste0("ord_layer_", by_i)] <<- by_i
      # Otherwise determine data order
      else formatted_data[, paste0("ord_layer_", by_i)] <<- get_by_order(formatted_data, target, by_i, a_by)
    })

    # Number of sorting columns needed, number of bys plus one for the target_var
    formatted_col_index <- length(by) + 1

    #### The Factor data order method.
    target_data <- target[, as_name(target_var$row)]

    #Pull levels from target variable
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


      # The logic is the same now for a byvarn so reuse that function
      formatted_data[, paste0("ord_layer_", formatted_col_index)] <-
        get_data_order_byvarn(formatted_data, fact_df, as_name(target_var$row),
                              formatted_col_index, total_row_sort_value = total_row_sort_value)

    rm(formatted_col_index)

  }, envir = x)
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

#' Add the order for the target variable
#'
#' I'm condisering refactoring this and breaking out the logic for 'byrow', 'byvarn',
#' and 'byfactor' into their own function
#'
#' @param x The tplyr layer environment
#' @param formatted_col_index the column index of the target variable data.
#'
#' @return Returns the index the variables should be ordered in, in the cases of
#'   ordering by a <VAR>N variable and ordering by factors. Returns the numeric
#'   value in the case of ordering by column values.
#'
#' @noRd
get_data_order <- function(x, formatted_col_index) {

  evalq({

    # Switch for the sorting method
    if (order_count_method == "bycount") {

      # Get the index of the row with the missing and total names
      label_row_ind <- which(names(formatted_data) %in%
                               tail(vars_select(names(formatted_data),
                                                starts_with("row_label")), 1))

      if(!is.null(missing_string)) missing_index <- which(unlist(formatted_data[, label_row_ind]) %in% missing_string)
      if(!is.null(total_row_label)) total_index <- which(unlist(formatted_data[, label_row_ind]) %in% total_row_label)

      # No processing is needed here just pass in the needed info
      get_data_order_bycount(numeric_data, ordering_cols,
                             treat_var, by, cols, result_order_var, target_var,
                             missing_index, missing_sort_value,
                             total_index, total_row_sort_value,
                             break_ties = break_ties,
                             numeric_cutoff = numeric_cutoff,
                             numeric_cutoff_stat = numeric_cutoff_stat,
                             numeric_cutoff_column = numeric_cutoff_column)

    } else if (order_count_method == "byvarn") {

      assert_that(has_varn(target, as_name(target_var[[1]])), msg = "No VAR<N> variables present")

      varn_df <- get_varn_values(target, as_name(target_var[[1]]))

      if(!is.null(missing_count_list)) {
        if(is.null(missing_sort_value)) {
          varN_name <- names(varn_df)[2]
          varn_df[,1] <- as.character(varn_df[,1])
          varn_df <- varn_df %>%
            bind_rows(tibble(
              !!target_var[[1]] := names(missing_count_list),
              !!varN_name := seq_along(missing_count_list) + max(varn_df[,2])
            ))
        } else {
          varN_name <- names(varn_df)[2]
          varn_df[,1] <- as.character(varn_df[,1])
          varn_df <- varn_df %>%
            bind_rows(tibble(
              !!target_var[[1]] := names(missing_count_list),
              !!varN_name := seq_along(missing_count_list) + missing_sort_value - 1
            ))
        }

      }

      get_data_order_byvarn(formatted_data, varn_df, as_name(target_var[[1]]),
                            formatted_col_index, total_row_sort_value = total_row_sort_value)


      # Here it is 'byfactor'
    } else {

      # If the target_var is a character, no order is needed
      if(is.character(quo_get_expr(target_var[[1]]))) return(NA)

      target_data <- target[, as_name(target_var[[1]])]

      #Pull levels from target variable
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

      if(!is.null(missing_count_list)) {
        if(is.null(missing_sort_value)) {
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

      # The logic is the same now for a byvarn so reuse that function
      get_data_order_byvarn(formatted_data, fact_df, as_name(target_var[[1]]),
                            formatted_col_index, total_row_sort_value = total_row_sort_value)
    }
  }, envir = x)
}

#' Helper method for get_data_order
#' @noRd
get_data_order_bycount <- function(numeric_data, ordering_cols,
                       treat_var, by, cols, result_order_var, target_var,
                       missing_index = NULL, missing_sort_value = NULL,
                       total_index = NULL, total_row_sort_value = NULL,
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

  # Logic for pcts
  if (as_name(result_order_var) == "pct") {

    pct_value <- numeric_data[, "n"] / numeric_data[, "total"]

    # This isn't in an evalq so the modifications here won't do anything to the layer
    numeric_data[, "pct"] <- replace_na(pct_value[[1]], 0)

  } else if (as_name(result_order_var) == "distinct_pct") {

    pct_value <- numeric_data[, "distinct_n"] / numeric_data[, "distinct_total"]

    numeric_data[, "distinct_pct"] <-  replace_na(pct_value, 0)
  }

  # What will become the column name of the tibble below. It nests the names of the ordering cols and
  # separates them with an underscore
  result_column <- paste(map_chr(ordering_cols, as_name), collapse = "_")

  ## WARNING: This has to be the same logic as the pivot in the count ordering or else it won't work
  numeric_ordering_data <- numeric_data %>%
    {if (nested) . else filter_numeric(.,
                                      numeric_cutoff,
                                      numeric_cutoff_stat,
                                      numeric_cutoff_column,
                                      treat_var)} %>%
    filter(!!!filter_logic) %>%

    # Sometimes row numbers are needed for nested counts if a value in the first
    # target variable is the same as a variable in the second
    mutate(row = row_number()) %>%

    # I'm like 98% sure this logic works out.
    pivot_wider(id_cols = c(match_exact(by), "summary_var", row),
                names_from = c(!!treat_var, !!!cols), values_from = !!result_order_var) %>%

    # Remove the placeholder row
    mutate(row = NULL) %>%

    ungroup() %>%
    select(as.symbol(result_column))

  if(!is.null(missing_index) && !is.null(missing_sort_value)) {
    numeric_ordering_data[missing_index,] <- seq_along(missing_index) - 1 + missing_sort_value
  }

  if(!is.null(total_index) && !is.null(total_row_sort_value)) {
    numeric_ordering_data[total_index,] <-  total_row_sort_value
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
                                  indentation = "", total_row_sort_value = NULL) {

  # Pull out the by values in the formatted data.
  by_values <- unlist(formatted_data[, by_column_index])

  varns <- map_dbl(by_values, function(a_by) {

    # Row containing the index and the value
    ind_row <- by_varn_df %>%
      # Converting by_var to a symbol didn't work here for some reason but this
      # works just as well.
      filter(.data[[as_name(by_var)]] == a_by)

    # If the row is length zero it is a total row. Just add one so it appears on the bottom or use the sort_value
    if (nrow(ind_row) == 0) {
      # Flag to determine where total row is positioned
      if(!is.null(total_row_sort_value)) {
        total_row_sort_value
      } else {
        max(by_varn_df[,2]) + 1
      }
    } else {
      # Index is always in the second row
      as.double(unlist(ind_row[, 2]))
    }
  })

  # Remove the names so its just an unnamed numeric vecetor
  unname(varns)

}

#' Add an ordering column for a nested count layer
#'
#' @param group_data A formatted_data object that has been grouped on the first
#'   target variable
#' @param final_col The last column of the formatted_data object. I.e where the
#'   sort happened
#' @param numeric_data The numeric_data object that is used to extract the order
#'   count
#' @param indentation_length The length of the prefix that was used in the
#'   nesting indentation.
#' @param ordering_cols The ordering_cols object used to select the columns used
#'   in ordering.
#' @param treat_var The treat_var variable binding
#' @param by The by variable binding
#' @param cols The cols variable binding
#' @param result_order_var The result being used to order the numeric data.
#' @param ... Additional arguments that are passed, most are above
#'
#' @noRd
add_data_order_nested <- function(group_data, final_col, numeric_data, ...) {

  # Pull out dots
  list2env(list(...), envir = environment())

  # Here are the names of the formatted data row labels. We usually only work with the last
  row_label_vec <- vars_select(names(group_data), starts_with("row_label"))

  ##### Outer nest values #####
  # The value of the outer label
  outer_value <- group_data[1, tail(row_label_vec, 1)][[1]]

  if(order_count_method[1] == "byvarn") {
    varn_df <- get_varn_values(target, as_name(by[[1]]))

    all_outer$..index <- group_data[1,] %>%
      get_data_order_byvarn(varn_df, by[[1]], final_col, total_row_sort_value = total_row_sort_value)

    group_data[, paste0("ord_layer_", final_col)] <- all_outer %>%
      filter(summary_var == outer_value) %>%
      ungroup() %>%
      select(..index)

  } else if(order_count_method[1] == "bycount") {

    all_outer$..index <- all_outer %>%
      get_data_order_bycount(ordering_cols, treat_var, vars(!!!head(by, -1)), cols,
                             result_order_var, vars(!!by[[1]], !!target_var),
                             break_ties = break_ties,
                             numeric_cutoff = numeric_cutoff,
                             numeric_cutoff_stat = numeric_cutoff_stat,
                             numeric_cutoff_column = numeric_cutoff_column,
                             nested = TRUE)

    group_data[, paste0("ord_layer_", final_col)] <- all_outer %>%
      filter(summary_var == outer_value) %>%
      ungroup() %>%
      select(..index)
  }

  present_vars <- unlist(group_data[-1, row_label_vec[length(row_label_vec)]])
  ##### Inner nest values #####
  filtered_numeric_data <- numeric_data %>%
    # Only include the parts of the numeric data that is in the current label
    filter(numeric_data$summary_var %in% present_vars, !is.na(!!by[[1]])) %>%
    # Remove nesting prefix to prepare numeric data.
    mutate(summary_var := str_sub(summary_var, indentation_length))


  #Same idea here, remove prefix
  filtered_group_data <- group_data[-1, ] %>%
    mutate(!!row_label_vec[length(row_label_vec)] := str_sub(.data[[row_label_vec[length(row_label_vec)]]],
                                                             indentation_length + 1))

  # The first row is always the first thing in the order so make it Inf
  group_data[1, paste0("ord_layer_", final_col + 1)] <- ifelse((is.null(outer_inf) || outer_inf), Inf, -Inf)

  if(tail(order_count_method, 1) == "bycount") {
    if (nrow(group_data) > 1) {
      group_data[-1 , paste0("ord_layer_", final_col + 1)] <- get_data_order_bycount(filtered_numeric_data,
                                                                                     ordering_cols,
                                                                                     treat_var,
                                                                                     head(by, -1),
                                                                                     cols,
                                                                                     result_order_var,
                                                                                     target_var,
                                                                                     break_ties = break_ties,
                                                                                     numeric_cutoff = numeric_cutoff,
                                                                                     numeric_cutoff_stat = numeric_cutoff_stat,
                                                                                     numeric_cutoff_column = numeric_cutoff_column,
                                                                                     nested = TRUE)
    }
  } else if(tail(order_count_method, 1) == "byvarn") {

    varn_df <- get_varn_values(target, target_var[[1]])



    group_data[-1, paste0("ord_layer_", final_col + 1)] <- get_data_order_byvarn(filtered_group_data,
                                                                                 varn_df,
                                                                                 target_var[[1]],
                                                                                 length(by) + 1,
                                                                                 indentation,
                                                                                 total_row_sort_value = total_row_sort_value)

  } else {

    group_row_count <- nrow(group_data[-1,])
    # Logic for group_row_count is when numeric_where values cause unexpected results
    group_data[-1, paste0("ord_layer_", final_col + 1)] <- 1:ifelse(group_row_count == 0, 1, group_row_count)

  }

  group_data

}

