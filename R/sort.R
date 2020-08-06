

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
  evalq({

    # Set all defaults for ordering
    if (is.null(result_order_var)) result_order_var <- quo(n)
    # A lot of weird stripping of the object is done here to make sure its the
    # right class
    if (is.null(ordering_cols)) ordering_cols <- quos(!!unname(unlist(as.character(
      head(unique(pop_data[, as_name(pop_treat_var)]), 1)
      ))))
    if (is.null(order_count_method)) order_count_method <- "byfactor"

    # If it is a nested count_layer
    if (length(target_var) == 2) {

      # Number of sorting columns needed, number of bys plus one for the target_var
      formatted_col_index <- length(by) + 1

      walk2(by, seq_along(by), function(a_by, by_i) {
        # If a_by is a character, skip and go to the next, it doesn't have any sorting information.
        if (!is.name(quo_get_expr(a_by))) return()
        formatted_data[, paste0("ord_layer_", by_i)] <<- get_by_order(formatted_data, target, by_i, a_by)
      })

      # This sorts the sub-pieces of the nested layer
      formatted_data[, paste0("ord_layer_", formatted_col_index)] <- nest_sort_index

      # Used to remove the prefix
      indentation_length <- ifelse(!is.null(indentation), length(indentation), 2)

      # Only the outer columns
      filter_logic <- map2(c(treat_var, cols), ordering_cols, function(x, y) {
        expr(!!sym(as_name(x)) == !!as_name(y))
      })
      all_outer <- numeric_data %>%
        filter(!!!filter_logic) %>%
        group_by(!!target_var[[1]]) %>%
        do(extract(., 1, ))

      # Add the ordering of the pieces in the layer
      formatted_data <- formatted_data %>%
        group_by(.data[[paste0("ord_layer_", formatted_col_index)]]) %>%
        do(add_data_order_nested(., formatted_col_index, numeric_data,
                                 indentation_length = indentation_length,
                                 ordering_cols = ordering_cols,
                                 treat_var = treat_var, by = by, cols = cols,
                                 result_order_var = result_order_var,
                                 target_var = target_var,
                                 order_count_method = order_count_method,
                                 target = target, all_outer = all_outer,
                                 filter_logic = filter_logic))

      # If it isn't a nested count layer
    } else {


      # Number of sorting columns needed, number of bys plus one for the target_var
      formatted_col_index <- length(by) + 1

      # This adds a column for each by variable and the target variable.
      walk2(by, seq_along(by), function(a_by, by_i) {
        # If a_by is a character, skip and go to the next, it doesn't have any sorting information.
        if (!is.name(quo_get_expr(a_by))) return()
        formatted_data[, paste0("ord_layer_", by_i)] <<- get_by_order(formatted_data, target, by_i, a_by)
      })

      formatted_data[, paste0("ord_layer_", formatted_col_index)] <- get_data_order(current_env(), formatted_col_index)
    }

  }, envir = x)
}

add_order_columns.desc_layer <- function(x) {

  evalq({

    # This adds a column for each by variable and the target variable.
    walk2(by, seq_along(by), function(a_by, by_i) {
      # If a_by is a character, skip and go to the next, it doesn't have any sorting information
      if (!is.name(quo_get_expr(a_by))) return()
      formatted_data[, paste0("ord_layer_", by_i)] <<- get_by_order(formatted_data, target, by_i, a_by)
    })

    # Number of sorting columns needed, number of bys plus one for the target_var
    formatted_col_index <- length(by) + 1

    formatted_data[, paste0("ord_layer_", formatted_col_index)] <- seq(nrow(formatted_data))


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
  levels_i <- levels(target[, as_name(var)])

  if (has_varn(target, as_name(var))) {

    varn_df <- get_varn_values(target, as_name(var))

    get_data_order_byvarn(formatted_data, varn_df, as_name(var), i)

  # If the variable isn't a factor, default to alphabetical
  } else if (is.null(levels_i)) {

    # Unlist to get out of tibble, turn into factor which will order it alphabeticlly
    # unclass it to get it as a number
    unclass(as.factor(unlist(formatted_data[, i])))

    # If it is a factor, just use levels to sort
  } else {

    # Unlist to pull it out of the tibble, order it based on the orders in the target
    # data.frame, unclass it to pull out the index
    unclass(ordered(unlist(formatted_data[, i]), levels_i))
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

    # Swtich for the sorting method
    if (order_count_method == "bycount") {

      # No processing is needed here just pass in the needed info
      get_data_order_bycount(numeric_data, ordering_cols,
                             treat_var, by, cols, result_order_var, target_var)

    } else if (order_count_method == "byvarn") {

      assert_that(has_varn(target, as_name(target_var[[1]])), msg = "No VAR<N> variables present")

      varn_df <- get_varn_values(target, as_name(target_var[[1]]))

      get_data_order_byvarn(formatted_data, varn_df, as_name(target_var[[1]]),
                            formatted_col_index)


      # Here it is 'byfactor'
    } else {

      # If the target_var is a character, no order is needed
      if(is.character(quo_get_expr(target_var[[1]]))) return(NA)

      target_data <- target[, as_name(target_var[[1]])]

      #Pull levels from target variable
      target_levels <- levels(target_data)

      # If the levels are null, the target was not a factor. So turn it into a factor
      if (is.null(target_levels)) {

        # Change target variable into a factor
        target_fact <- as.factor(unlist(target_data))

        # Create data.frame with levels and index
        fact_df <- tibble(
          !!target_var[[1]] := unique(sort(target_fact)),
          factor_index := unclass(unique(sort(target_fact)))
        )

        # The logic is the same now for a byvarn so reuse that function
        get_data_order_byvarn(formatted_data, fact_df, as_name(target_var[[1]]), formatted_col_index)

      } else {

        fact_df <- tibble(
          !!target_var[[1]] := unique(sort(target_data)),
          factor_index := unclass(unique(sort(target_data)))
        )

        get_data_order_byvarn(formatted_data, fact_df, as_name(target_var[[1]]), formatted_col_index)

      }
    }
  }, envir = x)
}

#' Helper method for get_data_order
#' @noRd
get_data_order_bycount <- function(numeric_data, ordering_cols,
                       treat_var, by, cols, result_order_var, target_var) {

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
    filter(!!!filter_logic) %>%

    # I'm like 98% sure this logic works out.
    pivot_wider(id_cols = c(match_exact(by), "summary_var", match_exact(head(target_var, -1))),
                names_from = c(!!treat_var, !!!cols), values_from = !!result_order_var) %>%
    ungroup() %>%
    select(as.symbol(result_column))

  # This is the numeric index that the numeric data is in. radix was chosen because
  # its the only method that gives indicies as far as I can tell
  # x are the values
  # ix are the indicies
  numeric_ordering_index <- sort(numeric_ordering_data[[ncol(numeric_ordering_data)]], method = "radix" ,
                                 index.return = TRUE, decreasing = TRUE, na.last = TRUE)


  # Order the vector based on the sort and return
  numeric_ordering_index$x[order(numeric_ordering_index$ix)]
}

get_data_order_byvarn <- function(formatted_data, by_varn_df, by_var, by_column_index) {

  # Pull out the by values in the formatted data.
  by_values <- unlist(formatted_data[, by_column_index])

  map_dbl(by_values, function(a_by) {

    # Row containing the index and the value
    ind_row <- by_varn_df %>%
      # Converting by_var to a symbol didn't work here for some reason but this
      # works just as well.
      filter(.data[[by_var]] == a_by)

    # If the row is length zero it is a total row. Just add one so it appears on the bottom
    if (nrow(ind_row) == 0) {
      max(by_varn_df[,2]) + 1
    } else {
      # Index is always in the second row
      as.double(unlist(ind_row[, 2]))
    }
  })
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
#'
#' @noRd
add_data_order_nested <- function(group_data, final_col, numeric_data, ...) {

  # Pull out dots
  dots <- list(...)

  # Here are the names of the formatted data row labels. We usually only work with the last
  row_label_vec <- vars_select(names(group_data), starts_with("row_label"))

  ##### Outer nest values #####
  # The value of the outer label
  outer_value <- group_data[1, tail(row_label_vec, 1)][[1]]

  if(dots$order_count_method[1] == "byvarn") {
    varn_df <- get_varn_values(dots$target, as_name(dots$target_var[[1]]))

    dots$all_outer$..index <- group_data[1,] %>%
      get_data_order_byvarn(varn_df, dots$target_var[[1]], final_col)

    group_data[, paste0("ord_layer_", final_col)] <- dots$all_outer %>%
      filter(summary_var == outer_value) %>%
      ungroup() %>%
      select(..index)

  } else if(dots$order_count_method[1] == "bycount") {

    dots$all_outer$..index <- dots$all_outer %>%
      get_data_order_bycount(dots$ordering_cols, dots$treat_var, dots$by, dots$cols,
                             dots$result_order_var, dots$target_var)

    group_data[, paste0("ord_layer_", final_col)] <- dots$all_outer %>%
      filter(summary_var == outer_value) %>%
      ungroup() %>%
      select(..index)
  }

  ##### Inner nest values #####
  filtered_numeric_data <- numeric_data %>%
    # Only include the parts of the numeric data that is in the current label
    filter(numeric_data$summary_var %in% unlist(group_data[-1, row_label_vec[length(row_label_vec)]])) %>%
    # Remove nesting prefix to prepare numeric data.
    mutate(summary_var := str_sub(summary_var, dots$indentation_length))


  #Same idea here, remove prefix
  filtered_group_data <- group_data[-1, ] %>%
    mutate(!!row_label_vec[length(row_label_vec)] := str_sub(.data[[row_label_vec[length(row_label_vec)]]],
                                                             dots$indentation_length))

  # The first row is always the first thing in the order so make it Inf
  group_data[1, paste0("ord_layer_", final_col + 1)] <- Inf

  if(tail(dots$order_count_method, 1) == "bycount") {
    group_data[-1 , paste0("ord_layer_", final_col + 1)] <- get_data_order_bycount(filtered_numeric_data,
                                                                                   dots$ordering_cols,
                                                                                   dots$treat_var,
                                                                                   dots$by,
                                                                                   dots$cols,
                                                                                   dots$result_order_var,
                                                                                   dots$target_var)
  } else if(tail(dots$order_count_method, 1) == "byvarn") {

    varn_df <- get_varn_values(dots$target, dots$target_var[[2]])

    group_data[-1, paste0("ord_layer_", final_col + 1)] <- get_data_order_byvarn(filtered_group_data,
                                                                                 varn_df,
                                                                                 dots$target_var[[2]],
                                                                                 final_col)

  } else {

    group_data[-1, paste0("ord_layer_", final_col + 1)] <- 1:nrow(group_data[-1,])

  }

  group_data

}

