

#' Set the sorting of rows/columns in a `tplyr_table` and `tplyr_layer` object
#'
#' Named this way to prevent conflicts
#'
#' @param x A \code{data.frame} or \code{tplyr_layer} object
#' @param ... Parameters passed to disptach that are used in sorting
#'
#' @section Sorting a Table:
#' You can pass the output of a build to reorder the columns. The function will
#' order the columns in the order the elipsis was passed. If all of the columns
#' aren't used, the columns that weren't selected will be moved to the end of
#' the data.frame after the columns that were passed.
#'
#' When a table is built, the output has several ordering(ord_) columns that are
#' appended. The first represents the layer index. The index is determined by
#' the order the layer was added to the table. The following are the indicies
#' for the by variables and the target variable. The by variables are ordered
#' based on:
#' 1) A <VAR>N variable (i.e. VISIT -> VISITN, TRT -> TRTN) if one is present.
#'
#' 2) If no <VAR>N variable is present, it is ordered based on the factor
#' present in the target dataset.
#'
#' 3) If the variable is not a factor in the
#' target dataset, it is coersed to one and ordered alphabetically.
#'
#' The target variable is ordered depending on the type of layer. See more below.
#'
#' @section Ordering a Count Layer:
#' There are many ways to order a count layer depending on the preferences of
#' the table programmer. \code{Tplyr} supports sorting by a descending amount in
#' a column in the table, sorting by a <VAR>N variable, and sorting by a custom
#' order. These can be set using the `set_order_count_method` function.
#' \itemize{
#' \item{Sorting by a numeric count - A selected numeric value from a selected
#' column will be indexed based on the descending numeric value. The numeric
#' value extracted defaults to 'n' but can be changed with
#' `set_byrow_numeric_value`. The column selected for sorting defaults to the
#' first value in the treatment group varialbe. If there were arguments passed
#' to the 'cols' argument in the table those must be specified with
#' `set_ordering_columns`.}
#' \item{Sorting by a 'varn' variable - If the treatment variable has a <VAR>N
#' variable. It can be indexed to that variable.}
#' \item{Sorting by a factor(Default) - If a factor is found for the target
#' variable in the target dataset that is used to order, if no factor is found
#' it is coersed to a factor and sorted alphabetically.}
#' \item{Sorting a nested count layer - WIP}
#' }
#'
#' @section Ordering a Desc Layer:
#' The order of a desc layer is mostly set during the object construction. The
#' by variables are resolved and index with the same logic as the count layers.
#' The target variable is ordered based on the format strings that were used
#' when the layer was created.
#'
#' @return An ordered data.frame if a data.frame was passed. Or nothing in the
#'   case of a tplyr_layer. These are adjusted silently and will be ordered
#'   when the table is output.
#' @export
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
#' @noRd
get_varn_values <- function(.data, a_by) {

  # A data.frame with only the by variable and its <by>N counterpart
  varn_df <- unique(.data[, c(as_name(a_by), paste0(as_name(a_by), "N"))])

  # For the <VAR>N to be well formed there should be one and only one N for each value.
  # Therefore the number unique indices should be the number of total rows.
  assert_that(nrow(unique(varn_df[, paste0(as_name(a_by), "N")])) == nrow(varn_df),
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

    # Set default for ordering column. A lot of weird stripping of the object is done here
    # to make sure its the right class
    if (is.null(ordering_cols)) ordering_cols <- quos(!!unname(unlist(as.character(
      head(unique(pop_data[, as_name(treat_var)]), 1)
      ))))

    if (is.null(order_count_method)) order_count_method <- "byfactor"

    # Number of sorting columns needed, number of bys plus one for the target_var
    formatted_row_index <- length(by) + 1

    # This adds a column for each by variable and the target variable.
    walk2(by, seq_along(by), function(a_by, by_i) {
      # If a_by is a character, skip and go to the next, it doesn't have any sorting information
      if (!is.name(quo_get_expr(a_by))) return()
      formatted_data[, paste0("ord_layer_", by_i)] <<- get_by_order(formatted_data, target, by_i, a_by)
    })

    formatted_data[, paste0("ord_layer_", formatted_row_index)] <- get_data_order(current_env(), formatted_row_index)
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
    formatted_row_index <- length(by) + 1

    formatted_data[, paste0("ord_layer_", formatted_row_index)] <- seq(nrow(formatted_data))


  }, envir = x)
}

#' The indicies of the rows based on the by variables
#'
#'
#' @param formatted_data The formatted_data object from the layer
#' @param target The target dataset in the tplyr_table
#' @param i The index of the by variable i.e. the order it was passed in the
#'   \code{tplyr_layer}.
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
#'
#' @return Returns the index the variables should be ordered in, in the cases of
#'   ordering by a <VAR>N variable and ordering by factors. Returns the numeric
#'   value in the case of ordering by column values.
#'
#' @noRd
get_data_order <- function(x, formatted_row_index) {

  evalq({
    if (order_count_method == "bycount") {

      if (is.null(byrow_numeric_value)) byrow_numeric_value <- quo(n)

      get_data_order_bycount(formatted_data, numeric_data, ordering_cols,
                             treat_var, by, cols, order_count_rows, byrow_numeric_value)

    } else if (order_count_method == "byvarn") {

      assert_that(has_varn(target, as_name(target_var[[1]])), msg = "No VAR<N> variables present")

      varn_df <- get_varn_values(target, as_name(target_var[[1]]))

      get_data_order_byvarn(formatted_data, varn_df, as_name(target_var[[1]]),
                            formatted_row_index)


      # Here it is 'byfactor'
    } else {

      target_data <- target[, as_name(target_var[[1]])]

      #Pull levels from target variable
      target_levels <- levels(target_data)

      # If the levels are null, the target was not a factor. So turn it into a factor
      if (is.null(target_levels)) {

        # Change target variable into a factor
        target_fact <- as.factor(target_data)

        # Create data.frame with levels and index
        fact_df <- tibble(
          !!target_var[[1]] := unique(sort(target_fact)),
          factor_index := unclass(unique(sort(target_fact)))
        )

        # The logic is the same now for a byvarn so reuse that function
        get_data_order_byvarn(formatted_data, fact_df, as_name(target_var[[1]]), formatted_row_index)

      } else {

        fact_df <- tibble(
          !!target_var[[1]] := unique(sort(target_data)),
          factor_index := unclass(unique(sort(target_data)))
        )

        get_data_order_byvarn(formatted_data, fact_df, as_name(target_var[[1]]), formatted_row_index)

      }
    }
  }, envir = x)
}

#' Helper method for get_data_order
#' @noRd
get_data_order_bycount <- function(formatted_data, numeric_data, ordering_cols,
                       treat_var, by, cols, order_count_rows, byrow_numeric_value) {

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
  if (as_name(byrow_numeric_value) == "pct") {

    # This isn't in an evalq so the modifictions here won't do anything to the layer
    numeric_data[, "pct"] <- numeric_data[, "n"] / numeric_data[, "total"]

  } else if (as_name(byrow_numeric_value) == "distinct_pct") {

    numeric_data[, "distinct_pct"] <- numeric_data[, "distinct_n"] / numeric_data[, "distinct_total"]
  }

  # What will become the column name of the tibble below. It nests the names of the ordering cols and
  # separates them with an underscore
  result_column <- paste(map_chr(ordering_cols, as_name), collapse = "_")

  ## WARNING: This has to be the same logic as the pivot in the count ordering or else it won't work
  numeric_ordering_data <- numeric_data %>%
    filter(!!!filter_logic) %>%

    # I'm like 80% sure this logic works out.
    pivot_wider(id_cols = c(match_exact(by), "summary_var"),
                names_from = c(!!treat_var, !!!cols), values_from = !!byrow_numeric_value) %>%

    select(as.symbol(result_column))

  # This is the numeric index that the numeric data is in. radix was chosen because
  # its the only method that gives indicies as far as I can tell
  # x are the values
  # ix are the indicies
  numeric_ordering_index <- sort(numeric_ordering_data[[1]], method = "radix" ,
                                 index.return = TRUE, decreasing = TRUE)


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

    # If the row is legnth zero it is a total row. Just add one so it apears on the bottom
    if (nrow(ind_row) == 0) {
      max(by_varn_df[,2]) + 1
    } else {
      # Index is always in the second row
      as.double(unlist(ind_row[, 2]))
    }
  })
}
