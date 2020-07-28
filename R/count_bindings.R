

#' Add a Total row into a count summary.
#'
#' Adding a total row creates an additional observation in the count summary that presents the total counts
#' (i.e. the n's that are) summarized by the \code{by} group variables and the columns (\code{treat_var} along with
#' any additional columns set by the \code{cols} argument.)
#'
#' @param e A layer object
#'
#' @export
#' @examples
#' # Load in Pipe
#' library(magrittr)
#'
#' tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       add_total_row()
#'    ) %>%
#'    build()
add_total_row <- function(e) {
  assert_inherits_class(e, "count_layer")

  env_bind(e, include_total_row = TRUE)

  e
}

#' Set the label for the total row
#'
#' The row label for a total row defaults to "Total", however this can be
#' overriden using this function.
#'
#' @param e A \code{count_layer} object
#' @param total_row_label A character to label the total row
#'
#' @return The modified \code{count_layer} object
#' @export
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#'
#' t <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       add_total_row() %>%
#'       set_total_row_label("Total Cyl")
#'   )
#' build(t)
set_total_row_label <- function(e, total_row_label) {

  assert_has_class(total_row_label, "character")
  assert_that(length(total_row_label) == 1)

  env_bind(e, total_row_label = total_row_label)

  e
}

#' Set counts to be distinct by some grouping variable.
#'
#' Occasionally summaries call for counting distint values within a group. \code{set_distinct_by}
#' will update a count layer to only count distinct values by the specified variables.
#'
#' @param e A count_layer object
#' @param distinct_by A variable to get the distinct
#'
#' @return The layer object with
#' @export
#'
#' @examples
#' #Load in pipe
#' library(magrittr)
#'
#' tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       set_distinct_by(carb)
#'   ) %>%
#'   build()
set_distinct_by <- function(e, distinct_by) {
  distinct_by <- enquo(distinct_by)

  # Any other assertions needed here?
  assert_inherits_class(e, "count_layer")

  env_bind(e, distinct_by = distinct_by)

  e
}

#' Set the option to prefix the row_label values when a layer is formatted
#'
#' This is generally used internally with a nested count layer.
#'
#' @param e A tplyr_count layer
#' @param count_row_prefix A character to prefix the row labels
#'
#' @return The modified count_layer environment
#' @export
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#'
#' tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       set_count_row_prefix("\t")
#'   ) %>%
#'   build()
#'
set_count_row_prefix <- function(e, count_row_prefix) {

  assert_inherits_class(count_row_prefix, "character")

  assert_that(length(count_row_prefix) == 1)

  env_bind(e, count_row_prefix = count_row_prefix)

  e
}

#' Set the option to prefix the row_labels in the inner count_layer
#'
#' This is generally used internally with a nested count layer.
#'
#' @param e A tplyr_count layer
#' @param indentation A character to prefix the row labels in an inner
#'   count layer
#'
#' @return The modified count_layer environment
#' @export
set_indentation <- function(e, indentation) {

  assert_inherits_class(indentation, "character")

  assert_that(length(indentation) == 1)

  env_bind(e, indentation = indentation)

  e
}

#' Set the option to nest count layers
#'
#' @param e A tplyr_count layer
#' @param nest_count A logical value to set the nest option
#'
#' @return The modified layer
#' @export
set_nest_count <- function(e, nest_count) {

  assert_inherits_class(nest_count, "logical")

  assert_that(length(nest_count) == 1)

  env_bind(e, nest_count = nest_count)

  e

}


#' Set the ordering logic for the count layer
#'
#' Count layers are generally displayed in a descending order for a particular
#' treatment group. However Tplyr also supports displaying based on the factor
#' of the treatment variable, or on a specified <VAR>N variable already in the
#' dataset (i.e. VISIT <-> VISITN, TRT <-> TRTN). Defaults to 'byrow'.
#'
#' @param e A \code{group_count} layer
#' @param order_count_method The logic determining how the rows in the final
#'   layer output will be indexed. Options are 'byrow', 'byfactor', and
#'   'byvarn'.
#'
#' @return The modifed layer object
#' @export
set_order_count_method <- function(e, order_count_method) {

  assert_inherits_class(order_count_method, "character")

  assert_that(order_count_method %in% c("byrow", "byfactor", "byvarn"),
              msg = "Invalid input passed to set_order_count_method.
              Options are: 'byrow', 'byfactor', or 'byvarn'")

  env_bind(e, order_count_method = order_count_method)

  e
}

#' Set the columns to use when ordering the data in the count layer
#'
#' @param e A tplyr \code{group_count} object
#' @param ... Unquoted variables used to select the columns whose values will be extracted for ordering.
#'
#' @return
#' @export
set_ordering_cols <- function(e, ...) {

  ordering_cols <- enquos(...)

  check_ordering_cols <- unpack_vars(ordering_cols, allow_character = FALSE)

  treat_var <- env_get(e, "treat_var", inherit = TRUE)
  cols <- env_get(e, "cols", inherit = TRUE)

  assert_that(map(check_ordering_cols, as_name) %in% map(c(treat_var, cols), as_name),
              msg = "Arguments passed to set_ordering_cols weren't found as columns in the table.")

  assert_that(length(check_ordering_cols) == length(c(treat_var, cols)),
              msg = "You need to pass a variable for each treat_var and cols variable.")

  env_bind(e, ordering_cols = ordering_cols)

  e
}

#' Set the value the count layer is sorting on when the method is byrow.
#'
#' The sorting logic will pull out the numeric value in the count layer and
#' add that as an ordering column. You can select which numeric value to pull
#' from.
#'
#' @param e A \code{group_count} layer
#' @param byrow_numeric_value The numeric value the ordering will be done on.
#'   This can be either n, distinct_n, pct, or distinct_pct. Due to the
#'   evaluation of the layer you can add a value that isn't actually being
#'   evaluated, if this happens this will only error out in the ordering.
#'
#' @return The modified layer object
#' @export
set_byrow_numeric_value <- function(e, byrow_numeric_value) {

  byrow_numeric_value <- enquo(byrow_numeric_value)

  assert_that(as_name(byrow_numeric_value) %in% c("n", "distinct_n", "pct", "distinct_pct"),
              msg = "Invalid argument for byrow_numeric_value. It can be n, distinct_n, pct, or distinct_pct.")

  env_bind(e, byrow_numeric_value = byrow_numeric_value)

  e
}
