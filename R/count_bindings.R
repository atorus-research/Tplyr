

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
#'
#' @param e A \code{group_count} layer
#' @param order_count_method The logic determining how the rows in the final
#'   layer output will be indexed. Options are 'byrow', 'byfactor', and
#'   'byvarn'.
#'
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
#' `set_result_order_var`. The column selected for sorting defaults to the
#' first value in the treatment group varialbe. If there were arguments passed
#' to the 'cols' argument in the table those must be specified with
#' `set_ordering_columns`.}
#' \item{Sorting by a 'varn' variable - If the treatment variable has a <VAR>N
#' variable. It can be indexed to that variable.}
#' \item{Sorting by a factor(Default) - If a factor is found for the target
#' variable in the target dataset that is used to order, if no factor is found
#' it is coersed to a factor and sorted alphabetically.}
#' \item{Sorting a nested count layer - As is standard with AE tables, the first
#' target variable is sorted either alphabetically, if a factor isn't provided,
#' or ordered based on a supplided factor. The second variable is sorted with
#' the 'bycount' method, the column and numeric value can be assigned with
#' `set_ordering_cols` and `set_result_order_var` respectively.}
#' }
#'
#' @section Ordering a Desc Layer:
#' The order of a desc layer is mostly set during the object construction. The
#' by variables are resolved and index with the same logic as the count layers.
#' The target variable is ordered based on the format strings that were used
#' when the layer was created.
#'
#'
#' @return Returns the modified layer object. The 'ord_' columns are added
#'   during the build process.
#' @export
#' @rdname ordering
#'
#' @examples
#' library(dplyr)
#'
#' # Default sorting by factor
#' t <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl)
#'   )
#' build(t)
#'
#' # Sorting by <VAR>N
#' mtcars$cylN <- mtcars$cyl
#' t <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       set_order_count_method("byvarn")
#'   )
#'
#' # Sorting by row count
#' t <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'     set_order_count_method("bycount") %>%
#'     # Orders based on the 6 gear group
#'     set_ordering_cols(6)
#'   )
set_order_count_method <- function(e, order_count_method) {

  assert_inherits_class(order_count_method, "character")

  assert_that(order_count_method %in% c("bycount", "byfactor", "byvarn"),
              msg = "Invalid input passed to set_order_count_method.
              Options are: 'bycount', 'byfactor', or 'byvarn'")

  env_bind(e, order_count_method = order_count_method)

  e
}

#' @param ... Unquoted variables used to select the columns whose values will be
#'   extracted for ordering.
#'
#' @export
#' @rdname ordering
set_ordering_cols <- function(e, ...) {

  ordering_cols <- enquos(...)

  check_ordering_cols <- unpack_vars(ordering_cols, allow_character = FALSE)

  treat_var <- env_get(e, "treat_var", inherit = TRUE)
  cols <- env_get(e, "cols", inherit = TRUE)

  assert_that(length(check_ordering_cols) == length(c(treat_var, cols)),
              msg = "You need to pass a variable for each treat_var and cols variable.")

  env_bind(e, ordering_cols = ordering_cols)

  e
}

#' @param result_order_var The numeric value the ordering will be done on.
#'   This can be either n, distinct_n, pct, or distinct_pct. Due to the
#'   evaluation of the layer you can add a value that isn't actually being
#'   evaluated, if this happens this will only error out in the ordering.
#'
#' @export
#' @rdname ordering
set_result_order_var <- function(e, result_order_var) {

  result_order_var <- enquo(result_order_var)

  assert_that(as_name(result_order_var) %in% c("n", "distinct_n", "pct", "distinct_pct"),
              msg = "Invalid argument for result_order_var It can be n, distinct_n, pct, or distinct_pct.")

  env_bind(e, result_order_var = result_order_var)

  e
}
