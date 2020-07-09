

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
