

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
set_distinct_by <- function(e, distinct_by) {
  distinct_by <- enquo(distinct_by)

  # Any other assertions needed here?
  assert_inherits_class(e, "count_layer")

  env_bind(e, distinct_by = distinct_by)

  e
}
