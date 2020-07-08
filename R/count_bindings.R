

#' Add a Total row into a count summary.
#'
#' Adding a total row creates an additional observation in the count summary that presents the total counts
#' (i.e. the n's that are) summarized by the \code{by} group variables and the columns (\code{treat_var} along with
#' any additional columns set by the \code{cols} argument.)
#'
#' @param x A layer object
#' @param add_total TRUE/FALSE
#'
#' @export
#' @example
#'
#'
add_total_row <- function(x, add_total) {
  assert_inherits_class(x, "count_layer")

  assert_that(is.logical(add_total))

  env_bind(x, include_total_row = add_total)

  x
}

#' Set the distinct_by binding for a count layer
#'
#' @param lay A count_layer object
#' @param distinct_by A variable to get the distinct
#'
#' @return The modified layer object
#' @export
set_distinct_by <- function(lay, distinct_by) {
  distinct_by <- enquo(distinct_by)

  # Any other assertions needed here?
  assert_inherits_class(lay, "count_layer")

  env_bind(lay, distinct_by = distinct_by)

  lay
}
