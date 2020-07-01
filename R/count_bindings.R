

#' Set the include_total_row option for count processing
#'
#' @param x A layer object
#' @param include_total A logical vector
set_include_total_row <- function(x, include_total) {
  assert_inherits_class(x, "count_layer")

  assert_that(is.logical(include_total))

  env_bind(x, include_total_row = include_total)

  x
}

#' Set Count Layer String Format
#'
#' @param x the layer object to add/modify the count format
#' @param str The f_str object to add
#'
#' @return
#' @export
#'
#' @examples
set_count_fmt <- function(x, str) {
  assert_inherits_class(x, "count_layer")

  assert_has_class(str, "f_str")

  assert_that(all(str$vars %in% c("n", "pct")),
              msg = "f_str in a count_layer can only be n or pct")

  env_bind(x, format_strings = str)

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
