### Formatting

#' Create a \code{f_str} object
#'
#' The \code{f_str} object carries information that powers a significant amount of layer processing. The \code{format_string} parameter is
#' capable of controlling display of a data point and decimal precision. The variables provided in \code{...} control the values from the
#' data the output a particular formatted display value.
#'
#' @param format_string The desired display format. X's indicate digits. On the left, the number of x's indicates the integer length. On the
#' right, the number of x's controls decimal precision and rounding. Variables are inferred by any separation of the 'x' values other than a
#' decimal.
#' @param ... The variables to be formatted using the format specified in \code{format_string}.
#'
#' @return A \code{f_str} object, built on a list with two elements:
#' \describe{
#' \item{\code{format_string}}{The specified format string for display}
#' \item{\code{vars}}{A list of names containing the variables that will be used to created the formatted display string}
#' }
#' @export
#'
#' @examples
#' f_str("xx.x (xx.x)", mean, sd)
#'
f_str <- function(format_string, ...) {

  # Capture the variables off of the ellipsis
  vars <- enexprs(...)

  # Check format string class
  assert_has_class(format_string, "character")

  # All ellipsis variables are names
  assert_that(all(sapply(vars, function(x) class(x) == "name")),
              msg = "In `f_str` all values submitted via `...` must be variable names.")

  structure(
    list(format_string = format_string, vars = vars),
    class=c("f_str", "list")
  )
}
