#' Apply Format Strings outside of a Tplyr table
#'
#' The `f_str` object in Tplyr is used to drive formatting of the outputs
#' strings within a Tplyr table. This function allows a user to use the same
#' interface to apply formatted string on any data frame within a
#' `dplyr::mutate()` context.
#'
#' Note that auto-precision is not currently supported within `apply_formats()`
#'
#' @param format_string The desired display format. X's indicate digits. On the
#'   left, the number of x's indicates the integer length. On the right, the
#'   number of x's controls decimal precision and rounding. Variables are
#'   inferred by any separation of the 'x' values other than a decimal.
#' @param ... The variables to be formatted using the format specified in
#'   \code{format_string}. These must be numeric variables.
#' @param empty The string to display when the numeric data is not available.
#'   Use a single element character vector, with the element named '.overall' to
#'   instead replace the whole string.
#'
#' @return Character vector of formatted values
#' @md
#' @export
#'
#' @examples
#'
#' library(dplyr)
#'
#' mtcars %>%
#'   head() %>%
#'   mutate(
#'     fmt_example = apply_formats('xxx (xx.x)', hp, wt)
#'   )
apply_formats <- function(format_string, ..., empty = c(.overall = "")) {
  format <- f_str(format_string, ..., empty=empty)

  if (format$auto_precision) {
    stop('Auto-precision is not currently supported within the `apply_formats()` context',
         call.=FALSE)
  }

  pmap_chr(list(...), function(...) apply_fmts(...), fmt=format)
}

#' Application of individual format string
#'
#' This is what's used internally on the vectorized apply_formats
#'
#' @param ... The variables to be formatted using the format specified in
#'   the `f_str` object
#' @param fmt An f_str object
#' @md
#'
#' @return An individually formatted string
#' @noRd
apply_fmts <- function(..., fmt) {
  nums <- list(...)
  repl <- vector('list', length(fmt$settings))
  for (i in seq_along(fmt$settings)) {
    repl[[i]] <- num_fmt(nums[[i]], i, fmt=fmt)
  }
  args <- append(list(fmt$repl_str), repl)
  do.call('sprintf', args)
}
