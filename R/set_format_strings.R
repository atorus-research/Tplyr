#' Set the format strings and associated summaries to be performed in a layer
#'
#' 'Tplyr' gives you extensive control over how strings are presented.
#' \code{set_format_strings} allows you to apply these string formats to your
#' layer. This behaves slightly differently between layers.
#'
#' @details Format strings are one of the most powerful components of 'Tplyr'.
#'   Traditionally, converting numeric values into strings for presentation can
#'   consume a good deal of time. Values and decimals need to align between
#'   rows, rounding before trimming is sometimes forgotten - it can become a
#'   tedious mess that, in the grand scheme of things, is not an important part
#'   of the analysis being performed. 'Tplyr' makes this process as simple as we
#'   can, while still allowing flexibility to the user.
#'
#'   In a count layer, you can simply provide a single \code{\link{f_str}}
#'   object to specify how you want your n's, percentages, and denominators formatted.
#'   If you are additionally supplying a statistic, like risk difference using
#'   \code{\link{add_risk_diff}}, you specify the count formats using the name
#'   'n_counts'. The risk difference formats would then be specified using the
#'   name 'riskdiff'. In a descriptive statistic layer,
#'   \code{set_format_strings} allows you to do a couple more things:
#'   \itemize{
#'   \item{By naming parameters with character strings, those character strings
#'   become a row label in the resulting data frame}
#'   \item{The actual summaries that are performed come from the variable names
#'   used within the \code{\link{f_str}} calls}
#'   \item{Using multiple summaries (declared by your \code{\link{f_str}}
#'   calls), multiple summary values can appear within the same line. For
#'   example, to present "Mean (SD)" like displays.}
#'   \item{Format strings in the desc layer also allow you to configure how
#'   empty values should be presented. In the \code{f_str} call, use the
#'   \code{empty} parameter to specify how missing values should present. A
#'   single element character vector should be provided. If the vector is
#'   unnamed, that value will be used in the format string and fill the space
#'   similar to how the numbers will display. Meaning - if your empty string is
#'   'NA' and your format string is 'xx (xxx)', the empty values will populate
#'   as 'NA ( NA)'. If you name the character vector in the 'empty' parameter
#'   '.overall', like \code{empty = c(.overall='')}, then that exact string will
#'   fill the value instead. For example, providing 'NA' will instead create the
#'   formatted string as 'NA' exactly.}
#'   }
#'
#'   See the \code{\link{f_str}} documentation for more details about how this
#'   implementation works.
#'
#' @param e Layer on which string formats will be bound
#' @param ... Named parameters containing calls to \code{f_str} to set the
#'   format strings
#'
#' @return The layer environment with the format string binding added
#' @export
#' @rdname set_format_strings
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#'
#' # In a count layer
#' tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_count(cyl) %>%
#'       set_format_strings(f_str('xx (xx%)', n, pct))
#'   ) %>%
#'   build()
#'
#' # In a descriptive statistics layer
#' tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_desc(mpg) %>%
#'       set_format_strings(
#'         "n"         = f_str("xx", n),
#'         "Mean (SD)" = f_str("xx.x", mean, empty='NA'),
#'         "SD"        = f_str("xx.xx", sd),
#'         "Median"    = f_str("xx.x", median),
#'         "Q1, Q3"    = f_str("xx, xx", q1, q3, empty=c(.overall='NA')),
#'         "Min, Max"  = f_str("xx, xx", min, max),
#'         "Missing"   = f_str("xx", missing)
#'       )
#'   ) %>%
#'   build()
#'
#' # In a shift layer
#' tplyr_table(mtcars, am) %>%
#'   add_layer(
#'     group_shift(vars(row=gear, column=carb), by=cyl) %>%
#'     set_format_strings(f_str("xxx (xx.xx%)", n, pct))
#'   ) %>%
#'   build()
#'
set_format_strings <- function(e, ...) {
  UseMethod("set_format_strings")
}


#' Desc layer S3 method for set_format_strings
#'
#' @param e Layer on which to bind format strings
#' @param ... Named parameters containing calls to \code{f_str} to set the
#'   format strings
#' @param cap A named character vector containing an 'int' element for the cap
#'   on integer precision, and a 'dec' element for the cap on decimal precision.
#'
#' @return tplyr_layer object with formats attached
#' @export
#'
#' @rdname set_format_strings
set_format_strings.desc_layer <- function(e, ..., cap=getOption('tplyr.precision_cap')) {

  # Catch the arguments from the function call so useful errors can be thrown
  check <- enquos(...)

  # Make sure that all of the attachments were `f_str` objects
  for (i in seq_along(check)) {

    if (is_named(check)) {
      msg = paste0("In `set_format_string` entry `",names(check)[[i]],"` is not an `f_str` object. All assignmentes made within",
                   " `set_format_string` must be made using the function `f_str`. See the `f_str` documentation.")
    } else {
      msg = paste0("In `set_format_string` entry ",i," is not an `f_str` object. All assignmentes made within",
                   " `set_format_string` must be made using the function `f_str`. See the `f_str` documentation.")
    }

    assert_that(class(quo_get_expr(check[[i]])) == "f_str" || (is_call(quo_get_expr(check[[i]])) && call_name(check[[i]]) == "f_str"),
                msg = msg)
  }

  # Row labels are pulled from names - so make sure that everything is named
  assert_that(is_named(check),
              msg = "In `set_format_string` all parameters must be named in order to create row labels.")


  # Pick off the ellipsis
  format_strings <- list(...)


  # Get the list of variable names that need to be transposed
  summary_vars <- flatten(map(format_strings, ~ .x$vars))

  # Get the list of transpose transpose variables needed
  trans_vars <- map(format_strings, ~ .x$vars[[1]])

  # Get the variable names that need to be kept on the same row
  keep_vars <- flatten(map(format_strings, ~ tail(.x$vars, n=length(.x$vars) -1)))

  # Get the max format length
  max_format_length <- max(map_int(format_strings, ~ .x$size))

  # Pick off the row labels
  row_labels <- names(format_strings)

  # Identify if auto precision is needed
  need_prec_table <- any(map_lgl(format_strings, ~ .x$auto_precision))

  # Fill in defaults if cap hasn't fully been provided
  if (!('int' %in% names(cap))) cap['int'] <- getOption('tplyr.precision_cap')['int']
  if (!('dec' %in% names(cap))) cap['dec'] <- getOption('tplyr.precision_cap')['dec']

  env_bind(e,
           format_strings = format_strings,
           summary_vars = vars(!!!summary_vars),
           keep_vars = vars(!!!keep_vars),
           trans_vars = vars(!!!trans_vars),
           row_labels = row_labels,
           max_length = max_format_length,
           need_prec_table = need_prec_table,
           cap = cap
  )
  e
}

#' Set Count Layer String Format
#'
#' @param e Layer on which to bind format strings
#' @param ... Named parameters containing calls to \code{f_str} to set the format strings
#'
#' @return Returns the modified layer object.
#' @export
#' @rdname set_format_strings
set_format_strings.count_layer <- function(e, ...) {

  params <- count_f_str_check(...)

  env_bind(e, format_strings = params)

  e
}

set_format_strings.shift_layer <- function(e, ...) {

  dots <- list(...)

  assert_that(all(dots$vars %in% c("n", "pct")),
              msg = "formats in shift layers can only be n")

  env_bind(e, format_strings = dots[[1]])

  e
}

#' Extract a translation vector for f_str objects
#'
#' The names of the format_strings list should be row labels in the output. The first
#' element of the \code{vars} object are the transpose variables, so make the names of
#' those variables the vector names, and the names of the format_strings elements the
#' values to allow easy creation of a \code{row_labels} variable in the data
#'
#' @param fmt_strings The \code{format_strings} varaible in a layer
#'
#' @return A named character vector with the flipping applied
#' @noRd
name_translator <- function(fmt_strings) {
  out <- names(fmt_strings)
  names(out) <- map_chr(fmt_strings, ~ as_name(.x$vars[[1]]))
  out
}

#' Check if format strings have been applied to a layer
#'
#' @param e Layer environment
#'
#' @return Boolean
#' @noRd
has_format_strings <- function(e) {
  'format_strings' %in% ls(envir=e)
}
