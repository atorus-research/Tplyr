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
f_str <- function(format_string, ..., empty='') {

  # Capture the variables off of the ellipsis
  vars <- enexprs(...)
  # Null out the names for vars
  names(vars) <- NULL

  # Check format string class
  assert_has_class(format_string, "character")

  # Capture the format groups
  # Regex looks for 1 or more lower case x, potentially followed by a period and more x's
  formats <- str_extract_all(format_string, regex("x+\\.{0,1}x*"))[[1]]

  # Duplicate any '%' to escape them
  format_string_1 <- str_replace_all(format_string, "%", "%%")
  # Make the sprintf ready string
  repl_str <- str_replace_all(format_string_1, regex("x+\\.{0,1}x*"), "%s")

  # Make sure that if two formats were found, two varaibles exist
  assert_that(length(formats) == length(vars),
              msg = paste0("In `f_str` ", length(formats), " formats were entered in the format string ",
                           format_string, "but ", length(vars), " variables were assigned."))

  settings <- lapply(formats, separate_int_dig)

  # All ellipsis variables are names
  assert_that(all(sapply(vars, function(x) class(x) == "name")),
              msg = "In `f_str` all values submitted via `...` must be variable names.")

  structure(
    list(format_string = format_string,
         vars = vars,
         formats = formats,
         settings = settings,
         size = nchar(format_string),
         repl_str = repl_str,
         empty=empty
    ),
    class="f_str"
  )
}

#' Evaluate a portion of a format string to check the integer and digit lengths
#'
#' @param x String to have sections counted
#'
#' @return A named vector with the names "int" and "dig", countaining numeric values
#'
#' @noRd
separate_int_dig <- function(x){

  # Initialize a vector and name the elements
  out <- numeric(2)
  names(out) <- c('int', 'dig')

  # Count the characters on each side of the decimal
  num_chars <- map_int(str_split(x, "\\.")[[1]], nchar)

  # Insert the number of characters into the named vector
  for (i in seq_along(num_chars)) {
    out[[i]] <- num_chars[[i]]
  }
  out
}


#' Set the format strings and associated summaries to be performed in a layer
#'
#' @param e Layer on which to bind format strings
#' @param ... Named parmeters containing calls to \code{f_str} to set the format strings
#'
#' @return The layer environment with the format string binding added
#' @export
#'
#' @examples
#' #TBD
set_format_strings <- function(e, ...) {

  # Pick off the ellpsis
  format_strings <- list(...)

  # Row labels are pulled from names - so make sure that everything is named
  assert_that(is_named(format_strings),
              msg = "In `set_format_string` all parameters must be named in order to create row labels.")

  # Make sure that all of the attachments were `f_str`` objects
  for (name in names(format_strings)) {
    assert_that(class(format_strings[[name]]) == "f_str",
                msg = paste0("In `set_format_string` entry `",name,"` is not an `f_str` object. All assignmentes made within",
                             " `set_format_string` must be made using the function `f_str`. See the `f_str` documentation."))
  }

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

  env_bind(e,
           format_strings = format_strings,
           summary_vars = vars(!!!summary_vars),
           keep_vars = vars(!!!keep_vars),
           trans_vars = vars(!!!trans_vars),
           row_labels = row_labels
    )
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
#'
name_translator <- function(fmt_strings) {
  out <- names(fmt_strings)
  names(out) <- map_chr(fmt_strings, ~ as_label(.x$vars[[1]]))
  out
}

#' Format a numeric value using an \code{f_str} object
#'
#' Using the \code{f_str} object, information about the integer length and
#' significant digits are extracted. Proper round is applied and the formatted numeric value is returned.
#'
#'
#' @param val Numeric value to be formatted
#' @param fmt \code{f_str} object with formatting information related to numeric value to be formatted
#' @param i Index of the format within the \code{f_str} object
#'
#' @return String formatted numeric value
#' @export
num_fmt <- function(val, i, fmt=NULL) {

  assert_that(is.numeric(val))
  assert_has_class(fmt, 'f_str')
  assert_that(i <= length(fmt$formats), msg="In `num_fmt` supplied ")

  int_len <- fmt$settings[[i]]['int']
  digits <- fmt$settings[[i]]['dig']

  # Formats summary stat strings to align display correctly
  if (is.na(val)) return(empty)

  # Set nsmall to input digits
  nsmall = digits

  # Incremement digits for to compensate for display
  if (digits > 0) digits <- digits + 1

  # Form the string
  return(
    format(
      # Round
      round(val, nsmall),
      # Set width of format string
      width=(int_len+digits),
      # Decimals to display
      nsmall=nsmall
    )
  )
}
