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

  # Capture the format groups
  # Regex looks for 1 or more lower case x, potentially followed by a period and more x's
  formats <- str_extract_all(format_string, regex("x+\\.{0,1}x*"))[[1]]

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
         size = nchar(format_string)
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
#' TBD
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
           summary_vars = summary_vars,
           keep_vars = keep_vars,
           row_labels = row_labels,
           trans_vars = trans_vars,
           max_format_length = max_format_length
    )
  e
}
