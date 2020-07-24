### Formatting

#' Create a \code{f_str} object
#'
#' \code{f_str} objects are intended to be used within the function \code{set_format_strings}. The \code{f_str} object carries
#' information that powers a significant amount of layer processing. The \code{format_string} parameter is capable of controlling
#' display of a data point and decimal precision. The variables provided in \code{...} control the values from the
#' data the output a particular formatted display value.
#'
#' @details
#' Format strings are one of the most powerful components of 'Tplyr'. Traditionally, converting numeric values into
#' strings for presentation can consume a good deal of time. Values and decimals need to align between rows, rounding
#' before trimming is sometimes forgotten - it can become a tedious mess that, in the grand scheme of things, is not
#' an important part of the analysis being performed. 'Tplyr' makes this process as simple as we can, while still allowing
#' flexibility to the user.
#'
#' The display of the numbers in the resulting dataframe is controlled by the \code{format_string} parameter. Just like dummy values
#' may be presented on your mocks, this is specified by the user simply by providing a string of how you'd like your strings formatted,
#' just replacing the numbers with x's. If you'd like 2 integers with 3 decimal places, you specify your string as 'xx.xxx'. 'Tplyr'
#' does the work to get the numbers in the right place.
#'
#' To take things further, if you want two numbers on the same line, you provide two sets of x's. For example, if you're presenting
#' a value like "mean (sd)" - you could provide the string 'xx.xx (xx.xxx)'. Note that you're able to provide different integer lengths and
#' different decimal precision for the two values.
#'
#' The other parameters of the \code{f_str} call specify what values should fill the x's. \code{f_str} objects are used
#' slightly differently between different layers. When declaring a format string within a count layer, \code{f_str} expects
#' to see the values \code{n} and (if desired) \code{pct}, which specifies the formatting for your n's and percent values.
#' But in descriptive statistc layers, \code{f_str} parameters refer to the names of the summaries being performed,
#' either by built in defaults, or custom summaries declared using \code{\link{set_custom_summaries}}.
#' See \code{\link{set_format_strings}} for some more notes about layers specific implementation
#'
#' @param format_string The desired display format. X's indicate digits. On the left, the number of x's indicates the integer length. On the
#' right, the number of x's controls decimal precision and rounding. Variables are inferred by any separation of the 'x' values other than a
#' decimal.
#' @param ... The variables to be formatted using the format specified in \code{format_string}.
#' @param empty The string to display when the numeric data is not available
#'
#' @return A \code{f_str} object
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

  # This regex does a few things so let's break it into pieces
  # (a(\\+\\d)?|x+) -> a, possibly followed by + and a digit, or 1 or more x's
  #    This captures the integer, with either the auto formats or x's
  # (\\.(a(\\+\\d)?|x+)?)? -> a period, then possibly the same a <+digit>, or multiple x's
  #    This captures the decimal places, but they don't have to exist
  rx <- "(a(\\+\\d)?|x+)(\\.(a(\\+\\d)?|x+)?)?"
  formats <- str_extract_all(format_string, regex(rx))[[1]]

  # Duplicate any '%' to escape them
  format_string_1 <- str_replace_all(format_string, "%", "%%")

  # Make the sprintf ready string
  repl_str <- str_replace_all(format_string_1, regex(rx), "%s")

  # Make sure that if two formats were found, two varaibles exist
  assert_that(length(formats) == length(vars),
              msg = paste0("In `f_str` ", length(formats), " formats were entered in the format string ",
                           format_string, "but ", length(vars), " variables were assigned."))

  # Pull out the integer and decimal
  settings <- map(formats, separate_int_dig)

  # A value in settings will be <0 if it's an auto format
  auto_precision <- any(map_lgl(settings, ~ any(.x < 0)))

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
         auto_precision = auto_precision,
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
  fields <- str_split(x, "\\.")[[1]]

  num_chars <- map_dbl(fields, parse_fmt)

  # Insert the number of characters into the named vector
  for (i in seq_along(num_chars)) {
    out[i] <- num_chars[i]
  }
  out
}


#' Parse a portion of a string format
#'
#' After the string is split by the decimal, parse what remains
#' Auto formats will start at -1 and decrement by the + value
#'
#' @param x Portioned string format
#'
#' @return A numeric value. >0 is literal length, <0 is auto format
#' @noRd
parse_fmt <- function(x) {
  # If it's an auto format, grab the output value
  if (grepl('a', x)) {
    # Pick out the digit
    add <- replace_na(as.double(str_extract(x, '\\d')), 0)
    # Auto formats will be -1 - the specified precision
    val <- -1 - add
  } else {
    val <- nchar(x)
  }
  val
}

#' Set the format strings and associated summaries to be performed in a layer
#'
#' 'Tplyr' allows you extensive control over how strings are presented. \code{set_format_strings} allows you
#' to apply these string formats to your layer. This behaves slightly differently between layers.
#'
#' In a count layer, you can simply provide a single \code{\link{f_str}} object to specify how you want your
#' n's (and possibly percents) formatted. If you are additionally supplying a statistic, like risk difference
#' using \code{\link{add_risk_diff}}, you specify the count formats using the name 'n_counts'. The risk difference formats
#' would then be specified using the name "riskdiff". In a descriptive statistic layer, \code{set_format_strings} allows you
#' to do a couple more things:
#' \itemize{
#' \item{By naming paramters with character strings, those character strings become a row label in the resulting data frame}
#' \item{The actual summaries that are performed come from the names used within the \code{\link{f_str}} calls}
#' \item{Using multiple summaries (declared by your \code{\link{f_str}} calls) multiple summary values can appear within
#' the same values. For example, to present mean (SD) like displays}
#' }
#'
#' @details
#' Format strings are one of the most powerful components of 'Tplyr'. Traditionally, converting numeric values into
#' strings for presentation can consume a good deal of time. Values and decimals need to align between rows, rounding
#' before trimming is sometimes forgotten - it can become a tedious mess that, in the grand scheme of things, is not
#' an important part of the analysis being performed. 'Tplyr' makes this process as simple as we can, while still allowing
#' flexibility to the user.
#'
#' See the \code{\link{f_str}} documentation for more details about how this implementation works.
#'
#' @param e Layer on which to bind format strings
#' @param ... Named parmeters containing calls to \code{f_str} to set the format strings
#'
#' @return The layer environment with the format string binding added
#' @export
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
#'     build()
#'
#' # In a descriptive statistics layer
#' tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_desc(mpg) %>%
#'       set_format_strings(
#'         "n"        = f_str("xx", n),
#'         "Mean (SD)"= f_str("xx.x", mean),
#'         "SD" = f_str("xx.xx", sd),
#'         "Median"   = f_str("xx.x", median),
#'         "Q1, Q3"   = f_str("xx, xx", q1, q3),
#'         "Min, Max" = f_str("xx, xx", min, max),
#'         "Missing"  = f_str("xx", missing)
#'       )
#'   ) %>%
#'   build()
#'
set_format_strings <- function(e, ...) {
  UseMethod("set_format_strings")
}


#' Desc layer S3 method for set_format_strings
#'
#' @param e Layer on which to bind format strings
#' @param ... Named parmeters containing calls to \code{f_str} to set the format strings
#'
#' @return
#' @export
#'
#' @noRd
set_format_strings.desc_layer <- function(e, ...) {

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
           row_labels = row_labels,
           max_length = max_format_length
    )
  e
}

#' Set Count Layer String Format
#'
#' @param e Layer on which to bind format strings
#' @param ... Named parmeters containing calls to \code{f_str} to set the format strings
#'
#' @return Returns the modified layer object.
#' @export
#'
#' @examples
#' # TBD
set_format_strings.count_layer <- function(e, ...) {
  # Grab the named parameters
  params <- list(...)

  # Make sure all parameters were f_str objects
  map(params, assert_has_class, should_be="f_str")

  # Currently supported format names
  valid_names <- c("n_counts", "riskdiff")

  # Raise error if names were invalid
  if (is_named(params)) {
    assert_that(all(names(params) %in% valid_names),
                msg = paste('Invalid format names supplied. Count layers only accept the following format names:',
                            paste(valid_names, collapse = ", "))
                )

  } else {
    # If unnamed, then only one argument should have been supplied
    assert_that(length(params) == 1, msg = "If names are not supplied, count layers can only have on format supplied.")
    # Force the name in of n_counts
    names(params) <- "n_counts"
  }


  # Check content of each f_str based on their supplied name
  for (name in names(params)) {

    if (name == "n_counts") {
      assert_that(all(params[['n_counts']]$vars %in% c("n", "pct", "distinct", "distinct_pct")),
                  msg = "f_str for n_counts in a count_layer can only be n, pct, distinct, or distinct_pct")
    } else if (name == "riskdiff") {
      assert_that(all(params[['riskdiff']]$vars %in% c('comp', 'ref', 'dif', 'low', 'high')),
                  msg = "f_str for riskdiff in a count_layer can only be comp, ref, dif, low, or high")
    }
  }

  env_bind(e, format_strings = params)

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
  if (is.na(val)) return(fmt$empty)

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

#' Check if format strings have been applied to a layer
#'
#' @param e Layer environment
#'
#' @return Boolean
#'  @noRd
has_format_strings <- function(e) {
  'format_strings' %in% ls(envir=e)
}

#' Pad Numeric Values
#'
#' This is generally used with a count layer
#'
#' @param string_ The current values of the numeric data
#' @param right_pad The total string length, done after the left pad
#' @param left_pad The length of the left_pad
#'
#' @return Modified string
#'
#' @noRd
pad_formatted_data <- function(x, right_pad, left_pad) {
  # Pad the left with difference between left_pad and nchar(string_)
  if(nchar(x)[1] < left_pad) {
    # The double pasting looks weird but the inner one is meant to create single character
    # that is the needed number of spaces and the outer pastes that to the value
    x <- map_chr(x,
                       ~ paste0(
                         paste0(rep(" ", left_pad - nchar(.x)), collapse = ""),
                         .x))
  }

  #Padd the right with the difference of the max layer length
  if(right_pad > max(nchar(x))) {
    x <- map_chr(x,
                       paste0, paste0(rep(" ", right_pad - max(nchar(x))),
                                      collapse = ""))
  }

  x
}
