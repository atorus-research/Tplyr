### Formatting

#' Create a \code{f_str} object
#'
#' \code{f_str} objects are intended to be used within the function
#' \code{set_format_strings}. The \code{f_str} object carries information that
#' powers a significant amount of layer processing. The \code{format_string}
#' parameter is capable of controlling the display of a data point and decimal
#' precision. The variables provided in \code{...} control which data points are
#' used to populate the string formatted output.
#'
#' @details Format strings are one of the most powerful components of 'Tplyr'.
#'   Traditionally, converting numeric values into strings for presentation can
#'   consume a good deal of time. Values and decimals need to align between
#'   rows, rounding before trimming is sometimes forgotten - it can become a
#'   tedious mess that is realistically not an important part of the analysis
#'   being performed. 'Tplyr' makes this process as simple as we can, while
#'   still allowing flexibility to the user.
#'
#'   Tplyr provides both manual and automatic decimal precision formatting. The
#'   display of the numbers in the resulting data frame is controlled by the
#'   \code{format_string} parameter. For manual precision, just like dummy
#'   values may be presented on your mocks, integer and decimal precision is
#'   specified by the user providing a string of 'x's for how you'd like your
#'   numbers formatted. If you'd like 2 integers with 3 decimal places, you
#'   specify your string as 'xx.xxx'. 'Tplyr' does the work to get the numbers
#'   in the right place.
#'
#'   To take this a step further, automatic decimal precision can also be
#'   obtained based on the collected precision within the data. When creating
#'   tables where results vary by some parameter, different results may call for
#'   different degrees of precision. To use automatic precision, use a single
#'   'a' on either the integer and decimal side. If you'd like to use increased
#'   precision (i.e. you'd like mean to be collected precision +1), use 'a+1'.
#'   So if you'd like both integer and and decimal precision to be based on the
#'   data as collected, you can use a format like 'a.a' - or for collected+1
#'   decimal precision, 'a.a+1'.  You can mix and match this with manual formats
#'   as well, making format strings such as 'xx.a+1'.
#'
#'   If you want two numbers on the same line, you provide two sets of x's. For
#'   example, if you're presenting a value like "mean (sd)" - you could provide
#'   the string 'xx.xx (xx.xxx)', or perhaps 'a.a+1 (a.a+2). Note that you're
#'   able to provide  different integer lengths and different decimal precision
#'   for the two values. Each format string is independent and relates only to
#'   the format specified.
#'
#'   The other parameters of the \code{f_str} call specify what values should
#'   fill the x's. \code{f_str} objects are used slightly differently between
#'   different layers. When declaring a format string within a count layer,
#'   \code{f_str} expects to see the values \code{n} and (if desired)
#'   \code{pct}, which specifies the formatting for your n's and percent values.
#'   But in descriptive statistic layers, \code{f_str} parameters refer to the
#'   names of the summaries being performed, either by built in defaults, or
#'   custom summaries declared using \code{\link{set_custom_summaries}}. See
#'   \code{\link{set_format_strings}} for some more notes about layers specific
#'   implementation.
#'
#'   Count and shift layers frequencies and percentages can be specified with
#'   'n' and 'pct' respectively. Distinct values can also be presented in count
#'   layers with the arguments 'distinct' and 'distinct_total'.
#'
#' @param format_string The desired display format. X's indicate digits. On the
#'   left, the number of x's indicates the integer length. On the right, the
#'   number of x's controls decimal precision and rounding. Variables are
#'   inferred by any separation of the 'x' values other than a decimal.
#' @param ... The variables to be formatted using the format specified in
#'   \code{format_string}.
#' @param empty The string to display when the numeric data is not available.
#'   For desc layers, an unnamed character vector will populate within the
#'   provided format string, set to the same width as the fitted numbers. Use a
#'   single element character vector, with the element named '.overall' to
#'   instead replace the whole string.
#'
#' @return A \code{f_str} object
#' @export
#'
#' @examples
#'
#' f_str("xx.x (xx.x)", mean, sd)
#'
#' f_str("a.a+1 (a.a+2)", mean, sd)
#'
#' f_str("xx.a (xx.a+1)", mean, sd)
#'
#' f_str("xx.x, xx.x, xx.x", q1, median, q3)
#'
f_str <- function(format_string, ..., empty=c(.overall='')) {

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
  rx <- "(a(\\+\\d+)?|x+)(\\.(a(\\+\\d+)?|x+)?)?"
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
  auto_precision <- any(map_lgl(settings, ~any(attr(.x, 'auto'))))

  # All ellipsis variables are names
  assert_that(all(map_lgl(vars, function(x) class(x) == "name")),
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
#' @return A named vector with the names "int" and "dec", countaining numeric values
#'
#' @noRd
separate_int_dig <- function(x){

  # Initialize a vector and name the elements
  out <- numeric(2)
  names(out) <- c('int', 'dec')
  attr(out, 'auto') <- c('int'=FALSE, 'dec'=FALSE)

  # Count the characters on each side of the decimal
  fields <- str_split(x, "\\.")[[1]]

  num_chars <- map(fields, parse_fmt)
  auto <- map_lgl(num_chars, ~attr(.x, 'auto'))
  num_chars <- as.numeric(num_chars)
  attr(num_chars, 'auto') <- auto

  # Insert the number of characters into the named vector
  for (i in seq_along(num_chars)) {
    out[i] <- num_chars[i]
    attr(out, 'auto')[i] <- attr(num_chars, 'auto')[i]
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
    add <- replace_na(as.double(str_extract(x, '\\d+')), 0)
    # Auto formats will be -1 - the specified precision
    val <- 0 + add
    # Give an attribute that there's an auto format
    attr(val, 'auto') <- TRUE
  } else {
    val <- nchar(x)
    # Not auto format
    attr(val, 'auto') <- FALSE
  }
  val
}

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
#'   object to specify how you want your n's (and possibly percents) formatted.
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
#' @return
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

  # Grab the named parameters
  params <- list(...)

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

#' Format a numeric value using an \code{f_str} object
#'
#' Using the \code{f_str} object, information about the integer length and
#' significant digits are extracted. Proper round is applied and the formatted numeric value is returned.
#'
#'
#' @param val Numeric value to be formatted
#' @param fmt \code{f_str} object with formatting information related to numeric value to be formatted
#' @param i Index of the format within the \code{f_str} object
#' @param autos A named numeric vector containing the 'auto' formatting values
#'   for the integer length(int) and decimal length(dec).
#'
#' @return String formatted numeric value
#' @noRd
num_fmt <- function(val, i, fmt=NULL, autos=NULL) {

  assert_that(is.numeric(val))
  assert_has_class(fmt, 'f_str')
  assert_that(i <= length(fmt$formats), msg="In `num_fmt` supplied ")

  # Auto precision requires that integer and decimal are
  # pulled from the row. If auto, settings will be the amount to add
  # to max prec, so add those together. Otherwise pull the manually
  # specified value
  int_len <- ifelse(attr(fmt$settings[[i]],'auto')['int'],
                    fmt$settings[[i]]['int'] + autos['int'],
                    fmt$settings[[i]]['int'])

  decimals  <- ifelse(attr(fmt$settings[[i]],'auto')['dec'],
                    fmt$settings[[i]]['dec'] + autos['dec'],
                    fmt$settings[[i]]['dec'])

  # Set nsmall to input decimals
  nsmall = decimals

  # Increment digits for to compensate for display
  if (decimals > 0) decimals <- decimals + 1

  # Empty return string
  if (is.na(val)) {
    return(str_pad(fmt$empty[1], int_len+decimals, side="left"))
  }

  # Form the string
  return(
    format(
      # Round
      round(val, nsmall),
      # Set width of format string
      width=(int_len+decimals),
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
#' @noRd
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
