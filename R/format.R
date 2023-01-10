### Formatting

#' Create a `f_str` object
#'
#' `f_str` objects are intended to be used within the function
#' `set_format_strings`. The `f_str` object carries information that powers a
#' significant amount of layer processing. The `format_string` parameter is
#' capable of controlling the display of a data point and decimal precision. The
#' variables provided in `...` control which data points are used to populate
#' the string formatted output.
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
#'   `format_string` parameter. For manual precision, just like dummy values may
#'   be presented on your mocks, integer and decimal precision is specified by
#'   the user providing a string of 'x's for how you'd like your numbers
#'   formatted. If you'd like 2 integers with 3 decimal places, you specify your
#'   string as 'xx.xxx'. 'Tplyr' does the work to get the numbers in the right
#'   place.
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
#'   able to provide different integer lengths and different decimal precision
#'   for the two values. Each format string is independent and relates only to
#'   the format specified.
#'
#'   As described above, when using 'x' or 'a', any other character within the
#'   format string will stay stationary. So for example, if your format string
#'   is 'xx (xxx.x)', your number may format as '12 ( 34.5)'. So the left side
#'   parenthesis stays fixed. In some displays, you may want the parenthesis to
#'   'hug' your number. Following this example, when allotting 3 spaces for the
#'   integer within parentheses, the parentehsis should shift to the right,
#'   making the numbers appear '12  (34.5)'. Using `f_str()` you can achieve
#'   this by using a capital 'X' or 'A'. For this example, the format string
#'   would be 'xx (XXX.x)'.
#'
#'   There are a two rules when using 'parenthesis hugging':
#'
#'   - Capital letters should only be used on the integer side of a number
#'   - A character must precede the capital letter, otherwise there's no
#'   character to 'hug'
#'
#'   The other parameters of the `f_str` call specify what values should fill
#'   the x's. `f_str` objects are used slightly differently between different
#'   layers. When declaring a format string within a count layer, `f_str()`
#'   expects to see the values `n` or `distinct_n` for event or distinct counts,
#'   `pct` or `distinct_pct` for event or distinct percentages, or `total` or
#'   `distinct_total` for denominator calculations. Note that in an `f_str()`
#'   for a count layer 'A' or 'a' are based on n counts, and therefore don't
#'   make sense to use in percentages. But in descriptive statistic layers,
#'   `f_str` parameters refer to the names of the summaries being performed,
#'   either by built in defaults, or custom summaries declared using
#'   [set_custom_summaries()]. See [set_format_strings()] for some more notes
#'   about layers specific implementation.
#'
#'   An `f_str()` may also be used outside of a Tplyr table. The function
#'   [apply_formats()] allows you to apply an `f_str` within the context of
#'   [dplyr::mutate()] or more generally a vectorized function.
#'
#' @section Valid `f_str()` Variables by Layer Type:
#'
#'   Valid variables allowed within the `...` parameter of `f_str()` differ by
#'   layer type.
#'
#'   - Count layers
#'     - `n`
#'     - `pct`
#'     - `total`
#'     - `distinct_n`
#'     - `distinct_pct`
#'     - `distinct_total`
#'   - Shift layers
#'     - `n`
#'     - `pct`
#'     - `total`
#'   - Desc layers
#'     - `n`
#'     - `mean`
#'     - `sd`
#'     - `median`
#'     - `var`
#'     - `min`
#'     - `max`
#'     - `iqr`
#'     - `q1`
#'     - `q3`
#'     - `missing`
#'     - Custom summaries created by [set_custom_summaries()]
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
#' @return A `f_str` object
#' @export
#' @md
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
#' f_str("xx (XXX.x%)", n, pct)
#'
#' f_str("a.a+1 (A.a+2)", mean, sd)
#'
f_str <- function(format_string, ..., empty=c(.overall='')) {

  # Capture the variables off of the ellipsis
  vars <- enexprs(...)
  # Null out the names for vars
  names(vars) <- NULL

  # Check format string class
  assert_has_class(format_string, "character")

  # Do a pre-check of the format string to catch invalid auto specifications
  if (str_detect(format_string, "AA|aa")) {
    stop(paste0("In f_str(), only use a single 'A' or 'a' on the integer or",
                " decimal side to trigger auto precision."), call.=TRUE)
  }

  # Parse out the format string sections
  rx <- get_format_string_regex()
  formats <- str_extract_all(format_string, rx)[[1]]

  # Duplicate any '%' to escape them
  format_string_1 <- str_replace_all(format_string, "%", "%%")

  # Make the sprintf ready string
  repl_str <- str_replace_all(format_string_1, rx, "%s")

  # Make sure that if two formats were found, two variables exist
  assert_that(length(formats) == length(vars),
              msg = paste0("In `f_str` ", length(formats), " formats were entered in the format string ",
                           format_string, "but ", length(vars), " variables were assigned."))

  # Pull out the integer and decimal
  settings <- map(formats, gather_settings)

  # A value in settings will be <0 if it's an auto format
  auto_precision <- any(map_lgl(settings, ~ any(as.logical(.[c('auto_int', 'auto_dec')]))))
  hug_formatting <- any(map_lgl(settings, ~ !is.na(.['hug_char'])))

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
         hug_formatting = hug_formatting,
         empty=empty
    ),
    class="f_str"
  )
}

#' Gather the settings for a specific format string section
#'
#' This function will collect specific settings about a format string section,
#' including integer and decimal length, whether autos were turned on, and hug
#' character settings/
#'
#' @param x A character string representing a format string section
#'
#' @return A named list of settings
#' @noRd
gather_settings <- function(x) {

  settings <- list(
    int = 0,
    dec = 0,
    auto_int = FALSE,
    auto_dec = FALSE,
    hug_char = NA_character_
  )

  settings <- parse_hug_char(x, settings)
  settings <- separate_int_dig(x, settings)

  settings
}

#' Find if a hug character exists and attach to settings
#'
#' @param x Format string section
#' @param settings A list of settings for a format string section
#'
#' @return List of settings
#' @noRd
parse_hug_char <- function(x, settings) {

  # Find hugging
  if (str_detect(x, "X|A")) {

    # Look for characters preceding X or A that aren't X or A
    hug_char_rx <- regex("([^XA]+)[XA]")

    # Search the hug character and pull out all matches
    # x is guaranteed to be a single element vector so pull out first
    # element of the list
    hug_char_match <- str_match_all(x, hug_char_rx)[[1]]

    # If no rows, then X or A was used with no specified hug character
    if (nrow(hug_char_match) == 0) {
      stop(
        paste0("In f_str(), an 'X' or 'A' was used but no hug character ",
               "was specified, such as a parenthesis. Use 'X' or 'A' to bind ",
               "a character within a format string."),
        call.=FALSE
      )
    }

    # The match matrix can't be more than one row. If it is, it was probably
    # because X or A were placed before and after a decimal, so show the user
    if (nrow(hug_char_match) > 1) {
      err_msg <- paste0(
        "In f_str(), invalid format string specification. The following section",
        " failed to parse:\n\t'", x,
        "'\nThe issue is present with a hug character. Was 'X' or 'A' used after",
        " a decimal?"
      )
      stop(err_msg, call.=FALSE)
    }

    # If X or A was used after the decimal at all, that's also invalid so error
    # out as well
    if (str_detect(hug_char_match[1,1], fixed("."))) {
      stop(
        paste0("In f_str(), 'X' or 'A' can only be used on the left side of a",
               " decimal within a format string."),
        call.=FALSE
      )
    }

    # The hug char is in a capture group, so we pull it out of the match
    settings$hug_char <- hug_char_match[1,2]
  }

  settings
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
  if (grepl('a|A', x)) {
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

#' Evaluate a portion of a format string to check the integer and digit lengths
#'
#' @param x Format string section
#' @param settings A list of settings for a format string section
#'
#' @return List of settings
#'
#' @noRd
separate_int_dig <- function(x, settings){

  # Count the characters on each side of the decimal
  fields <- str_split(x, "\\.")[[1]]
  # Label the split segments
  names(fields) <- c('int', 'dec')[1:length(fields)]

  # Parse out length and auto info from each field and apply to settings
  num_chars <- map(fields, parse_fmt)
  auto <- map_lgl(num_chars, ~attr(.x, 'auto'))

  settings[names(num_chars)] <- as.numeric(num_chars)
  settings[paste0("auto_", names(auto))] <- auto

  # If a hug character is specified,subtract if from the integer length
  if (!is.na(settings$hug_char) && settings$auto_int) {
    settings$int <- settings$int + (nchar(settings$hug_char) - 1)
  }

  settings
}

#' Helper for changing values on count f_str
#'
#' @param ... The object passed to `set_format_strings` or `set_count_layer_formats`
#'
#' @noRd
count_f_str_check <- function(...) {
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
      assert_that(all(params[['n_counts']]$vars %in% c("n", "pct", "distinct", "distinct_n", "distinct_pct", "total", "distinct_total")),
                  msg = "f_str for n_counts in a count_layer can only be n, pct, distinct, distinct_pct, total, or distinct_total")

      # Check to make sure both disintct(old), and distinct_n(new) aren't passed
      assert_that(!all(c("distinct", "distinct_n") %in% params[["n_counts"]]$vars),
                  msg = "You can't pass both distinct and distinct_n, just use distinct_n")

      # Check to make sure duplicated parameters aren't passed
      assert_that(length(params[["n_counts"]]$vars) == length(unique(params[["n_counts"]]$vars)),
                  msg = "You've passed duplicate parameters to `set_format_strings`")

      # Replace the disinct with distinct_n
      if (any(params[["n_counts"]]$vars %in% "distinct")) {
        warning("The use of 'distinct' in count f_strs is discouraged. It was replaced with 'distinct_n' for consistancy.")
      }
      params[["n_counts"]]$vars[params[["n_counts"]]$vars %in% "distinct"] <- "distinct_n"

    } else if (name == "riskdiff") {
      assert_that(all(params[['riskdiff']]$vars %in% c('comp', 'ref', 'dif', 'low', 'high')),
                  msg = "f_str for riskdiff in a count_layer can only be comp, ref, dif, low, or high")
    }
  }

  params
}

