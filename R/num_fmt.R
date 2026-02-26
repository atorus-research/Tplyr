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
num_fmt <- function(val, i, fmt = NULL, autos = NULL) {
  # Auto precision requires that integer and decimal are pulled from the row. If
  # auto, settings will be the amount to add to max prec, so add those together.
  # Otherwise pull the manually specified value
  int_len <- ifelse(fmt$setting[[i]]$auto_int,
    fmt$setting[[i]]$int + autos["int"],
    fmt$setting[[i]]$int
  )

  decimals <- ifelse(fmt$setting[[i]]$auto_dec,
    fmt$setting[[i]]$dec + autos["dec"],
    fmt$setting[[i]]$dec
  )

  # Set nsmall to input decimals
  nsmall <- decimals

  # Increment digits for to compensate for display
  if (decimals > 0) decimals <- decimals + 1

  # Empty return string
  if (is.na(val)) {
    if (is.na(fmt$settings[[i]]$hug_char)) {
      return(stri_pad_left(fmt$empty[1], int_len + decimals))
    } else {
      return(
        stri_pad_left(
          paste0(fmt$settings[[i]]$hug_char, fmt$empty[1]),
          int_len + decimals
        )
      )
    }
  }

  # Use two different rounding methods based on if someone is matching with IBM rounding
  if (getOption("tplyr.IBMRounding", FALSE)) {
    warn(
      paste0(c(
        "You have enabled IBM Rounding. This is an experimental feature.",
        " If you have feedback please get in touch with the maintainers!"
      )),
      .frequency = "regularly", .frequency_id = "tplyr.ibm", immediate. = TRUE
    )
    rounded <- ut_round(val, nsmall)
  } else {
    rounded <- round(val, nsmall)
  }

  # Form the string
  if (is.na(fmt$settings[[i]]$hug_char)) {
    fmt_num <- format(
      # Round
      rounded,
      # Set width of format string
      width = (int_len + decimals),
      # Decimals to display
      nsmall = nsmall
    )
  } else {
    fmt_num <- stri_pad_left(
      paste0(
        # Paste the hug character
        fmt$settings[[i]]$hug_char,
        format(
          rounded,
          nsmall = nsmall
        )
      ),
      width = (int_len + decimals)
    )
  }

  fmt_num
}

#' Vectorized number formatting for count layers
#'
#' Formats an entire numeric vector using f_str settings. This is a vectorized
#' replacement for the map_chr(values, num_fmt, ...) pattern used in count
#' string construction.
#'
#' Note: This function does not support auto-precision (a/A format specifiers)
#' as count layers use fixed precision. For auto-precision support, use the
#' scalar num_fmt() function.
#'
#' @param vals Numeric vector to format
#' @param i Index of the format setting within the f_str object
#' @param fmt f_str object with formatting information
#'
#' @return Character vector of formatted values
#' @noRd
num_fmt_vec <- function(vals, i, fmt) {
  # Extract format settings once for the entire vector
  int_len <- fmt$settings[[i]]$int
  decimals <- fmt$settings[[i]]$dec
  hug_char <- fmt$settings[[i]]$hug_char
  nsmall <- decimals

  # Calculate display width (add 1 for decimal point if decimals > 0)
  width <- int_len + ifelse(decimals > 0, decimals + 1, decimals)

  # Handle IBM rounding option
  if (getOption("tplyr.IBMRounding", FALSE)) {
    warn(
      paste0(c(
        "You have enabled IBM Rounding. This is an experimental feature.",
        " If you have feedback please get in touch with the maintainers!"
      )),
      .frequency = "regularly", .frequency_id = "tplyr.ibm", immediate. = TRUE
    )
    rounded <- ut_round(vals, nsmall)
  } else {
    rounded <- round(vals, nsmall)
  }

  # Use sprintf for exact width control - format() when vectorized expands

  # all values to match the widest one, but we need each value formatted
  # to exactly the specified width independently
  fmt_string <- sprintf("%%%d.%df", width, nsmall)

  # Vectorized formatting with sprintf
  if (is.na(hug_char)) {
    # Standard formatting
    fmt_nums <- sprintf(fmt_string, rounded)
  } else {
    # Hug character formatting - paste hug char then pad to width
    # First format without width, then add hug char and pad
    fmt_string_no_width <- sprintf("%%.%df", nsmall)
    fmt_nums <- stri_pad_left(
      paste0(hug_char, sprintf(fmt_string_no_width, rounded)),
      width = width
    )
  }

  # Handle NA values vectorized
  na_mask <- is.na(vals)
  if (any(na_mask)) {
    empty_str <- fmt$empty[1]
    if (is.na(hug_char)) {
      na_replacement <- stri_pad_left(empty_str, width)
    } else {
      na_replacement <- stri_pad_left(paste0(hug_char, empty_str), width)
    }
    fmt_nums[na_mask] <- na_replacement
  }

  fmt_nums
}

#' Vectorized number formatting with auto-precision support for desc layers
#'
#' Formats an entire numeric vector using f_str settings, with support for
#' auto-precision (a/A format specifiers). This extends num_fmt_vec for use
#' in desc layers where precision can be determined from the data.
#'
#' @param vals Numeric vector to format
#' @param i Index of the format setting within the f_str object
#' @param fmt f_str object with formatting information
#' @param max_int Numeric vector or scalar for auto integer precision (from data)
#' @param max_dec Numeric vector or scalar for auto decimal precision (from data)
#'
#' @return Character vector of formatted values
#' @noRd
num_fmt_vec_auto <- function(vals, i, fmt, max_int = 0, max_dec = 0) {
  settings <- fmt$settings[[i]]

  # Use first element if vectors were passed (precision should be consistent within a group)
  max_int <- if (length(max_int) > 1) max_int[1] else max_int
  max_dec <- if (length(max_dec) > 1) max_dec[1] else max_dec

  # Calculate integer length - use auto if specified
  if (settings$auto_int) {
    int_len <- settings$int + max_int
  } else {
    int_len <- settings$int
  }

  # Calculate decimal places - use auto if specified
  if (settings$auto_dec) {
    decimals <- settings$dec + max_dec
  } else {
    decimals <- settings$dec
  }

  hug_char <- settings$hug_char
  nsmall <- decimals

  # Calculate display width (add 1 for decimal point if decimals > 0)
  width <- int_len + ifelse(decimals > 0, decimals + 1, decimals)

  # Handle IBM rounding option
  if (getOption("tplyr.IBMRounding", FALSE)) {
    warn(
      paste0(c(
        "You have enabled IBM Rounding. This is an experimental feature.",
        " If you have feedback please get in touch with the maintainers!"
      )),
      .frequency = "regularly", .frequency_id = "tplyr.ibm", immediate. = TRUE
    )
    rounded <- ut_round(vals, nsmall)
  } else {
    rounded <- round(vals, nsmall)
  }

  # Build format string
  fmt_string <- sprintf("%%%d.%df", width, nsmall)

  # Vectorized formatting with sprintf
  if (is.na(hug_char)) {
    fmt_nums <- sprintf(fmt_string, rounded)
  } else {
    # Hug character formatting
    fmt_string_no_width <- sprintf("%%.%df", nsmall)
    fmt_nums <- stri_pad_left(
      paste0(hug_char, sprintf(fmt_string_no_width, rounded)),
      width = width
    )
  }

  # Handle NA values vectorized
  na_mask <- is.na(vals)
  if (any(na_mask)) {
    empty_str <- fmt$empty[1]
    if (is.na(hug_char)) {
      na_replacement <- stri_pad_left(empty_str, width)
    } else {
      na_replacement <- stri_pad_left(paste0(hug_char, empty_str), width)
    }
    fmt_nums[na_mask] <- na_replacement
  }

  fmt_nums
}

#' Pad Numeric Values
#'
#' This is generally used with a count layer. Uses vectorized str_pad
#' for efficient padding of character vectors.
#'
#' @param x The current values of the numeric data
#' @param right_pad The total string length, done after the left pad
#' @param left_pad The length of the left_pad
#'
#' @return Modified string
#'
#' @noRd
pad_formatted_data <- function(x, right_pad, left_pad) {
  # Handle empty input
  if (length(x) == 0) {
    return(x)
  }

  # Vectorized left padding
  if (!is.na(nchar(x[1])) && nchar(x[1]) < left_pad) {
    x <- stri_pad_left(x, left_pad)
  }

  # Vectorized right padding
  max_nchar <- max(nchar(x), na.rm = TRUE)
  if (!is.na(max_nchar) && right_pad > max_nchar) {
    x <- stringi::stri_pad_right(x, right_pad)
  }

  x
}
