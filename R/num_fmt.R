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

  # Auto precision requires that integer and decimal are pulled from the row. If
  # auto, settings will be the amount to add to max prec, so add those together.
  # Otherwise pull the manually specified value
  int_len <- ifelse(fmt$setting[[i]]$auto_int,
                    fmt$setting[[i]]$int + autos['int'],
                    fmt$setting[[i]]$int)

  decimals  <- ifelse(fmt$setting[[i]]$auto_dec,
                      fmt$setting[[i]]$dec + autos['dec'],
                      fmt$setting[[i]]$dec)

  # Set nsmall to input decimals
  nsmall <- decimals

  # Increment digits for to compensate for display
  if (decimals > 0) decimals <- decimals + 1

  # Empty return string
  if (is.na(val)) {if (is.na(fmt$settings[[i]]$hug_char)) {
    return(str_pad(fmt$empty[1], int_len+decimals, side="left"))
  } else{
    return(
      str_pad(
        paste0(fmt$settings[[i]]$hug_char, fmt$empty[1]),
        int_len+decimals,
        side="left")
    )
  }

  }

  # Use two different rounding methods based on if someone is matching with IBM rounding
  if(getOption("tplyr.IBMRounding", FALSE)) {
    warn(paste0(c("You have enabled IBM Rounding. This is an experimental feature.",
                  " If you have feedback please get in touch with the maintainers!")),
         .frequency = "regularly", .frequency_id = "tplyr.ibm", immediate. = TRUE)
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
      width=(int_len+decimals),
      # Decimals to display
      nsmall=nsmall
    )
  } else {
    fmt_num <- str_pad(
      paste0(
        #Paste the hug character
        fmt$settings[[i]]$hug_char,
        format(
          rounded,
          nsmall=nsmall
        )
      ),
      width=(int_len+decimals)
    )
  }

  fmt_num
}

#' Pad Numeric Values
#'
#' This is generally used with a count layer
#'
#' @param x The current values of the numeric data
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
