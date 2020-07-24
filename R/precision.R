#' Get the lengths of either the right or left side of a decimal
#'
#' @param v A vector of character strings
#' @param side 1 = Integer, 2 = Decimal
#'
#' @return A vector of lenths for the specified field
#' @noRd
nchar_unit <- function(v, side) {

  # Prepare the output vector
  out <- double(length(v))

  # Convert to character upfront
  v <- as.character(replace_na(v, 0))

  # Split the strings and count characters
  splits <- nchar(str_split(v, '\\.', simplify=TRUE))

  # If the group had no decimals, there will be no right side of the decimals
  if (!(side == 2 && dim(splits)[2] == 1)) {
    out <- splits[,side]
  }
  out
}

make_prec_data <- function(.data, precision_by, precision_on) {
  .data %>%
    group_by(!!!precision_by) %>%
    summarize(
      max_int = max(nchar_unit(AVAL, 1)),
      max_dec = max(nchar_unit(AVAL, 2))
    ) %>%
    select(PARAMCD, max_int, max_dec)
}
