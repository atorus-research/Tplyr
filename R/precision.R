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

#' Make precision look-up table
#'
#' Creates the look up table based on precision_by and precision_on bindings
#'
#' @param .data Data precision is calculated from
#' @param precision_by Precision by variables - defaulted to the layer by
#' @param precision_on Precision on variable - defaulted to first target_var variable
#'
#' @return A tibble look-up table with the precision_by variables, a variable for the
#' maximum integer length (max_int), and the maximum decimal length (max_dec).
#'
#' @noRd
make_prec_data <- function(.data, precision_by, precision_on, cap) {
  .data %>%
    group_by(!!!precision_by) %>%
    # Grab the maximum level of collected precision within the precision by group
    summarize(
      # We want the minimum of either the max collected precision, or the cap
      max_int = min(max(nchar_unit(!!precision_on, 1)), cap['int']),
      max_dec = min(max(nchar_unit(!!precision_on, 2)), cap['dec'])
    ) %>%
    ungroup() %>%
    mutate(
      precision_on = as_label(precision_on)
    )
}
