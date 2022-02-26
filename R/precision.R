#' Get the lengths of either the right or left side of a decimal
#'
#' @param v A vector of character strings
#' @param side 1 = Integer, 2 = Decimal
#'
#' @return A vector of lengths for the specified field
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
#' @param cap Capped precision passed in from set_format_strings
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

#' Check and return the provided precision lookup table
#'
#' This function is called to extract the precision data if the user manually
#' provided a precision look up table. This functions assumes that
#' set_precision_data has been run on the layer.
#'
#' If the user specified default='auto', then the prec_error parameter in the layer
#' metadata is set to auto. When set to 'error', the table build will error if there
#' are by variable cases found for which the user did not provide precision. When set
#' to 'auto', these cases will be passed into make_prec_data, which will auto-calculate
#' the precision based on the built_target data.
#'
#' @param built_target Data precision is calculated from
#' @param prec External precision dataset
#' @param precision_by Precision by variables - pulled from the provided precision data
#' @param precision_on Precision on variable - defaulted to first target_var variable
#' @param cap Capped precision passed in from set_format_strings
#' @param prec_error How should unspecified cases be handled?
#'
#' @return A tibble look-up table with the precision_by variables, a variable for the
#' maximum integer length (max_int), and the maximum decimal length (max_dec).
#'
#' @return A tibble look-up table with the precision_by variables, a variable for the
#' maximum integer length (max_int), and the maximum decimal length (max_dec).
#' @noRd
get_prec_data <- function(built_target, prec, precision_by, precision_on, cap, prec_error) {

  # Do the types match between the prec data and the built target?
  prec_types <- map_chr(precision_by, ~ class(prec[[as_label(.)]]))
  data_types <- map_chr(precision_by, ~ class(built_target[[as_label(.)]]))

  assert_that(
    all(prec_types == data_types),
    msg = "By variable types mismatch between precision dataset and target data"
  )

  # What's in the data?
  precision_by_cases <- built_target %>%
    distinct(!!!precision_by)

  # What's missing from the provided table?
  mismatches <- anti_join(precision_by_cases, prec, by = map_chr(precision_by, as_label))

  if (prec_error == "error" && nrow(mismatches) > 0) {
    stop('The precision data provided is missing by variable cases:\n',
         paste(capture.output(print(mismatches)), collapse = "\n"),
         call. = FALSE)
  } else if (prec_error == "auto" && nrow(mismatches) > 0) {
    message('Unhandled precision cases were found - calculating precision based on source data')
    subset_target <- left_join(mismatches, built_target, by = map_chr(precision_by, as_label))
    auto_prec <- make_prec_data(subset_target, precision_by, precision_on, cap)
    prec <- bind_rows(prec, auto_prec)
  }

  prec_on <- as_label(precision_on)

  prec %>%
    mutate(
      precision_on = as_name(prec_on)
    )
}
