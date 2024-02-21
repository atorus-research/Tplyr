#' Validator for apply_conditional_format
#'
#' @param string Target character vector where text may be replaced
#' @param format_group An integer representing the targeted numeric field within
#'   the string, numbered from left to right
#' @param condition An expression, using the variable name 'x' as the target
#'   variable within the condition
#' @param replacement A string to use as the replacement value
#' @param full_string TRUE if the full string should be replaced, FALSE if the
#'   replacement should be done within the format group
#' @noRd
validate_conditional_format_params <- function(string, format_group, condition, replacement, full_string) {
  if (!inherits(string, "character")) {
    stop("Parameter `string` must be a character vector", call.=FALSE)
  }

  if (!inherits(format_group, "numeric") || (inherits(format_group, "numeric") && format_group %% 1 != 0)) {
    stop("Parameter `format_group` must be an integer", call.=FALSE)
  }

  if (!inherits(replacement, "character")) {
    stop("Parameter `replacement` must be a string", call.=FALSE)
  }

  # Condition statement must use the variable name 'x'
  if (!identical(all.vars(condition), "x")) {
    stop("Condition must be a valid expression only using the variable name `x`", call.=FALSE)
  }

  if (!inherits(full_string, "logical")) {
    stop("Parameter `full_string` must be bool", call.=FALSE)
  }

}

#' Conditional reformatting of a pre-populated string of numbers
#'
#' This function allows you to conditionally re-format a string of numbers based
#' on a numeric value within the string itself. By selecting a "format group",
#' which is targeting a specific number within the string, a user can establish
#' a condition upon which a provided replacement string can be used. Either the
#' entire replacement can be used to replace the entire string, or the
#' replacement text can refill the "format group" while preserving the original
#' width and alignment of the target string.
#'
#' @param string Target character vector where text may be replaced
#' @param format_group An integer representing the targeted numeric field within
#'   the string, numbered from left to right
#' @param condition An expression, using the variable name 'x' as the target
#'   variable within the condition
#' @param replacement A string to use as the replacement value
#' @param full_string TRUE if the full string should be replaced, FALSE if the
#'   replacement should be done within the format group
#'
#' @return A character vector
#' @export
#'
#' @examples
#'
#' string <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)")
#'
#' apply_conditional_format(string, 2, x == 0, " 0        ", full_string=TRUE)
#'
#' apply_conditional_format(string, 2, x < 1, "(<1%)")
#'
apply_conditional_format <- function(string, format_group, condition, replacement, full_string=FALSE) {

  condition <- enexpr(condition)

  # Validate all parameters
  validate_conditional_format_params(string, format_group, condition, replacement, full_string)

  # Pull out regex to drive the work
  f_grp_rx <- get_format_group_regex()

  # Pull out all the match groups and then get the numeric for the conditional number
  match_groups <- str_match_all(string, f_grp_rx)

  # Get the number upon which the condition will be evaluated
  x <- map_dbl(
    match_groups,
    ~ if (nrow(.) < format_group) {NA_real_} else {as.double(.[format_group, 2])}
    )

    # Get the bool vector for where strings should be replaced and handle NAs
  tf <- replace_na(eval(condition), FALSE)

  if (full_string) {
    out_string <- if_else(tf, replacement, string)
  } else {
    # Grab the match locations to use for sub stringing
    match_locs <- str_locate_all(string, f_grp_rx)
    # Get the group length out to ensure that the string is fully padded
    group_length <- map_int(
      match_groups,
      ~ if (nrow(.) < format_group) {NA_integer_} else {as.integer(nchar(.[format_group, 1]))}
      )

    if (any(nchar(replacement) > group_length[!is.na(group_length)])) {
      warning(
        paste0("Replacement string length is longer that some string's format group length.",
               "Some alignment will not be preserved")
      )
    }

    # Pad at least as long as the format group space
    pad_length <- map_int(
      group_length,
      ~ if_else(nchar(replacement) > ., nchar(replacement), .)
    )

    # Pull out locs for the format group
    end_locs <- map_int(
      match_locs,
      ~ if (nrow(.) < format_group) {NA_integer_} else {.[format_group, 'end']}
      )
    start_locs <- end_locs - pad_length + 1

    # Build the sub string matrix
    sub_mat <- matrix(c(rbind(start_locs, end_locs)), ncol=2, byrow=TRUE)

    # Generate a vector with replacements already done
    rep_string <- string
    str_sub(rep_string, sub_mat) <- str_pad(replacement, pad_length)

    out_string <- if_else(tf, rep_string, string)
  }

  out_string
}

