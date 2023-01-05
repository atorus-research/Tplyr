#' Extract format group strings or numbers
#'
#' These functions allow you to extract segments of information from within a
#' result string by targetting specific format groups. `str_extract_fmt_group()`
#' allows you to pull out the individual format group string, while
#' `str_extract_num()` allows you to pull out that specific numeric result.
#'
#' Format groups refer to individual segments of a string. For example, given
#' the string ' 5 (34.4%) \[9]', there are three separate format groups, which
#' are ' 5', '(34.4%)', and '\[9]'.
#'
#' @param string A string of number results from which to extract format groups
#' @param format_group An integer representing format group that should be
#'   extracted
#'
#' @family String extractors
#' @rdname str_extractors
#'
#' @return A character vector
#' @export
#' @md
#'
#' @examples
#'
#' string <- c(" 0  (0.0%)", " 8  (9.3%)", "78 (90.7%)")
#'
#' str_extract_fmt_group(string, 2)
#'
#' str_extract_num(string, 2)
#'
str_extract_fmt_group <- function(string, format_group) {

  if (!inherits(string, "character")) {
    stop("Paramter `string` must be a character vector", call.=FALSE)
  }

  if (!inherits(format_group, "numeric") || (inherits(format_group, "numeric") && format_group %% 1 != 0)) {
    stop("Paramter `format_group` must be an integer", call.=FALSE)
  }

  # Pull out regex to drive the work
  f_grp_rx <- get_format_group_regex()

  # Pull out all the match groups and then get the numeric for the conditional number
  match_groups <- str_extract_all(string, f_grp_rx)

  # Get string broken out from groups
  map_chr(
    match_groups,
    ~ if (length(.) < format_group) {NA_character_} else {.[format_group]}
  )
}

#' @family String extractors
#' @rdname str_extractors
#' @export
str_extract_num <- function(string, format_group) {

  if (!inherits(string, "character")) {
    stop("Paramter `string` must be a character vector", call.=FALSE)
  }

  if (!inherits(format_group, "numeric") || (inherits(format_group, "numeric") && format_group %% 1 != 0)) {
    stop("Paramter `format_group` must be an integer", call.=FALSE)
  }

  # Pull out regex to drive the work
  f_grp_rx <- get_format_group_regex()

  # Pull out all the match groups and then get the numeric for the conditional number
  match_groups <- str_match_all(string, f_grp_rx)

  # Get the number upon which the condition will be evaluated
  map_dbl(
    match_groups,
    ~ if (nrow(.) < format_group) {NA_real_} else {as.double(.[format_group, 2])}
  )
}
