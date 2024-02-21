#' Reformat strings with leading whitespace for HTML
#'
#' @param x Target string
#' @param tab_width Number of spaces to compensate for tabs
#'
#' @return String with &nbsp; replaced for leading whitespace
#' @export
#'
#' @examples
#' x <- c(" Hello there", "  Goodbye Friend ",  "\tNice to meet you",
#' "  \t What are you up to? \t \t ")
#' replace_leading_whitespace(x)
#'
#' replace_leading_whitespace(x, tab=2)
#'
replace_leading_whitespace <- function(x, tab_width=4) {
  # Pull out the leading whitespace chunk
  leading_spaces <- str_match(x, "^([ \\t])+")[,1]
  # Count spaces and tabs, factor in tab width
  spaces <- str_count(leading_spaces, pattern = " ")
  tabs <- str_count(leading_spaces, pattern = "\\t") * tab_width
  leading_length <- as.integer(spaces + tabs)

  # Build the &nbsp; string and combine with the trimmed string
  nbsp_string <- map_chr(leading_length, \(.x) {
    if (!is.na(.x)) {
      paste(rep("&nbsp;", .x), collapse="")
    } else {
      ""
    }})
  minus_whitespace <- str_trim(x, side='left')
  paste(nbsp_string, minus_whitespace, sep="")
}

