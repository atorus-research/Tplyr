#' Retrieve one of Tplyr's regular expressions
#'
#' This function allows you to extract important regular expressions used inside
#' Tplyr.
#'
#' There are two important regular expressions used within Tplyr. The
#' format_string expression is the expression to parse format strings. This is
#' what is used to make sense out of strings like 'xx (XX.x%)' or 'a+1 (A.a+2)'
#' by inferring what the user is specifying about number formatting.
#'
#' The 'format_group' regex is the opposite of this, and when given a string of
#' numbers, such as ' 5 (34%) \[9]' will return the separate segments of numbers
#' broken into their format groups, which in this example would be ' 5',
#' '(34%)', and '\[9]'.
#'
#' @param rx A character string with either the value 'format_string' or
#'   'format_group'
#'
#' @return A regular expression object
#' @export
#' @md
#'
#' @examples
#'
#' get_tplyr_regex('format_string')
#'
#' get_tplyr_regex('format_group')
#'
get_tplyr_regex <- function(rx=c("format_string", "format_group")) {
  rx <- match.arg(rx)

  switch(
    rx,
    'format_string' = get_format_string_regex(),
    'format_group' = get_format_group_regex()
  )
}

#' Generate the format string parsing regular expression
#'
#' @return A regular expression object with the compiled expression
#'
#' @noRd
get_format_string_regex <- function() {

  # On the integer side, find an a that may be followed by a + and a number
  # so this could look like a or a+1, a+2, etc.
  int_auto <- "a(\\+\\d+)?"

  # Same as above, but look for an A and a non-whitespace character preceding
  # the A
  int_auto_hug <- "(\\S+)A(\\+\\d+)?"

  # Look for one or more X's, with a non-whitespace character preceding
  int_fixed_hug <- "(\\S+)X+"

  # Look for one or more x's
  int_fixed <- "x+"

  # Look for an A or a that may be followed by a + and a number
  # so this could look like a or a+1, a+2, etc.
  # A's will be invalid here but that will be caught by error checking
  # in parse_hug_char()
  dec_auto <- "[A|a](\\+\\d+)?"

  # One or more X or x - again X is invalid but caught later
  dec_fixed <- "[X|x]+"

  # Now prepare to piece the chunks together - all of the int side pieces are
  # combined with "or's". The decimal side comes after that, and this specifies
  # that it will find them if they exist, but the integer side will be found
  # even if they don't
  joined_string <- "(%s|%s|%s|%s)(\\.(%s|%s)?)?"

  # Concatenate it all together and convert it to a regex
  regex(
    sprintf(
      joined_string,
      int_auto,
      int_auto_hug,
      int_fixed_hug,
      int_fixed,
      dec_auto,
      dec_fixed
    )
  )
}

#' Return the regex for identifying format groups in populated strings
#'
#' This regex is the reverse of the f_str() regex, and is used to find populated
#' format groups with real numbers rather than mock formatting
#'
#' @return A regular expression
#' @noRd
get_format_group_regex <- function() {

  # 0 or more non-whitespace or non-digit character
  nwsd <- "[^\\s\\d]*"

  # 0 or more whitespace
  ws <- "\\s*"

  # Positive or negative integer or decimal
  num <- "(\\-?\\d+(\\.\\d+)?)"

  # 0 or more non-whitespace
  nws <- "\\S*"

  regex(paste0(nwsd, ws, num, nws))

}
