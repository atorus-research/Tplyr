string <- "row label 1 {test | test2} | row label 2 | Spanning header {Inside header1 | Inside header 2} | Spanning header 2 {Inside header1 | Inside header 2} | last item"


#' Title
#'
#' @param s
#'
#' @return
#' @export
#'
#' @examples
make_column_headers <- function(s) {

  # Query everything needed from teh string
  string_segments <- identify_string_segments(s)

  validate_header_string(s, string_segments)
}

#' Title
#'
#' @param s
#' @param string_segments
#'
#' @return
#' @export
#'
#' @examples
validate_header_string <- function(s, string_segments) {

  list2env(string_segments, environment())

  # Make sure that brackets are all balanced
  if (length(op) != length(cl)) {
    abort("Unmatched brackets for spanning headers")
  }

  # Check for nesting
  for (i in seq_along(op)) {
    # Check that the next opening bracket isn't before the current closing (i.e. no nesting)
    if (i+1 <= length(op) && (cl[i] > op[i + 1])) {
      abort("Nested spanning headers are not yet supported")
    }
  }

}

#' Title
#'
#' @param s
#'
#' @return
#' @export
#'
#' @examples
identify_string_segments <- function(s) {

  # Find the inside groups
  op <- str_locate_all(string, fixed("{"))[[1]][,1] # Opening brackets
  cl <- str_locate_all(string, fixed("}"))[[1]][,1] # Closing brackets

  # Get the spanner locations and spanner sections
  spanner_locs <- str_locate_all(string, "[^{\\|]*\\{")[[1]] # Text from | to {
  spanned_sects <- str_locate_all(string,"[^\\{\\|]*\\{[^\\}]*\\}")[[1]] # The above but with the text between {}

  # Place it all in a list
  string_segments <- list(op = op, cl = cl, spanner_locs = spanner_locs, spanned_sects = spanned_sects)

  # Return the object with a class and `spanners` attribute specifying if there are spanners
  structure(
    string_segments,
    class="header_locs",
    spanners = op > 0
    )
}

#' Title
#'
#' @param s
#' @param string_segments
#'
#' @return
#' @export
#'
#' @examples
get_spanner_row <- function(s, string_segments) {
  spanning_row <- character(nrow(spanner_locs) + 1)

  for (i in seq_along(string_segments$op)) {
    # Get the locations
    top_locs <-string_segments$spanner_locs[i, ]

    # Grab part of the string to count the | from (i.e. the end of the last section)
    top_start <- ifelse(i == 1, 1, spanner_locs[i-1, ]['end'])

    # Count the bars that need to be prefixed to this part of the spanner to split up cells
    bars <- str_count(str_sub(s, top_start, top_locs['end']), fixed("|"))

    # Prefix with | to pad how many cells are before the text and commit into spanning vector
    spanning_row[[i]] <- paste(c(rep("|", bars), top_txt), collapse="")
  }

  # Add in the final number of bars after the last spanning header
  bars <- str_count(str_sub(s, top_locs['end']), fixed("|"))
  spanning_row[[i + 1]] <- paste(c(rep("|", bars)), collapse="")

  spanner_row_text <- paste(spanning_row, collapse="")
  spanner_row_text

}

#' Title
#'
#' @param s
#' @param string_segments
#'
#' @return
#' @export
#'
#' @examples
get_bottom_row <- function(s, string_segments) {

  ## TODO: This is broken.
  bottom_row <- character(nrow(spanner_locs) + 1)

  for (i in seq_along(string_segments$op)) {
    # Get the spanned section locations
    sect_locs <- string_segments$spanned_sects[i ,]

    # Grab part of the string to count the | from (i.e. the end of the last section)
    sect_txt <- str_trim(str_sub(s, sect_locs['start'], sect_locs['end']))

    # Get the text between the brackets
    spanned[[i]] <- str_sub(s, start=op[i]+1, end=cl[i]-1)
  }

  # Last elements of the bottom row
  bottom_row[[i+1]] <- str_sub(s, sect_locs['end']+2)

  # Build the final text string
  bottom_row_text <- paste(map_chr(bottom_row, trim_bars), collapse="|")
  bottom_row_text
}


#' Title
#'
#' @param string_segments
#'
#' @return
#' @export
#'
#' @examples
has_spanners <- function(string_segments) {
  attr(string_segments, 'spanners')
}



#' Trim bars from the start of end of a string
#'
#' Intended for use in the column headers prep
#'
#' @param s A string to trim bars off of
#'
#' @return The trimmed string
trim_bars <- function(s) {
  # If a bar is the first character, strip it
  if (str_sub(s, 1, 1) == "|") {
    s <- str_sub(s, 2)
  }
  # If a bar is the last character, strip it
  if (str_sub(s, nchar(s)) == "|") {
    s <- str_sub(s,1,nchar(s)-1)
  }
  s
}


#' Title
#'
#' @param s
#' @param names
#'
#' @return
#' @export
#'
#' @examples
make_header_row <- function(s, names) {

  # Split
  row <- as_tibble(as.list(str_split(s, fixed("|"))[[1]]), .name_repair='minimal')
  names(row) <- names
  row

}

#' Title
#'
#' @param st
#' @param bt
#' @param name_from
#'
#' @return
#' @export
#'
#' @examples
make_column_df <- function(st, bt, name_from) {
  assert_that(str_count(st, fixed("|")) == str_count(bt, fixed("|")),
              msg = "Malformed column header string - cells in spanner don't match cells in bottom row")

  rows <- map(list(st, bt), make_header_row, names=names(name_from))

  bind_rows(rows)
}













foo <- tibble(one=character(5),
              two=character(5),
              three=character(5),
              four=character(5),
              five=character(5),
              six=character(5),
              seven=character(5),
              eight=character(5))









for (i in 1:nrow(spanner_locs)) {
  # Pick out the location info
  top_locs <-spanner_locs[i, ]
  sect_locs <- spanned_sects[i ,]

  # Grab part of the string to count the | from (i.e. the end of the last section)
  top_start <- ifelse(i == 1, 1, spanner_locs[i-1, ]['end'])
  sect_start <- ifelse(i == 1, 1, spanned_sects[i-1, ]['end']+2)

  # Grab the text of the spanner header
  top_txt <- str_trim(str_sub(string, top_locs['start'], top_locs['end']-1))
  sect_txt <- str_trim(str_sub(string, sect_locs['start'], sect_locs['end']))

  # Count the bars that need to be prefixed to this part of the spanner to split up cells
  bars <- str_count(str_sub(string, top_start, top_locs['end']), fixed("|"))


  # Prefix with | to pad how many cells are before the text and commit into spanning vector
  spanning_row[[i]] <- paste(c(rep("|", bars), top_txt), collapse="")

  # Get the text between the brackets
  spanned[[i]] <- str_sub(string, start=op[i]+1, end=cl[i]-1)

  # Build up the bottom row vector
  bottom_row[[i]] <- str_replace(str_sub(string, sect_start, sect_locs['end']), fixed(sect_txt), spanned[[i]])

}
