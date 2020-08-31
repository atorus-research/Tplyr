#' Attach column headers to a Tplyr output
#'
#' When working with 'huxtable' tables, column headers can be controlled as if they are rows in the data frame.
#' \code{add_column_headers} eases the process of introducing these headers.
#'
#' Headers are created by providing a single string. Columns are specified by delimitting each header with a '|' symbol.
#' Instead of specifying the destination of each header, \code{add_column_headers} assumes that you have organized the columns
#' of your data frame before hand. This means that after you use \code{Tplyr::build()}, if you'd like to reorganize the
#' default column order (which is simply alphabetical), simply pass the build output to a \code{dplyr::select} or \code{dplyr::relocate}
#' statement before passing into \code{add_column_headers}.
#'
#' Spanning headers are also supported. A spanning header is an overarching header that sits across multiple columns.
#' Spanning headers are introduced to \code{add_column_header} by providing the spanner text (i.e. the text that
#' you'd like to sit in the top row), and then the spanned text (the bottom row) within curly brackets ('\{\}). For example,
#' take the iris dataset. We have the names:
#'
#' \code{"Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"}
#'
#' If we wanted to provide a header string for this dataset, with spanners to help with categorization of
#' the variables, we could provide the following string:
#'
#' \code{"Sepal {Length | Width} | Petal {Length | Width} | Species"}
#'
#' @section Important note:
#' Make sure you are aware of the order of your variables prior to passing in to \code{add_column_headers}. The only requirement
#' is that the number of column match. The rest is up to you.
#'
#' @section Development notes:
#' There are a few features of \code{add_column_header} that are intended but not yet supported:
#' \itemize{
#' \item{Nested spanners are not yet supported. Only a spanning row and a bottom row can currently be created}
#' \item{Different delimiters and indicators for a spanned group may be used in the future. The current choices were intuitive,
#' but based on feedback it could be determined that less common characters may be necessary.}
#' }
#'
#' @section Token Replacement:
#' This function has support for reading values from the header_n object in a Tplyr table
#' and adding them in the column headers. Note: The order of the parameters
#' passed in the token is important. They should be first the treatment variable
#' then any \code{cols} variables in the order they were passed in the table construction.
#'
#' Use a double asterisk "**" at the begining to start the token and another
#' double asterisk to close it. You can separate column parameters in the token
#' with a single underscore. For example, **group1_flag2_param3** will pull the count
#' from the header_n binding for group1 in the \code{treat_var}, flag2 in the first \code{cols}
#' argument, and param3 in the second \code{cols} argument.
#'
#' You can pass fewer arguments in the token to get the sum of multiple columns.
#' For example, **group1** would get the sum of the group1 treat_var,
#' and all cols from the header_n.
#'
#' @param s The text containing the intended header string
#' @param .data The data.frame/tibble on which the headers shall be attached
#' @param header_n A header_n or generic data.frame to use for binding count values.
#'   This is required if you are using the token replacement.
#'
#' @return A data.frame with the processed header string elements attached as the top rows
#' @export
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#' library(dplyr)
#' header_string <- "Sepal {Length | Width} | Petal {Length | Width} | Species"
#'
#' iris2 <- iris %>%
#'   mutate_all(as.character)
#'
#' iris2 %>% add_column_headers(header_string)
#'
#' # Example with counts
#' mtcars2 <- mtcars %>%
#'   mutate_all(as.character)
#'
#' t <- tplyr_table(mtcars2, vs, cols = am) %>%
#'   add_layer(
#'     group_count(cyl)
#'   )
#'
#' b_t <- build(t) %>%
#'   mutate_all(as.character)
#'
#' count_string <- paste0(" | V N=**0** {auto N=**0_0** | man N=**0_1**} |",
#'                        " S N=**1** {auto N=**1_0** | man N=**1_1**} | | ")
#'
#' add_column_headers(b_t, count_string, header_n(t))
add_column_headers <- function(.data, s, header_n = NULL) {

  assert_that(all(map_chr(.data, class) == 'character'), msg = "When binding headers, all columns must be character")

  # Query everything needed from the string
  string_segments <- identify_string_segments(s)

  # Check to make sure the header string ois compliant
  validate_header_string(s, string_segments)

  # Check for spanners and grab each row separately if needed
  if (has_spanners(string_segments)) {
    rows <- c(get_spanner_row(s, string_segments), get_bottom_row(s, string_segments))
  } else {
    rows <- s
  }

  # Make into a data frame
  headers <- make_column_df(rows, names_from = .data, header_n = header_n)

  # Bind back on top of the data
  bind_rows(headers, .data)
}

#' Header string validation
#'
#' Checks of basic header string compliance
#'
#' @param s The header string
#' @param string_segments Processed string segments from \code{identify_string_segments}
#' @noRd
validate_header_string <- function(s, string_segments) {

  # Unpack the string segment variables into the environment
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

#' Parse out location information necessary to processing the header string
#'
#' Also identifies if spanners exist
#'
#' @param s The header string
#'
#' @return A header_locs object with all location information in a list. Attribute for if spanners exist
#' @noRd
identify_string_segments <- function(s) {

  # Find the inside groups
  op <- str_locate_all(s, fixed("{"))[[1]][,1] # Opening brackets
  cl <- str_locate_all(s, fixed("}"))[[1]][,1] # Closing brackets

  # Get the spanner locations and spanner sections
  spanner_locs <- str_locate_all(s, "[^{\\|]*\\{")[[1]] # Text from | to {
  spanned_sects <- str_locate_all(s,"[^\\{\\|]*\\{[^\\}]*\\}")[[1]] # The above but with the text between {}

  # Place it all in a list
  string_segments <- list(op = op, cl = cl, spanner_locs = spanner_locs, spanned_sects = spanned_sects)

  # Return the object with a class and `spanners` attribute specifying if there are spanners
  structure(
    string_segments,
    class="header_locs",
    spanners = length(op) > 0
    )
}

#' Extract the spanner row
#'
#' Pick out the spanner text and format into a delimitted string that can be split into the proper number
#' of elements to form a row of the umtimate data frame
#'
#' @param s The header string
#' @param string_segments Processed string segments from \code{identify_string_segments}
#'
#' @return Delimitted spanner row string
#' @noRd
get_spanner_row <- function(s, string_segments) {

  # Unpack the string segment variables into the environment
  list2env(string_segments, environment())

  spanning_row <- character(nrow(spanner_locs) + 1)

  for (i in seq_along(op)) {
    # Get the locations
    top_locs <- spanner_locs[i, ]

    # Grab part of the string to count the | from (i.e. the end of the last section)
    top_start <- ifelse(i == 1, 1, spanner_locs[i-1, ]['end'])

    # Grab the text for the spanner header
    top_txt <- str_trim(str_sub(s, top_locs['start'], top_locs['end']-1))

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

#' Extract the spanned/bottom row
#'
#' Pick out the spanned text and other elements and format into a delimitted string that can be split into the proper number
#' of elements to form a row of the umtimate data frame
#'
#' @param s The header string
#' @param string_segments Processed string segments from \code{identify_string_segments}
#'
#' @return Delimitted bottom string
#' @noRd
get_bottom_row <- function(s, string_segments) {

  # Unpack the string segment variables into the environment
  list2env(string_segments, environment())

  # Initialize the bottom row vector
  bottom_row <- character(nrow(spanner_locs) + 1)

  for (i in seq_along(op)) {
    # Get the spanned section locations
    sect_locs <- spanned_sects[i ,]
    sect_start <- ifelse(i == 1, 1, spanned_sects[i-1, ]['end']+2)

    # Grab part of the string to count the | from (i.e. the end of the last section)
    sect_txt <- str_trim(str_sub(s, sect_locs['start'], sect_locs['end']))

    # Get the text between the brackets
    spanned <- str_sub(s, start=op[i]+1, end=cl[i]-1)

    # Build up the bottom row vector
    bottom_row[[i]] <- str_replace(str_sub(s, sect_start, sect_locs['end']), fixed(sect_txt), spanned)
  }

  # Last elements of the bottom row
  if (nchar(s) == unname(sect_locs['end'])) {
    bottom_row <- head(bottom_row, -1)
  } else{
    bottom_row[[i+1]] <- str_sub(s, sect_locs['end']+2)
  }

  # Build the final text string
  bottom_row_text <- paste(map_chr(bottom_row, trim_bars), collapse="|")
  bottom_row_text
}


#' Check if spanners exist
#'
#' @param string_segments Processed string segments from
#'   \code{identify_string_segments}
#'
#' @return Boolean
#' @noRd
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
#' @noRd
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

#' Convert a string into a data.frame row with provided names
#'
#' @param s The header string or processed spanner/bottom rows
#' @param names Names to be provided as column names in the output
#' @param header_n Data.frame to use for evaluation of counts
#'
#' @return A data.frame row with the supplied names as columns
#' @noRd
make_header_row <- function(s, names, df) {

  # Logic for adding in counts based on double underscore token
  if (str_detect(s, "\\*\\*.+\\*\\*")) {

    assert_that(!is.null(df),
                msg = "You must pass a header_n if you are using replacement tokens")

    # Not sure how this would be done in a map
    string_placeholder <- str_extract_all(s, "\\*\\*.+?\\*\\*")

    for (i in seq_along(string_placeholder[[1]])) {

      # Remote the leading and trailing double underscores
      string_arg <- str_sub(string_placeholder[[1]][i], 3)
      string_arg <- str_sub(string_arg,
                            end = nchar(string_arg) - 2)

      # Split the placeholder on underscore and parse it as an expression
      split_args <- parse(text =  paste0("`", str_split(string_arg, "_", simplify = TRUE), "`"))

      # Evaluate the function in the text. Might want to add more tests to
      # header_n_value in case weird things are passed
      replacement <- as.character(get_header_n_value(df, split_args))

      # Replace Asterisk. Lots of escaping here
      string_placeholder[[1]][i] <- str_replace_all(string_placeholder[[1]][i],
                                                    "\\*", "\\\\*")
      # TODO: Might want to add formatting here
      s <- str_replace(s, string_placeholder[[1]][i], replacement)
    }
  }

  # Split the strings into invidual columns of a data frame
  row <- as_tibble(as.list(str_trim(str_split(s, fixed("|"))[[1]])), .name_repair='minimal')

  assert_that(ncol(row) == length(names), msg = "Number of columns provided in header string does not match data")

  # Bind the names and return the names data frame
  names(row) <- names
  row

}

#' Process the supplied rows and bind them as a data frame
#'
#' @param rows Either the header string or the spanner/bottoms rows (as a character vector)
#' @param names_from The dataframe that names shall be pulled from
#' @param header_n The header_n data.frame used to evaluate counts
#'
#' @return The bound header data frame
#' @noRd
make_column_df <- function(rows, names_from, header_n) {

  # Make sure all incoming rows are of the same length
  lengths_match <- length(unique(map_dbl(rows, ~ str_count(.x, fixed("|"))))) == 1
  assert_that(lengths_match, msg = "Malformed column header string - cells in spanner don't match cells in bottom row")

  # Create the individual dataframes from each row
  dfs <- map(rows, make_header_row, names=names(names_from), df = header_n)

  # Bind them together
  bind_rows(dfs)
}

