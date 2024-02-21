#' Add indentation level based
#'
#' @param .x The number of levels to indent
#' @param .y Input variable for which indentation will be done
#' @param indent Indentation string to be used, which is multiplied at each indentation level
#'
#' @return Character string with indentation applied
#' @noRd
add_indentation <- function(.x, .y, indent = "  ") {
  paste(c(rep("",.x-1), .y), collapse=indent)
}


#' Collapse row labels into a single column
#'
#' This is a generalized post processing function that allows you to take groups
#' of by variables and collapse them into a single column. Repeating values are
#' split into separate rows, and for each level of nesting, a specified
#' indentation level can be applied.
#'
#' @param x Input data frame
#' @param ... Row labels to be collapsed
#' @param indent Indentation string to be used, which is multiplied at each indentation level
#' @param target_col The desired name of the output column containing collapsed row labels
#'
#' @return data.frame with row labels collapsed into a single column
#' @export
#'
#' @examples
#' x <- tibble::tribble(
#' ~row_label1, ~row_label2, ~row_label3, ~row_label4, ~var1,
#'   "A",         "C",         "G",         "M",        1L,
#'   "A",         "C",         "G",         "N",        2L,
#'   "A",         "C",         "H",         "O",        3L,
#'   "A",         "D",         "H",         "P",        4L,
#'   "A",         "D",         "I",         "Q",        5L,
#'   "A",         "D",         "I",         "R",        6L,
#'   "B",         "E",         "J",         "S",        7L,
#'   "B",         "E",         "J",         "T",        8L,
#'   "B",         "E",         "K",         "U",        9L,
#'   "B",         "F",         "K",         "V",        10L,
#'   "B",         "F",         "L",         "W",        11L
#' )
#'
#'
#' collapse_row_labels(x, row_label1, row_label2, row_label3, row_label4)
#'
#' collapse_row_labels(x, row_label1, row_label2, row_label3)
#'
#' collapse_row_labels(x, row_label1, row_label2, indent = "    ", target_col = rl)
#'
collapse_row_labels <- function(x, ..., indent = "  ", target_col=row_label) {

  target_col = enquo(target_col)
  dots <- enquos(...)

  # browser()
  dot_names <- map_chr(dots, as_label)

  if (!inherits(x, 'data.frame')) {
    stop('x must be a data frame', call.=FALSE)
  }

  if (!inherits(indent, 'character')) {
    stop("indent must be a character string", call.=FALSE)
  }

  if (!all(map_lgl(dots, quo_is_symbol))) {
    stop("Columns provided to dots must be provided as unquoted symbols.", call.=FALSE)
  }

  if (!all(dot_names %in% names(x))) {
    stop("Columns provided to dots are missing from x.", call.=FALSE)
  }

  if (!quo_is_symbol(target_col)) {
    stop("target_col must be provided as an unquoted symbol.", call.=FALSE)
  }

  if (length(dots) < 2) {
    stop("Must have two or more columns to collapse", call.=FALSE)
  }

  all_but_last <- dots[1:length(dots)-1]

  # Add the original row identifier
  x['og_row'] <- as.numeric(rownames(x))

  # Grab the desired rowlabels, except for the last one specified in the dots
  rowlabs <- select(x, !!!all_but_last, og_row)

  # Get the distinct list of stubs from the data and grab the nesting level
  stubs <- rowlabs %>%
    group_by(!!!all_but_last) %>%
    slice_head() %>%
    pivot_longer(
      map_chr(all_but_last, as_label),
      names_to = NULL,
      values_to = as_label(target_col)
    ) %>%
    group_by(og_row) %>%
    mutate(
      stub_sort = row_number()
    )

  # Join back to the original data
  x %>%
    bind_rows(stubs, .id="id") %>%
    # Put everything into the right spot
    arrange(og_row, desc(id)) %>%
    fill(stub_sort) %>%
    mutate(
      # Figure out the indentation level
      stub_sort = if_else(id == 1, stub_sort + 1, stub_sort),
      # Build and indent the new row label column
      !!target_col := if_else(is.na(!!target_col), !!!tail(dots, 1), !!target_col),
      !!target_col := map2_chr(stub_sort, !!target_col, add_indentation, indent = indent),
      # Fill in the empty character fields
      across(where(is.character), ~ replace_na(., ''))
    ) %>%
    select(!!target_col, !c(id, og_row, stub_sort, !!!dots))
}
