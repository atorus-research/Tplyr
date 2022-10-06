#' Wrap strings to a specific width with hyphenation while preserving
#' indentation
#'
#' `str_indent_wrap()` leverages `stringr::str_wrap()` under the hood, but takes
#' some extra steps to preserve any indentation that has been applied to a
#' character element, and use hyphenated wrapping of single words that run
#' longer than the allotted wrapping width.
#'
#' The function `stringr::str_wrap()` is highly efficient, but in the
#' context of table creation there are two select features missing - hyphenation
#' for long running strings that overflow width, and respect for pre-indentation
#' of a character element. For example, in an adverse event table, you may have
#' body system rows as an un-indented column, and preferred terms as indented
#' columns. These strings may run long and require wrapping to not surpass the
#' column width. Furthermore, for crowded tables a single word may be longer
#' than the column width itself.
#'
#' This function takes steps to resolve these two issues, while trying to
#' minimize additional overhead required to apply the wrapping of strings.
#'
#' Note: This function automatically converts tabs to spaces. Tab width varies
#' depending on font, so width cannot automatically be determined within a data
#' frame. As such, users can specify the width
#'
#' @param x An input character vector
#' @param width The desired width of elements within the output character vector
#' @param tab_width The number of spaces to which tabs should be converted
#'
#' @return A character vector with string wrapping applied
#' @export
#' @md
#'
#' @examples
#' ex_text1 <- c("RENAL AND URINARY DISORDERS", "   NEPHROLITHIASIS")
#' ex_text2 <- c("RENAL AND URINARY DISORDERS", "\tNEPHROLITHIASIS")
#'
#' cat(paste(str_indent_wrap(ex_text1, width=8), collapse="\n\n"),"\n")
#' cat(paste(str_indent_wrap(ex_text2, tab_width=4), collapse="\n\n"),"\n")
str_indent_wrap <- function(x, width=10, tab_width=5) {

  if (!inherits(x, 'character')) {
    stop('x must be a character vector', call.=FALSE)
  }

  # Scan out tabs and convert them to spaces
  x <- str_replace_all(x, "\\t", strrep(" ", tab_width))

  # Find where the splits need to happen
  sections <- str_locate_all(x, paste0("\\w{", width-1, "}(?=\\w)"))

  # Using the locations, build up the matrix of substrings
  split_mat <- map(sections, ~ matrix(c(1, .[,2]+1, .[,2], -1), ncol=2))

  # Dive the string into the necessary chunks
  splits <- map2(x, split_mat, str_sub)

  hyph_str <- map_chr(splits, paste, collapse = "- ")

  # Get the indentation of the strings and make a data frame
  wrap_df <- tibble(
    l = get_ind_len(x),
    w = width - l,
    s = hyph_str
  )

  # Group by width and length, wrap the strings, and return the output vector
  wrap_df %>%
    group_by(w, l) %>%
    mutate(
      out = grouped_str_wrap(s)
    ) %>%
    pull(out)
}

#' Get the indentation length
#'
#' Vectorized approach to extracting the length of indentation, with
#' compensation for NAs
#'
#' @param s Input string to have indentation length measured
#'
#' @return Integer vector of character length of indentation
#' @noRd
get_ind_len <- function(s) {
  inds <- str_extract(s, "^\\s+")
  inds[which(is.na(inds))] <- ""
  map_int(inds, nchar, type = "width")
}

#' Proper application of str_wrap in a grouped context
#'
#' Width, indent, and exdent all need single elements, so str_wrap doesn't work
#' well in an grouped mutate context through a data frame. So using the grouped
#' structure, this pulls out the group call str_wrap using the single element
#' integers for width, indent, and exdent
#'
#' @param s Input character vector string to wrap
#'
#' @return Character vector of wrapped strings
#' @noRd
grouped_str_wrap <- function(s) {
  g <- cur_group()
  str_wrap(s, width = g$w, indent = g$l, exdent = g$l)
}
