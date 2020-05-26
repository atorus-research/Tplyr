### Populations Functions

#' Title
#'
#' @param table `tplyr_table` object
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr n
#'
#' @noRd
default_header_n <- function(table) {
  pop_data <- NULL
  pop_treat_var <- NULL

  evalq({
    df <- pop_data %>%
      group_by(!!pop_treat_var) %>%
      summarise(N = dplyr::n())

    header_n <- unlist(df[, 2])
    names(header_n) <- unlist(df[, 1])
    rm(df)
  }, envir = table)
  table
}
