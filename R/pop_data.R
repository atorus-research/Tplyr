### Populations Functions

#' Title
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr n
default_header_n <- function(table) {
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
