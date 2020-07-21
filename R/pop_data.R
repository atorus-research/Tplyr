### Populations Functions

#' The default setter for header_n table binding
#'
#' @param table `tplyr_table` object
#'
#' @noRd
default_header_n <- function(table) {
  pop_data <- NULL
  pop_treat_var <- NULL

  evalq({
    df <- pop_data %>%
      filter(!!table_where) %>%
      group_by(!!pop_treat_var, !!!cols) %>%
      tally() %>%
      ungroup() %>%
      complete(!!pop_treat_var, !!!cols, fill = list(n = 0))

    header_n <- df
    rm(df)
  }, envir = table)
  table
}

#' Add a treatment group for the entire dataset population
#'
#' This function is a wrapper around \code{add_treat_group} and creates a group
#' named 'Total', which contains all subjects.
#'
#' @param table A \code{tplyr_table} object
#'
#' @return The modified table object
#' @export
#'
#' @examples
#' tab <- tplyr_table(iris, Species)
#'
#' add_total_group(tab)
#' treat_grps(tab)
#' # Returns "setosa", "verisicolor", and "virginica"
#'
#' @rdname treat_grps
add_total_group <- function(table) {
  evalq({
    add_treat_group(current_env(), "Total",
                    as.character(unlist(unique(pop_data[, quo_name(pop_treat_var)]))))
  }, envir = table)
}
