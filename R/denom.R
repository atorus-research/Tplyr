

#' Get the value of the header_n value in a grouped df
#'
#' This is intended to be called in a dplyr context using `do`. This function
#' returns the value of the header_n for the current group. `do` is currently
#' labeled as questioning but its replacement, group_map, is experimental
#'
#' @param .data A tibble that has been grouped
#' @param header_n The header_n binding from a \code{tplyr_table} object.
#'
#' @return A single numeric value from the header_n binding that is pulled
#'  from the groups
#' @export
#'
#' @examples
#' library(magrittr)
#'
#' t <- tplyr_table(mtcars, gear)
#'
#' mtcars %>%
#'   group_by(gear, vs) %>%
#'   do(this_denom(., header_n(t)))
#'
this_denom <- function(.data, header_n) {
  # Get names of header_n execpt last one, last should be the count
  header_n_grp_names <- names(header_n)[1:ncol(header_n) - 1]

  # Make sure all of the header_n group names have only one unique value.
  # If they don't that means they weren't grouped on correctly.
  assert_that(all(map_lgl(header_n_grp_names, function(x) {
    nrow(unique(.data[, x])) == 1
  })),
  msg = "The groups passed to `this_denom` weren't correct.
  All columns in the call must be in separate groups in the table.")

  # Pull out each unique filter requirement. Each name for header_n is stored
  # on the LHS and its unique value in the function is on the RHS.
  # Examples
  # gear == 3
  # am == 1
  # These are stored in a list that is evaluated below
  filter_logic <- map(header_n_grp_names, function(x) {
    expr(!!as.symbol(x) == !!unique(.data[, x])[[1]])
  })

  # Evaluate the filter above and pull out the 'n' column
  header_n %>%
    filter(!!!filter_logic) %>%
    select(n) %>%
    rename("total" = n)
}
