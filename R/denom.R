

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
#' library(dplyr)
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


#' Get the header_n value of a certain
#'
#' @param x A tplyr_table or data.frame object
#' @param ... Parameters to filter the header_n. Should be in the order of
#'   variables passed in the tplyr_table
#'
#' @return The sum of the subset of the header_n values after filtering
#' @noRd
#'
#' @examples
#' library(dplyr)
#'
#' t <- tplyr_table(mtcars, gear, cols = vars(cyl, am))
#'
#' get_header_n_value(t, 3, 6, 0)
#' # Returns the number of cars that have 3 gears, 6 cyinders, and auto trasmission
get_header_n_value <- function(x, ...) {
  UseMethod("get_header_n_value")
}

#' @noRd
get_header_n_value.tplyr_table <- function(x, ...) {
  # Arguments passed
  dots <- enquos(...)

  header_names <- names(header_n(x))

  # You can use all columns in the header_n except the last, find the minimum
  # of the number of dots passed and the columns you can subset on
  param_num <- min(length(header_names) - 1, length(dots))

  # Just pull out the names you are selecting for
  dots_names <- header_names[1:param_num]

  ## I tried this in a map but I had trouble with the names being stripped out
  filter_logic <- list()
  for (i in seq_along(dots)) {
    filter_logic <- append(filter_logic, expr(!!as.symbol(dots_names[i]) == !!dots[[i]]))
  }

  # filter_logic <- map(dots, function(x) {
  #   print(x)
  #   print(class(x))
  #   print(str(x))
  #   expr(!!as.symbol(names(x)) == !!x)
  #   })

  header_n(x) %>%
    filter(!!!filter_logic) %>%
    select(n) %>%
    sum()
}

#' @noRd
get_header_n_value.data.frame <- function(x, ...) {
  # Arguments passed
  #dots <- enquos(...)

  header_names <- names(x)

  # You can use all columns in the header_n except the last, find the minimum
  # of the number of dots passed and the columns you can subset on
  param_num <- min(length(header_names) - 1, length(...))

  # Just pull out the names you are selecting for
  dots_names <- header_names[1:param_num]

  ## I tried this in a map but I had trouble with the names being stripped out
  filter_logic <- list()
  for (i in seq_along(...)) {
    filter_logic <- append(filter_logic, expr(!!as.symbol(dots_names[i]) == !!...[[i]]))
  }

  x %>%
    filter(!!!filter_logic) %>%
    select(n) %>%
    sum()

}
