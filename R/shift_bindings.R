
#' Set variables used in pct denominator calculation
#'
#' This function is used when calculating pct in count or shift layers. The
#' percentages default to the treatment variable and any column variables but
#' can be calculated on any variables passed to target_var, treat_var, by, or
#' cols.
#'
#' @param x A count/shift layer object
#' @param ... Unquoted variable names
#'
#' @return The modified layer object
#' @export
#' @examples
#' library(magrittr)
#'
#' # Default has matrix of treatment group, additional columns,
#' # and by variables sum to 1
#' tplyr_table(mtcars, am) %>%
#'   add_layer(
#'     group_shift(vars(row=gear, column=carb), by=cyl) %>%
#'       set_format_strings(f_str("xxx (xx.xx%)", n, pct))
#'   ) %>%
#'   build()
#'
#' tplyr_table(mtcars, am) %>%
#'   add_layer(
#'     group_shift(vars(row=gear, column=carb), by=cyl) %>%
#'       set_format_strings(f_str("xxx (xx.xx%)", n, pct)) %>%
#'       set_denoms_by(cyl, gear) # Row % sums to 1
#'   ) %>%
#'   build()
#'
#' tplyr_table(mtcars, am) %>%
#'   add_layer(
#'     group_shift(vars(row=gear, column=carb), by=cyl) %>%
#'       set_format_strings(f_str("xxx (xx.xx%)", n, pct)) %>%
#'       set_denoms_by(cyl, gear, am) # % within treatment group sums to 1
#'   ) %>%
#'   build()
set_denoms_by <- function(x, ...) {
  UseMethod("set_denoms_by")
}

set_denoms_by.shift_layer <- function(x, ...) {

  dots <- vars(...)
  dots_chr <- map_chr(dots, as_name)

  # Pull these variables to make sure the denoms used make sense
  by_ <- map_chr(env_get(x, "by"), as_name)
  cols_ <- map_chr(env_get(x, "cols", inherit = TRUE), as_name)
  treat_var_ <- as_name(env_get(x, "treat_var", inherit = TRUE))
  target_var <- env_get(x, "target_var")
  target_var_ <- map_chr(target_var, as_name)

  assert_that(all(dots_chr %in% c(by_, cols_, treat_var_, target_var_)),
              msg = "A denom_by wasn't found as a grouping variable in the layer/table.")

  # If the row variable is here, rename it to summary_var
  if(as_name(target_var$row) %in% dots_chr) {
    dots[[which(dots_chr %in% as_name(target_var$row))]] <- quo(summary_var)
  }

  env_bind(x, denoms_by = dots)

  x
}

set_denoms_by.count_layer <- function(x, ...) {
  dots <- vars(...)
  dots_chr <- map_chr(dots, as_name)

  # Pull these variables to make sure the denoms used make sense
  by_ <- map_chr(env_get(x, "by"), as_name)
  cols_ <- map_chr(env_get(x, "cols", inherit = TRUE), as_name)
  treat_var_ <- as_name(env_get(x, "treat_var", inherit = TRUE))
  target_var <- env_get(x, "target_var")
  target_var_ <- map_chr(target_var, as_name)

  assert_that(all(dots_chr %in% c(by_, cols_, treat_var_, target_var_)),
              msg = "A denom_by wasn't found as a grouping variable in the layer/table.")

  # If the row variable is here, rename it to summary_var
  if(as_name(target_var[[1]]) %in% dots_chr) {
    dots[[which(as_name(target_var[[1]]) %in% dots_chr)]] <- quo(summary_var)
  }

  env_bind(x, denoms_by = dots)

  x
}
