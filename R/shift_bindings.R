

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
set_denoms_by <- function(x, ...) {

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
    dots[[which(as_name(target_var$row) %in% dots_chr)]] <- quo(summary_var)
  }

  env_bind(x, denoms_by = dots)

  x
}
