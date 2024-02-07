#' Set variables to limit reported data values only to those that exist rather
#' than fully completing all possible levels
#'
#' This function allows you to select a combination of by variables or
#' potentially target variables for which you only want to display values
#' present in the data. By default, Tplyr will create a cartesian combination of
#' potential values of the data. For example, if you have 2 by variables
#' present, then each potential combination of those by variables will have a
#' row present in the final table. `set_limit_data_by()` allows you to choose
#' the by variables whose combination you wish to limit to values physically
#' present in the available data.
#'
#' @param e A tplyr_layer
#' @param ... Subset of variables within by or target variables
#'
#' @return a tplyr_table
#' @md
#' @export
#'
#' @examples
#'
#' tplyr_table(tplyr_adpe, TRT01A) %>%
#'   add_layer(
#'     group_desc(AVAL, by = vars(PECAT, PARAM, AVISIT))
#'   ) %>%
#'   build()
#'
#' tplyr_table(tplyr_adpe, TRT01A) %>%
#'   add_layer(
#'     group_desc(AVAL, by = vars(PECAT, PARAM, AVISIT)) %>%
#'       set_limit_data_by(PARAM, AVISIT)
#'   ) %>%
#'   build()
#'
#' tplyr_table(tplyr_adpe, TRT01A) %>%
#'   add_layer(
#'     group_count(AVALC, by = vars(PECAT, PARAM, AVISIT)) %>%
#'       set_limit_data_by(PARAM, AVISIT)
#'   ) %>%
#'   build()
#'
#' tplyr_table(tplyr_adpe, TRT01A) %>%
#'   add_layer(
#'     group_count(AVALC, by = vars(PECAT, PARAM, AVISIT)) %>%
#'       set_limit_data_by(PECAT, PARAM, AVISIT)
#'   ) %>%
#'   build()
set_limit_data_by <- function(e, ...) {
  UseMethod("set_limit_data_by")
}

#' @export
#' @noRd
set_limit_data_by.count_layer <- function(e, ...) {
  dots <- enquos(...)
  dots_chr <- map_chr(dots, as_name)

  # Pull these variables to make sure the denoms used make sense
  by_ <- map_chr(env_get(e, "by"), as_name)
  tv_ <- map_chr(env_get(e, "target_var"), as_name)

  if (!all(dots_chr %in% c(by_, tv_))) {
    stop("Limit data by variables must be included in by variables or target variable set on layer", call.=FALSE)
  }

  env_bind(e, limit_data_by = dots)
  e
}

#' @export
#' @noRd
set_limit_data_by.shift_layer <- function(e, ...) {
  set_limit_data_by.count_layer(e, ...)
}

#' @export
#' @noRd
set_limit_data_by.desc_layer <- function(e, ...) {
  dots <- enquos(...)
  dots_chr <- map_chr(dots, as_name)

  # Pull these variables to make sure the denoms used make sense
  by_ <- map_chr(env_get(e, "by"), as_name)

  if (!all(dots_chr %in% by_)) {
    stop("Limit data by variables must be included in by variables set on layer", call.=FALSE)
  }

  env_bind(e, limit_data_by = dots)
  e
}

#' General function used to process the steps to pad levels in data, or limit to
#' combinations available in the data itself
#'
#' @param dat Input dataset
#' @param treat_var treat_var from tplyr_table
#' @param by by from tplyr_layer
#' @param cols cols from tplyr_table
#' @param target_var target_var from tplyr_layer
#' @param limit_data_by The variables to limit data by
#' @param .fill .fill parameter passed onto dplyr::complete
#' @param outer Whether to bypass variables if working through the outer layer
#'
#' @noRd
complete_and_limit <- function(dat, treat_var, by, cols, target_var=quos(), limit_data_by, .fill=list(), outer=FALSE) {

  complete_levels <- dat %>%
    # complete all combinations of factors to include combinations that don't exist.
    # add 0 for combinations that don't exist
    complete(!!treat_var, !!!by, !!!unname(target_var), !!!cols,
             fill = .fill)

  # Apply data limits specified by setter
  if (!is.null(limit_data_by)) {
    # Outer layer won't have the target variable to limit by
    if (outer) {
      limit_data_by <- limit_data_by[map_chr(limit_data_by, as_name) %in% names(dat)]
    }

    # Find the combinations actually in the data
    groups_in_data <- dat %>%
      distinct(!!!limit_data_by)

    # Join back to limit the completed levels based on the preferred
    # data driven ones
    limited_data <- groups_in_data %>%
      left_join(complete_levels, by = map_chr(limit_data_by, as_name))

    return(limited_data)
  }

  complete_levels
}
