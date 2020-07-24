
#' Set or return treat_var binding
#'
#' @param layer A \code{tplyr_layer} object
#'
#' @return For \code{treat_var}, the treatment variable binding of the layer
#'   object. For \code{set_treat_var}, the modified layer environment.
#' @export
#' @rdname target_var
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#' iris$Species2 <- iris$Species
#' lay <- tplyr_table(iris, Species) %>%
#'   group_count(Species) %>%
#'   set_target_var(Species2)
get_target_var <- function(layer) {
  env_get(layer, "target_var")
}

#' @param target_var A symbol to perform the analysis on
#'
#' @export
#' @rdname target_var
set_target_var <- function(layer, target_var) {
  target_var <- enquos(target_var)

  # Unpack sort_vars
  target_var <- unpack_vars(target_var, allow_character=FALSE)
  assert_quo_var_present(target_var, envir=layer, allow_character=FALSE)

  env_bind(layer, target_var = target_var)

  layer
}

#' Set or return by layer binding
#'
#' @param layer A \code{tplyr_layer} object
#'
#' @return For \code{get_by}, the by binding of the supplied layer. For
#'   \code{set_by} the modified layer environment.
#' @export
#' @rdname by
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#' iris$Species2 <- iris$Species
#' lay <- tplyr_table(iris, Species) %>%
#'   group_count(Species) %>%
#'   set_by(vars(Species2, Sepal.Width))
get_by <- function(layer) {
  env_get(layer, "by")
}

#' @param by A string, a variable name, or a list of variable names supplied
#'   using \code{dplyr::vars}.
#'
#' @export
#' @rdname by
set_by <- function(layer, by) {
  by <- enquos(by)

  # Unpack by
  by <- unpack_vars(by)
  assert_quo_var_present(by, envir=layer)

  env_bind(layer, by = by)

  layer
}

#' @export
#' @rdname where
get_where.tplyr_layer <- function(obj) {
  env_get(obj, "where")
}

#' @export
#' @rdname where
set_where.tplyr_layer <- function(obj, where) {
  where <- enquo(where)

  assert_that(is_logical_or_call(where),
              msg = "The `where` parameter must contain subsetting logic (enter without quotes)")

  env_bind(obj, where = where)

  obj
}

#' Return or set sort_vars layer binding
#'
#' @param layer A \code{tplyr_layer} object
#'
#' @return For \code{sort_vars}, the bindings of the layer object. For
#'   \code{set_sort_vars}, the modified layer environment.
#' @export
#' @rdname sort_vars
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#'
#' iris$Species2 <- iris$Species
#' lay <- tplyr_table(iris, Species) %>%
#'   group_count(Species) %>%
#'   set_sort_vars(vars(Sepal.Length, Sepal.Width))
get_sort_vars <- function(layer) {
  env_get(layer, "sort_vars")
}

#' @param sort_vars A character vector to sort the results of the summary.
#'
#' @export
#' @rdname sort_vars
set_sort_vars <- function(layer, sort_vars) {
  sort_vars <- enquos(sort_vars)

  #Unpack sort_vars
  sort_vars <- unpack_vars(sort_vars)
  assert_quo_var_present(sort_vars, envir=layer)

  env_bind(layer, sort_vars = sort_vars)

  layer
}

#' Set or return sort layer binding
#'
#' @param layer A \code{tplyr_layer} object
#'
#' @return For \code{layer_sort}, the sort binding of the layer object. For
#'   \code{set_layer_sort}, the modified layer environment.
#' @export
#' @rdname sort
#'
#' @examples
#' # Load in the pipe
#' library(magrittr)
#'
#' iris$Species2 <- iris$Species
#' lay <- tplyr_table(iris, Species) %>%
#'   group_count(Species) %>%
#'   set_layer_sort("desc")
get_layer_sort <- function(layer) {
  env_get(layer, "sort")
}

#' @param sort A string containing the sort method.
#'
#' @export
#' @rdname sort
set_layer_sort <- function(layer, sort) {
  assert_that(length(sort) == 1,
              sort %in% c("ascending", "desc"),
              msg = "sort must be 'ascending', 'desc'")

  env_bind(layer, sort = sort)

  layer
}

#' Set or return precision_by layer binding
#'
#' The precision_by variables are used to collect the integer and decimal precision
#' when auto-precision is used. These by variables are used to group the input data
#' and identify the maximum precision available within the dataset for each
#' by group. The precision_by variables must be a subset of the by variables
#'
#' @param layer A \code{tplyr_layer} object
#'
#' @return For \code{get_precision_by}, the precision_by binding of the supplied layer. For
#'   \code{set_precision_by} the modified layer environment.
#' @export
#' @rdname precision_by
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#' lay <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_desc(mpg, by=vars(carb, am)) %>%
#'     set_precision_by(carb)
#'   )
get_precision_by <- function(layer) {
  env_get(layer, "precision_by")
}

#' @param precision_by A string, a variable name, or a list of variable names supplied
#'   using \code{dplyr::vars}.
#'
#' @export
#' @rdname by
set_precision_by <- function(layer, precision_by) {
  precision_by <- enquos(precision_by)

  # Unpack by
  precision_by <- unpack_vars(precision_by)

  assert_that(all(precision_by %in% env_get(layer, "by")),
              msg = "The precision_by variables must be a subset of the layer by variables.")

  env_bind(layer, precision_by = precision_by)

  layer
}

#' Set or return precision_on layer binding
#'
#' The precision_on variable is the variable used to establish numeric precision.
#' This variable must be included in the list of \code{target_var} variables.
#'
#' @param layer A \code{tplyr_layer} object
#'
#' @return For \code{get_precision_on}, the precision_on binding of the supplied layer. For
#'   \code{set_precision_on} the modified layer environment.
#' @export
#' @rdname precision_on
#'
#' @examples
#' # Load in pipe
#' library(magrittr)
#' lay <- tplyr_table(mtcars, gear) %>%
#'   add_layer(
#'     group_desc(vars(mpg, disp), by=vars(carb, am)) %>%
#'     set_precision_on(disp)
#'   )
get_precision_on <- function(layer) {
  env_get(layer, "precision_on")
}

#' @param precision_on A string, a variable name, or a list of variable names supplied
#'   using \code{dplyr::vars}.
#'
#' @export
#' @rdname precision_on
set_precision_on <- function(layer, precision_on) {
  precision_on <- enquo(precision_on)

  assert_that(list(precision_on) %in% env_get(layer, "target_var"),
              msg = "The precision_on variable must be included in `target_var`")

  env_bind(layer, precision_on = precision_on)

  layer
}
