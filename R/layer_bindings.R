
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
