
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
#' @return For \code{tplyr_by}, the by binding of the supplied layer. For
#'   \code{set_by} the modified layer environment.
#' @export
#' @rdname by
#'
#' @examples
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

#' Set or return layer formatter
#'
#' @param layer A \code{tplyr_layer} object
#'
#' @return For \code{layer_formatter}, the formetter function bound to the
#'   supplied layer. For \code{set_layer_formatter} the modified layer environment.
#' @export
#' @rdname formatter
#'
#' @examples
#' iris$Species2 <- iris$Species
#' lay <- tplyr_table(iris, Species) %>%
#'   group_count(Species) %>%
#'   set_layer_formatter(as.numeric)
get_layer_formatter <- function(layer) {
  env_get(layer, "formatter")
}

#' @param formatter A function used to create the string formats for the
#'   resulting numbers in output presentation.
#'
#' @export
#' @rdname formatter
set_layer_formatter <- function(layer, formatter) {
  assert_that(is_function(formatter),
              msg = "formatter must be a function")

  env_bind(layer, formatter = formatter)

  layer
}

