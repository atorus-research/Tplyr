
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
target_var <- function(layer) {
  env_get(layer, "target_var")
}

#' @param target_var A symbol to perform the analysis on
#'
#' @export
#' @rdname target_var
set_target_var <- function(layer, target_var) {
  target_var <- enquo(target_var)

  assert_that(class(quo_get_expr(target_var)) == "name",
              msg = "target_var must be a variable name")

  env_bind(layer, target_var = target_var)
}

#' Set or return by layer binding
#'
#' @param layer A \code{tplyr_layer} object
#'
#' @return For \code{tplyr_by}, the by binding of the supplied layer. For
#'   \code{set_tplyr_by} the modified layer environment.
#' @export
#' @rdname by
#'
#' @examples
#' iris$Species2 <- iris$Species
#' lay <- tplyr_table(iris, Species) %>%
#'   group_count(Species) %>%
#'   set_tplyr_by(vars(Species2, Sepal.Width))
tplyr_by <- function(layer) {
  env_get(layer, "by")
}

#' @param by A string, a variable name, or a list of variable names supplied
#'   using \code{dplyr::vars}.
#'
#' @export
#' @rdname by
set_tplyr_by <- function(layer, by) {
  dmessage(paste("By came in as: ",class(by)))

  by <- enquos(by)

  # Unpack the `by` group to ensure that the type is `list_of<quosures>`
  # It had to be a 1 item list, so check if that element is a `call`
  # The only valid use of a `call` is to provide multiple variables using `vars`
  c <- quo_get_expr(by[[1]])
  if (is.call(c)) {
    # If it's a call, we need to pull it out a level
    by <- tryCatch({
      # If it's in here, the call has to be to dplyr::vars
      if (call_name(c) != "vars") stop("Multiple variables should be using dplyr::vars")

      # Evaluate the quosure by getting the expression
      eval(c, envir=caller_env())
    },
    # If a 1 item list of variable was provided, it'll fail
    error = function(err) {
      abort(message = paste0("Invalid input to `by`. Submit either a string, a variable name, ",
                             "or multiple variable names using `dplyr::vars`."))
    })
  }

  # Make sure that by variables not submitted as characters exist in the target dataframe
  if (!quo_is_null(by[[1]])) {
    # Make sure the variables provided to `by` are of the correct type
    msg = paste0("Invalid input to `by`. Submit either a string, a variable name, ",
                 "or multiple variable names using `dplyr::vars`.")
    are_quosures <- all(sapply(by, function(x) is_quosure(x)))
    assert_that(are_quosures, msg = msg)

    # Check each element of the `by`` list
    for (v in by) {
      dmessage(print(quo_get_expr(v)))
      dmessage(paste("Checking", as.character(quo_get_expr(v))))
      # While looping, making sure calls weren't submitted
      if (class(quo_get_expr(v)) == "call") {
        abort("Arguments to `by` must be names or character strings - cannot be calls (i.e. x + y, list(a, b c)).")
      }
      else if (!class(quo_get_expr(v)) %in% c('name', 'character')) {
        abort("Invalid input to `by`. Submit either a string, a variable name, or multiple variable names using `dplyr::vars`.")
      }
    }
  }

  env_bind(layer, by = by)
}

#' Set or return where layer binding
#'
#' @param layer A \code{tplyr_layer} object.
#'
#' @return For \code{tplyr_where}, the where binding of the supplied object.
#'   For \code{set_tplyr_where}, the modified object
#' @export
#' @rdname where
#'
#' @examples
#' iris$Species2 <- iris$Species
#' lay <- tplyr_table(iris, Species) %>%
#'   group_count(Species) %>%
#'   set_tplyr_where(Petal.Length > 3)
tplyr_where <- function(layer) {
  env_get(layer, "where")
}

#' @param where A function detailing the subset
#'
#' @return
#' @export
#' @rdname where
set_tplyr_where <- function(layer, where) {
  where <- enquo(where)
  assert_that(quo_is_missing(where) || class(quo_get_expr(where)) == 'call',
              msg = "The `where` parameter must contain subsetting logic (enter without quotes)")

  env_bind(layer, where = where)
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
sort_vars <- function(layer) {
  env_get(layer, "sort_vars")
}

#' @param sort_vars A character vector to sort the results of the summary.
#'
#' @export
#' @rdname sort_vars
set_sort_vars <- function(layer, sort_vars) {
  dmessage(paste("sort_vars came in as: ",class(sort_vars)))

  sort_vars <- enquos(sort_vars)

  # Unpack the `sort_vars` group to ensure that the type is `list_of<quosures>`
  # It had to be a 1 item list, so check if that element is a `call`
  # The only valid use of a `call` is to provide multiple variables using `vars`
  c <- quo_get_expr(sort_vars[[1]])
  if (is.call(c)) {
    # If it's a call, we need to pull it out a level
    sort_vars <- tryCatch({
      # If it's in here, the call has to be to dplyr::vars
      if (call_name(c) != "vars") stop("Multiple variables should be using dplyr::vars")

      # Evaluate the quosure sort_vars getting the expression
      eval(c, envir=caller_env())
    },
    # If a 1 item list of variable was provided, it'll fail
    error = function(err) {
      abort(message = paste0("Invalid input to `sort_vars`. Submit either a string, a variable name, ",
                             "or multiple variable names using `dplyr::vars`."))
    })
  }

  # Make sure that sort_vars variables not submitted as characters exist in the target dataframe
  if (!quo_is_null(sort_vars[[1]])) {
    # Make sure the variables provided to `sort_vars` are of the correct type
    msg = paste0("Invalid input to `sort_vars`. Submit either a string, a variable name, ",
                 "or multiple variable names using `dplyr::vars`.")
    are_quosures <- all(sapply(sort_vars, function(x) is_quosure(x)))
    assert_that(are_quosures, msg = msg)

    # Check each element of the `sort_vars` list
    for (v in sort_vars) {
      dmessage(print(quo_get_expr(v)))
      dmessage(paste("Checking", as.character(quo_get_expr(v))))
      # While looping, making sure calls weren't submitted
      if (class(quo_get_expr(v)) == "call") {
        abort("Arguments to `sort_vars` must be names or character strings - cannot be calls (i.e. x + y, list(a, b c)).")
      }
      else if (!class(quo_get_expr(v)) %in% c('name', 'character')) {
        abort("Invalid input to `sort_vars`. Submit either a string, a variable name, or multiple variable names using `dplyr::vars`.")
      }
    }
  }


  env_bind(layer, sort_vars = sort_vars)
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
layer_sort <- function(layer) {
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
layer_formatter <- function(layer) {
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
}

