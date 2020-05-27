### Layer Constructor


#' Create a \code{tplyr_layer} object
#'
#' @description
#' This object is the workhorse of the \code{tplyr} package. A \code{tplyr_layer} can be thought of
#' as a block, or "layer" of a table. Summary tables typically consist of different sections that require
#' different summaries. When programming these section, your code will create different layers that need to be
#' stacked or merged together. A \code{tplyr_layer} is the container for those isolated building blocks.
#'
#' When building the \code{tplyr_table}, each layer will execute independently. When all of the data processing has
#' completed, the layers are brought together to construct the output.
#'
#' \code{tplyr_layer} objects are not created directly, but rather the layer constructor functions \code{add_group_counts},
#' \code{add_group_desc}, \code{add_group_shift}, and their related subgroup functions.
#'
#' See the \href{<link tbd}{vignette} for a more extensive explanation of how these pieces fit together.
#'
#' @param parent \code{tplyr_table} or \code{tplyr_layer}. Required. The parent environment of the layer. This must be either the
#'   \code{tplyr_table} object that the layer is contained within, or another \code{tplyr_layer} object of which
#'   the layer is a subgroup.
#' @param type "count", "desc", or "shift". Required. The category of layer - either "counts" for categorical counts, "desc" for
#'   descriptive statistics, or "shift" for shift table counts
#' @param by A string, a variable name, or a list of variable names supplied using \code{dplyr::vars}
#' @param target_var Symbol. Required, The variable name on which the summary is to be performed. Must be a variable within
#'   the target dataset. Enter unquoted - i.e. target_var = AEBODSYS.
#' @param where Call. Filter logic used to subset the target data when performing a summary.
#' @param ... Additional arguments that will be passed directly into the \code{tplyr_layer} environment. See the
#'   \href{<link tbd>}{vignette} on adding extensions.
#'
#' @return An \code{tplyr_layer} environment that is a child of the specified parent. The environment contains the object
#'   as listed below.
#'
#' @section \code{tplyr_layer} Object Structure:
#' \describe{
#' \item{\code{type}}{This is an attribute. A string indicating the layer type, which controls the summary that will be performed.}
#' \item{\code{target_var}}{A quosure of a name, which is the variable on which a summary will be performed.}
#' \item{\code{by}}{A list of quosures representing either text labels or variable names used in grouping. Variable names must exist
#' within the target dataset Text strings submitted do not need to exist in the target dataset.}
#' \item{\code{where}}{A quosure of a call that containers the filter logic used to subset the target dataset.}
#' \item{\code{sort_vars}}{A character vector containingn the variables that will be used to sort the results of the summary.
#'   Set by default to the value of \code{target_var}}
#' \item{\code{sort}}{A string containing the sort method. Defaults to 'asc' for ascending.}
#' \item{\code{layers}}{A list with class \code{tplyr_layer_container}. Initialized as empty, but serves as the container for
#' any sublayers of the current layer.}
#' \item{\code{formatter}}{A function used to create the string formats for the resulting numbers in output presentation.}
#' }
#' @export
#'
#' @examples
#' tab <- tplyr_table(iris, Sepal.Width)
#'
#' l <- tplyr_layer(tab, type='count', by=vars('Label Text', AEBODSYS), target_var=AEDECOD, where= AESER == "Y")
#'
#' @seealso \code{\link{tplyr_table}}
tplyr_layer <- function(parent, type, by=NULL, target_var=NULL, where=NULL, ...) {

  # Return a null object if the parent is missing -
  if(missing(parent)) return(structure(env(), class = c("tplyr_layer", "environment")))

  # If necessary variables provided then build the layer
  as_tplyr_layer(parent, type=type, by=enquos(by), target_var=enquo(target_var), where=enquo(where), ...)
}

# Method dispatch
#' Create a tplyr layer
#'
#' @inheritParams tplyr_layer
#' @noRd
as_tplyr_layer <- function(parent, type, by, target_var, where, ...) {
  UseMethod("as_tplyr_layer")
}

## tplyr table
as_tplyr_layer.tplyr_table <- function(parent, type, by, target_var, where, ...) {
  dmessage('Dispatch tplyr_table')
  new_tplyr_layer(parent, type, by, target_var, where, ...)
}

## tplyr layer
as_tplyr_layer.tplyr_layer <- function(parent, type, by, target_var, where, ...) {
  dmessage('Dispatch tplyr_layer')
  layer <- new_tplyr_layer(parent, type, by, target_var, where, ...)
  class(layer) <- append('tplyr_subgroup_layer', class(layer))
  layer
}

as_tplyr_layer.tplyr_subgroup_layer <- function(parent, type, by, target_var, where, ...) {
  dmessage('Dispatch tplyr_layer')
  layer <- new_tplyr_layer(parent, type, by, target_var, where, ...)
  class(layer) <- unique(append('tplyr_subgroup_layer', class(layer)))
  layer
}

## Unsupported object
as_tplyr_layer.default <- function(parent, type, by, target_var, where, ...) {
  dmessage('Dispatch default')
  stop('Must provide `tplyr_table`, `tplyr_layer`, or `tplyr_subgroup_layer` object from the `tplyr` package.')
}

#' Create a new tplyr layer
#'
#' @inheritParams tplyr_layer
#' @noRd
new_tplyr_layer <- function(parent, type, by, target_var, where, ...) {
  dmessage('--- new_tplyr_layer')

  # Pull out the arguments from the function call that aren't quosures (and exclude parent)
  arg_list <- as.list(match.call())[-c(1:3)]

  # Insert parent to the front of the list to prepare the call
  arg_list <- append(arg_list, parent, after=0)

  # Unpack the `by` group to ensure that the type is `list_of<quosures>`
  # It had to be a 1 item list, so check if that element is a `call`
  # The only valid use of a `call` is to provide multiple variables using `vars`
  if (class(quo_get_expr(by[[1]])) == "call") {
    # If it's a call, we need to pull it out a level
    by <- tryCatch(
      # Evaluate the quosure by getting the expressionn
      eval(quo_get_expr(by[[1]]), envir=caller_env()),
      # If a 1 item list of variable was provided, it'll fail
      error = function(err) {
        abort(message = paste0("Invalid input to `by`. Submit either a string, a variable name, ",
                               "or multiple variable names using `dplyr::vars`."))
      }
    )
    # Override in the arglist
    arg_list$by <- by
  }

  # Run validation
  validate_tplyr_layer(parent, type, by, target_var, where)

  # Create the new environment by contructing the env call
  e <- do.call('env', arg_list)

  # Add non-parameter specified defaults into the environment.
  evalq({
    sort <- 'ascending' # Default sorting to ascending
    sort_var <- target_var # Sort by the target variable itself
    formatter <- as.character # default format of just the function `as.character`
    layers <- structure(list(), class=append("tplyr_layer_container", "list"))
  }, envir = e)

  # Create the object
  structure(e,
            class=append('tplyr_layer', class(e)),
            type=type)
}

#' Validate a tplyr layer
#'
#' @inheritParams tplyr_layer
#' @noRd
validate_tplyr_layer <- function(parent, type, by, target_var, where, ...) {
  dmessage('--- validate_tplyr_layer')


  # Make sure type is valid
  assert_that(!is.null(type) && length(type) == 1 && type %in% c('count', 'desc', 'shift'),
              msg = '`type` must be one of "count", "desc", or "shift"')

  # Check that the parent environment is valid
  assert_that(is.environment(parent) && inherits(parent, c('tplyr_table', 'tplyr_layer', 'tplyr_subgroup_layer')),
              msg="Parent environment must be a `tplyr_table` or `tplyr_layer")

  # Check that the quosures are properly quosures
  assert_that(is_quosure(target_var))
  assert_that(is_quosure(where))

  # Check that the quosures are proper types
  assert_that(class(quo_get_expr(target_var)) == 'name',
              msg = "The `target_var` parameter must refer to a valid variable name (enter without quotes)")

  # Where is not a required field - either must be null or a call. Converted to quosure regardless
  assert_that(is.null(quo_get_expr(where)) || class(quo_get_expr(where)) == 'call',
              msg = "The `where` parameter must contain subsetting logic (enter without quotes)")

  # Make sure `target_var` exists in the target data.frame
  target <- NULL # Mask global definitions check
  vname <- as.character(quo_get_expr(target_var))
  vnames <- evalq(names(target), envir=parent)
  assert_that(vname %in% vnames,
              msg = paste('`target_var` value', vname, 'does not exist in target data frame.'))

  dmessage(paste("By came in as: ",class(by)))

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
      if (class(quo_get_expr(v)) == "name") {
        vname <- as.character(quo_get_expr(v))
        assert_that(vname %in% vnames,
                    msg = paste0("By variable `",vname, "` does not exist in target dataset"))
      }
      # While looping, making sure calls weren't submitted
      if (class(quo_get_expr(v)) == "call") {
        abort("Arguments to `by` must be names or character strings - cannot be calls (i.e. x + y, list(a, b c)).")
      }
      else if (!class(quo_get_expr(v)) %in% c('name', 'character')) {
        abort("Invalid input to `by`. Submit either a string, a variable name, or multiple variable names using `dplyr::vars`.")
      }
    }
  }
}















