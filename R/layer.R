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
#' @param by Character Vector. Required. Variable to group the summary being performed, or label of the group to be provided
#'   in the stub. Can be a combination of both.
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
#' @itemize{
#' \item{type - A string indicating the layer type, which controls the summary that will be performed.}
#' \item{target_var - A quosure of a name, which is the variable on which a summary will be performed.}
#' \item{by - A character vector of either a character string label that will be used in the stub of the output table,
#'   or variable names that will be used when created by groups for the summary.}
#' \item{where - A quosure of a call that containers the filter logic used to subset the target dataset.}
#' \item{sort_vars - A character vector containingn the variables that will be used to sort the results of the summary.
#'   Set by default to the value of \code{target_var}}
#' \item{sort - A string containing the sort method. Defaults to 'asc' for ascending.}
#' \item{formatter - A function used to create the string formats for the resulting numbers in output presentation.}
#' }
#' @export
#'
#' @examples
#' tab <- env(test_data = c(1,2,3,4))
#' class(tab) <- append('tplyr_table', class(tab))
#'
#' l <- tplyr_layer(tab, type='count', by="AEBODSYS", target_var=AEDECOD, where= AESER == "Y")
#'
#' @seealso \code{\link{tplyr_table}}
tplyr_layer <- function(parent, type, by, target_var, where, ...) {

  # Return a null object if the parent is missing -
  if(missing(parent)) return(structure(empty_env(), class = "tplyr_layer"))

  # If necessary variables provided then build the layer
  as_tplyr_layer(parent, type=type, by=by, target_var=enquo(target_var), where=enquo(where))
}

# Method dispatch
#' Create a tplyr layer
#'
#' @inheritParams tplyr_layer
#' @noRd
as_tplyr_layer <- function(parent, type, by, target_var, where, ...) {
  UseMethod("as_tplyr_layer")
}

## tplyr layer
as_tplyr_layer.tplyr_layer <- function(parent, type, by, target_var, where, ...) {
  dmessage('Dispatch tplyr_layer')
  layer <- new_tplyr_layer(parent, type, by, target_var, where)
  class(layer) <- append(class(layer), 'tplyr_subgroup_layer')
}

## tplyr table
as_tplyr_layer.tplyr_table <- function(parent, type, by, target_var, where, ...) {
  dmessage('Dispatch tplyr_table')
  new_tplyr_layer(parent, type, by, target_var, where)
}

## Unsupported object
as_tplyr_layer.default <- function(parent, type, by, target_var, where, ...) {
  dmessage('Dispatch default')
  stop('Must provide `table` object from the `tplyr` package.')
}

# New layer
#' Create a tplyr layer
#'
#' @inheritParams tplyr_layer
#' @noRd
new_tplyr_layer <- function(parent, type, by, target_var, where, ...) {
  dmessage('--- new_tplyr_layer')

  # Pull out the arguments from the function call that aren't quosures (and exclude parent)
  arg_list <- as.list(match.call())[-c(1,2)]

  # Run validation
  validate_tplyr_layer(parent, type, by, target_var, where)

  # Add parent back in as the first argument
  arg_list <- append(arg_list, parent, after=0)

  # Create the new environment by contructing the env call
  e <- do.call('env', arg_list)

  # Add non-parameter specified defaults into the environment.
  evalq({
    sort <- 'ascending' # Default sorting to ascending
    sort_var <- as.character(quo_get_expr(target_var)) # Sort by the target variable itself
    formatter <- as.character # default format of just the function `as.character`
  }, envir = e)

  # Create the object
  structure(e, class=append('tplyr_layer', class(e)))
}

# Validate layer
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
  assert_that(is.environment(parent) && inherits(parent, c('tplyr_table', 'tplyr_layer')),
              msg="Parent environment must be a `tplyr_table` or `tplyr_layer")

  # Check that the by group variable is character
  assert_that(is.character(by) && length(by) >= 1,
              msg = "`by` must be a character vector with 1 or more elements")

  # Check that the quosures are properly quosures
  assert_that(is_quosure(target_var))
  assert_that(is_quosure(where))

  # Check that the quosures are proper types
  assert_that(class(quo_get_expr(target_var)) == 'name',
              msg = "The `target_var` parameter must refer to a valid variable name")

  # Where is not a required field - either must be null or a call. Converted to quosure regardless
  assert_that(is.null(quo_get_expr(where)) || class(quo_get_expr(where)) == 'call',
              msg = "The `where` parameter must contain subsetting logic")

  }
