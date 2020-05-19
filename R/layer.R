### Layer Constructor

layer <- function(parent, type=NULL, by=NULL, target_var=NULL, where=NULL) {
  # Return a null object of class rtf_doc if no table is passed.
  if(missing(table)) return(structure(logical(0), class = "layer"))
  # If necessary variables provided then build the layer
  as_layer()
}

## Method dispatch
#' Create an Rich Text Format table document
#'
#' @inheritParams layer
#' @noRd
as_layer <- function() {
  UseMethod("as_layer")
}

## For rtf_doc, return the table
as_layer.layer <- function(parent, type=NULL, by=NULL, target_var=NULL, where=NULL) {
  layer <- as_layer(parent, type, by, target_var, where)
  class(layer) <- append(class(layer), 'subgroup_layer')
}

## Unsupported table
as_rtf_doc.tplyr_table <- function(parent, type=NULL, by=NULL, target_var=NULL, where=NULL) {
  as_layer(parent, type, by, target_var, where)
}


## Unsupported table
as_rtf_doc.default <- function(parent, type=NULL, by=NULL, target_var=NULL, where=NULL) {
  stop('Must provide `table` object from the `tplyr` package.')
}



#' Create an Rich Text Format table document
#'
#' @param table A table of a supported class
#' @param titles A list of \code{hf_line} objects.
#' @param footnotes An object/list of \code{hf_line}
#'
#' @return A list with a table, titels, and footnotes component. Class of "rtf_doc"
#'
#' @noRd
#' @importFrom rlang env enquos
new_layer <- function(parent, type=NULL, by=NULL, target_var=NULL, where=NULL) {

  # Convert variables that need to be quosures
  quo_vars <- as.list(enquos(where=where, target_var=target_var))

  # Pull out the arguments from the function call that aren't quosures (and exclude parent)
  arg <- as.list(match.call())[-c(1,2)]
  arg <- arg[!(names(arg) %in% c('where', 'target_var'))]

  # Build the argument list for the environment creation
  arg_list <- append(quo_vars, arg)

  # Add parent back in as named parameter
  arg_list$parent <- parent

  # Create the new environment
  e <- do.call('env', arg_list)

  # Add non-parameter specified defaults into the environment.
  evalq({
    denom <- 'default' # Default denominators
    sort <- 'ascending' # Default sorting to ascending
    sort_var <- target_var # Sort by the target variable itself
    formatter <- as.character # default format of just the function `as.character`
  }, envir = e)

  # Create the object
  structure(e, class=append(class(e), 'layer'))
}

#' Validate parameters passed to layer
#'
#' @param table A table of a supported class
#' @param titles An object/list of /code{hf_line}
#' @param footnotes An object/list of /code{hf_line}
#'
#' @return nothing for now
#' @noRd
validate_layer <- function(arg_list) {

  # Bring the list variables into the caller environment to evaluate
  list2env(arg_list, environment())

  # Check that the parent environment is valid
  assert_that(is.environment(parent) && inherits(parent, c('tplyr_table', 'layer')),
              msg="Parent environment must be a `tplyr_table` or `layer")

  # Check that the by group variable is character
  assert-that(is.character(by) && length(by) >= 1,
              msg = "`by` must be a character vector with 1 or more elements")

  # Check that the quosures are properly quosures
  assert_that(is_quosure(target_var))
  assert_that(is_quosure(where))

  # Check that the quosures are proper types
  assert_that(class(quo_get_expr(target_var)) == 'name',
              msg = "The `target_var` parameter must refer to a valid variable name")

  assert_that(class(quo_get_expr(where)) == 'call',
              msg = "The `where` parameter must contain subsetting logic")

  }
