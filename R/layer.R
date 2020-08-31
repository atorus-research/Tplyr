### Layer Constructor


#' Create a \code{tplyr_layer} object
#'
#' @description This object is the workhorse of the \code{tplyr} package. A
#' \code{tplyr_layer} can be thought of as a block, or "layer" of a table.
#' Summary tables typically consist of different sections that require different
#' summaries. When programming these section, your code will create different
#' layers that need to be stacked or merged together. A \code{tplyr_layer} is
#' the container for those isolated building blocks.
#'
#' When building the \code{tplyr_table}, each layer will execute independently.
#' When all of the data processing has completed, the layers are brought
#' together to construct the output.
#'
#' \code{tplyr_layer} objects are not created directly, but are rather created
#' using the layer constructor functions \code{\link{group_count}},
#' \code{\link{group_desc}}, and \code{\link{group_shift}}.
#'
#' @param parent \code{tplyr_table} or \code{tplyr_layer}. Required. The parent
#'   environment of the layer. This must be either the \code{tplyr_table} object
#'   that the layer is contained within, or another \code{tplyr_layer} object of
#'   which the layer is a subgroup.
#' @param type "count", "desc", or "shift". Required. The category of layer -
#'   either "counts" for categorical counts, "desc" for descriptive statistics,
#'   or "shift" for shift table counts
#' @param by A string, a variable name, or a list of variable names supplied
#'   using \code{dplyr::vars}
#' @param target_var Symbol. Required, The variable name on which the summary is
#'   to be performed. Must be a variable within the target dataset. Enter
#'   unquoted - i.e. target_var = AEBODSYS.
#' @param where Call. Filter logic used to subset the target data when
#'   performing a summary.
#' @param ... Additional arguments
#'
#' @return A \code{tplyr_layer} environment that is a child of the specified
#'   parent. The environment contains the object as listed below.
#'
#' @section \code{tplyr_layer} Core Object Structure: \describe{
#'   \item{\code{type}}{This is an attribute. A string indicating the layer
#'   type, which controls the summary that will be performed.}
#'   \item{\code{target_var}}{A quosure of a name, which is the variable on
#'   which a summary will be performed.} \item{\code{by}}{A list of quosures
#'   representing either text labels or variable names used in grouping.
#'   Variable names must exist within the target dataset Text strings submitted
#'   do not need to exist in the target dataset.} \item{\code{cols}}{A list of
#'   quosures used to determine the variables that are used to display in
#'   columns.} \item{\code{where}}{A quosure of a call that containers the
#'   filter logic used to subset the target dataset. This filtering is in
#'   addition to any subsetting done based on \code{where} criteria specified in
#'   \code{\link{tplyr_table}}} \item{\code{layers}}{A list with class
#'   \code{tplyr_layer_container}. Initialized as empty, but serves as the
#'   container for any sublayers of the current layer. Used internally.} }
#'
#'   Different layer types will have some different bindings specific to that
#'   layer's needs.
#'
#' @family Layer construction functions
#'
#' @export
#'
#' @examples
#' tab <- tplyr_table(iris, Sepal.Width)
#'
#' l <- group_count(tab, by=vars('Label Text', Species),
#'                  target_var=Species, where= Sepal.Width < 5.5,
#'                  cols = Species)
#'
#'
#' @seealso \link{tplyr_table}
tplyr_layer <- function(parent, target_var, by, where, type, ...) {

  # Return a null object if the parent is missing
  if(missing(parent)) abort("The `parent` argument must be provided.")

  # If necessary variables provided then build the layer
  as_tplyr_layer(parent, type=type, by=by, target_var=target_var, where=where, ...)
}

# Method dispatch
#' Create a tplyr layer
#'
#' @inheritParams tplyr_layer
#' @noRd
as_tplyr_layer <- function(parent, target_var, by, where, type, ...) {
  UseMethod("as_tplyr_layer")
}

#' S3 method for tplyr layer creation of \code{tplyr_table} object as parent
#' @noRd
as_tplyr_layer.tplyr_table <- function(parent, target_var, by, where, type, ...) {
  new_tplyr_layer(parent, target_var, by, where, type, ...)
}

#' S3 method for tplyr layer creation of \code{tplyr_layer}  object as parent
#' @noRd
as_tplyr_layer.tplyr_layer <- function(parent, target_var, by, where, type, ...) {
  layer <- new_tplyr_layer(parent, target_var, by, where, type, ...)
  class(layer) <- append('tplyr_subgroup_layer', class(layer))
  layer
}

#' S3 method for tplyr layer creation of \code{tplyr_subgroup_layer}  object as parent
#' @noRd
as_tplyr_layer.tplyr_subgroup_layer <- function(parent, target_var, by, where, type, ...) {
  layer <- new_tplyr_layer(parent, target_var, by, where, type, ...)
  class(layer) <- unique(append('tplyr_subgroup_layer', class(layer)))
  layer
}

#' S3 method to produce error for unsupported objects as parent
#' @noRd
as_tplyr_layer.default <- function(parent, target_var, by, where, type, ...) {
  stop('Must provide `tplyr_table`, `tplyr_layer`, or `tplyr_subgroup_layer` object from the `tplyr` package.')
}

#' Create a new tplyr layer
#'
#' @inheritParams tplyr_layer
#' @noRd
new_tplyr_layer <- function(parent, target_var, by, where, type, ...) {

  # Pull out the arguments from the function call that aren't quosures (and exclude parent)
  # Specifically excluding the function call, parent, and type
  arg_list <- as.list(match.call())[-c(1, 2, 6, 7)]

  # Insert parent to the front of the list to prepare the call
  arg_list <- append(arg_list, parent, after=0)

  # Unpack the `by` group to ensure that the type is `list_of<quosures>`
  # It had to be a 1 item list, so check if that element is a `call`
  # The only valid use of a `call` is to provide multiple variables using `vars`
  by <- unpack_vars(by)
  arg_list$by <- by

  # Do the same for target_var
  target_var <- unpack_vars(target_var)
  arg_list$target_var <- target_var

  # Run validation
  validate_tplyr_layer(parent, target_var, by, cols, where, type)

  # Create the new environment by contructing the env call
  e <- do.call('env', arg_list)

  # Add non-parameter specified defaults into the environment.
  evalq({
    layers <- structure(list(), class=append("tplyr_layer_container", "list"))
    precision_by <- by
    precision_on <- target_var[[1]]
    stats <- list()
  }, envir = e)

  # Create the object
  structure(e,
            class=append(c('tplyr_layer', paste0(type,'_layer')), class(e))) %>%
    set_where(!!where)
}

#' Validate a tplyr layer
#'
#' @inheritParams tplyr_layer
#' @noRd
validate_tplyr_layer <- function(parent, target_var, by, cols, where, type, ...) {

  # Make sure type is valid
  assert_that(!is.null(type) && length(type) == 1 && type %in% c('count', 'desc', 'shift'),
              msg = '`type` must be one of "count", "desc", or "shift"')

  # Check that the parent environment is valid
  assert_that(is.environment(parent) && inherits(parent, c('tplyr_table', 'tplyr_layer', 'tplyr_subgroup_layer')),
              msg="Parent environment must be a `tplyr_table` or `tplyr_layer")

  # Make sure `target_var` exists in the target data.frame
  target <- NULL # Mask global definitions check
  vnames <- evalq(names(target), envir=parent)

  # Make sure that by variables not submitted as characters exist in the target dataframe
  assert_quo_var_present(by, vnames)
  # Do the same for target_var
  assert_quo_var_present(target_var, vnames)

  # For desc layers additionally make sure that the target variables all are numeric
  if (type == "desc") {
    walk(target_var, ~ assert_that(is.numeric(evalq(target, envir=parent)[[as_name(.x)]]),
                                   msg = paste0("Target variable `", as_name(.x), "` is not numeric. ",
                                                "Target variables must be numeric for desc layers.")))
  }
}
