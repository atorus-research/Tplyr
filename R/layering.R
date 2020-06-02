### Layering Interfaces

#' Attach a layer to a \code{tplyr_table} object
#'
#' @description
#' \code{add_layer} inserts a \code{tplyr_layer} into the \code{layers} container of a \code{tplyr_table} object. This allows
#' for a tidy style of programming (using \code{magrittr} piping - \code{\%>\%}) with a secondary advantage - the construction
#' of the layer object may consist of a series of piped functions itself.
#'
#' \code{tplyr} encourages a user to view the construction of a table as a series of "layers". The construction of each of these
#' layers are isolated and independent of one another - but each of these layers are children of the table itself. \code{add_layer}
#' isolates the construction of an individual layer and allows the user to construct that layer and insert it back into the parent.
#' The syntax for this is intuitive and allows for tidy piping. Simply pipe the current table object in, and write the code to
#' construct your layer within the \code{layer} paramater.
#'
#' Sometimes layers may contain subgroups. For example, a total count row can be viewed as a separate layer with sub-group counts
#' intermingled between each total group (for example, within adverse event tables). \code{add_layer} and the \code{tplyr_layer}
#' objects handle this by creating \code{tplyr_subgroup_layer} objects. This is as simple as using \code{add_layer} within your current
#' \code{add_layer} call, thus creating a subgroup layer. \code{tplyr} understands that the subgroup is a child on the initial group
#' and handles it appropriately, from which you can customize with modifier functions.
#'
#' @param parent A \code{tplyr_table} or \code{tplyr_layer}/\code{tplyr_subgroup_layer} object
#' @param layer A layer construction function and associated modifier functionns
#'
#' @return A \code{tplyr_table} or \code{tplyr_layer}/\code{tplyr_subgroup_layer} with a new layer inserted into the \code{layer}
#'   binding
#'
#' @seealso [tplyr_table(), tplyr_layer(), group_count(), group_desc(), group_shift()]
#'
#' @export
#'
#' @examples
#' ## Single layer
#' t <- tplyr_table(iris, Sepal.Width) %>%
#'   add_layer(
#'     group_desc(target_var=Species)
#'   )
#'
#' # Layer with sub layer
#' t <- tplyr_table(iris, Sepal.Width) %>%
#'   add_layer(
#'     group_desc(target_var=Species) %>%
#'       add_layer(
#'         group_count(target_var=Sepal.Width)
#'       )
#'   )
add_layer <- function(parent, layer) {

  assert_that(!missing(parent), msg = "`parent` parameter must be provided")
  assert_that(!missing(layer), msg = "`layer` parameter must be provided")

  # Capture the layer code as a quosure
  layer <- enquo(layer)
  dmessage(layer)

  # Insert the `parent` argument into the topmost call of the layer code
  # (i.e. if any pipes %>% then pull out the left most call and modify it)
  l <- modify_nested_call(layer, parent=parent)
  dmessage(paste('Modified call:', as_label(l)))

  # Evaluate the layer and grab `tplyr_layer` or `tplyr_subgroup_layer` object
  executed_layer <- eval(quo_get_expr(l))

  # Insert the layer into the parent object
  parent$layers <- append(parent$layers, executed_layer)
  parent
}

#' Create a \code{count}, \code{desc}, or \code{shift} layer for discrete count based summaries, descriptive statistics summaries,
#'  or shift count summaries
#'
#' @description
#' This family of functions specifies the type of summary that is to be performed within a layer. \code{count} layers are used
#' to create summary counts of some discrete variable. \code{desc} layer create summary statistics, and \code{shift} layers
#' summaries the counts of different changes in states. See the "details" section below for more information.
#'
#' @param parent \code{tplyr_table} or \code{tplyr_layer}. Required. The parent environment of the layer. This must be either the
#'   \code{tplyr_table} object that the layer is contained within, or another \code{tplyr_layer} object of which
#'   the layer is a subgroup.
#' @param by A string, a variable name, or a list of variable names supplied using \code{dplyr::vars}
#' @param target_var Symbol. Required, The variable name on which the summary is to be performed. Must be a variable within
#'   the target dataset. Enter unquoted - i.e. target_var = AEBODSYS.
#' @param where Call. Filter logic used to subset the target data when performing a summary.
#' @param ... Additional arguments that will be passed directly into the \code{tplyr_layer} environment. See the
#'   \href{<link tbd>}{vignette} on adding extensions.
#'
#' @details
#' #TODO: Complete this section with more information about each layer type.
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
#'
#' @return A \code{tplyr_layer} object
#'
#' @family Layer construction functions
#'
#' @rdname layer_constructors
#'
#' @seealso [add_layer(), tplyr_table, tplyr_layer()]
#'
#' @export
#'
#' @examples
#' t <- tplyr_table(iris, Sepal.Width) %>%
#'   add_layer(
#'     group_desc(target_var=Species)
#'   )
group_count <- function(parent, ...) {
  tplyr_layer(parent, type='count', ...)
}

#' @rdname layer_constructors
#' @export
group_desc <- function(parent, ...) {
  tplyr_layer(parent, type='desc', ...)
}

#' @rdname layer_constructors
#' @export
group_shift <- function(parent, ...) {
  tplyr_layer(parent, type='shift', ...)
}
